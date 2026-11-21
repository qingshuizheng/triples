;;; triples-fts.el --- Sqlite full text search for triples. -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/triples
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides full text search for triples.  It uses sqlite's FTS
;; capabilities.  It indexes all text objects.

;; This only will work with the built-in sqlite support in Emacs 29.1 or later.

;;; Code:

(require 'triples)
(require 'sqlite)
(require 'seq)

(defgroup triples-fts nil
  "Full text search for triples using SQLite FTS5."
  :group 'triples)

(defcustom triples-fts-tokenizer nil
  "Tokenizer passed to the FTS5 virtual table, or nil for the
SQLite default (unicode61).

To enable Chinese full-text search, set this to \"simple\" and
`triples-fts-extension-path' to the wangfenjin/simple shared
library.  After changing the tokenizer you must rebuild the FTS
index with `(triples-fts-setup DB t)'."
  :type '(choice (const :tag "SQLite default (unicode61)" nil) string)
  :group 'triples-fts)

(defcustom triples-fts-extension-path nil
  "Shared library to load before creating the FTS table.

Passed to `sqlite-load-extension'.  Needed when the tokenizer
ships as a loadable SQLite extension (e.g. wangfenjin/simple).
Loading is idempotent: reloading an already-loaded extension
returns nil without error."
  :type '(choice (const :tag "None" nil) string)
  :group 'triples-fts)

(defcustom triples-fts-term-expander nil
  "Function used to rewrite single query terms before FTS5 sees them.

When nil, every term is handed to FTS5 as a bound parameter and is
interpreted with FTS5's own query syntax.

When set to a function, it is called with one argument, a term from
the user's query, and should return either

  nil     to leave TERM alone, so it is passed as a bound
          parameter, or
  STRING  to use STRING as the SQL expression matching TERM,
          with TERM still bound to the \"?\" inside it.

This exists because a tokenizer extension may expect a query
syntax that plain FTS5 parsing cannot produce.  With the
wangfenjin/simple extension, returning \"simple_query(?)\" makes
pinyin work: \"zhongwen\" then matches notes containing 中文.

Prefer `simple_query' over `jieba_query' there.  Both expand
pinyin identically, but `jieba_query' needs the external cppjieba
dictionary loaded with `jieba_dict', and calls abort() -- killing
the whole Emacs process -- when that dictionary is missing.
`simple_query' only needs the pinyin data compiled into the
extension.

Terms that `triples-fts--transform-query' rewrites into
`predicate:\"...\" object:\"...\"' pairs are never passed to the
expander, because rewriting them would destroy that syntax."
  :type '(choice (const :tag "None" nil) function)
  :group 'triples-fts)

;;; Pinyin data
;;
;; `triples-pinyin.txt' holds 20924 common characters, one "character<TAB>pinyin"
;; pair per line, derived from mozillazg/pinyin-data via the dictionary that the
;; wangfenjin/simple tokenizer ships.  The sqlite tokenizer expands pinyin inside
;; SQLite; for the pg interface the same expansion has to be computed in Lisp.

(defvar triples-fts--pinyin-table-cache nil
  "Hash table mapping a character to its pinyin, loaded on first use.")

(defun triples-fts--pinyin-data-file ()
  "Return the path of the pinyin data file."
  (expand-file-name "triples-pinyin.txt"
                    (file-name-directory (locate-library "triples"))))

(defun triples-fts--pinyin-table ()
  "Return the character to pinyin table, loading it on first use."
  (or triples-fts--pinyin-table-cache
      (setq triples-fts--pinyin-table-cache
            (let ((table (make-hash-table :test #'equal))
                  (file (triples-fts--pinyin-data-file)))
              (when (file-readable-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (re-search-forward "^\\(.\\)\t\\([a-z]+\\)$" nil t)
                    (puthash (match-string 1) (match-string 2) table))))
              table))))

(defun triples-fts-pinyin (text)
  "Return TEXT with the pinyin of every Chinese character appended.
This is meant to be the value of `triples-fts-pinyin-function': the sqlite
tokenizer builds the same expansion into the FTS index, and PostgreSQL has no
tokenizer, so it has to be stored instead.

The syllables are joined without separators, so that a query for `biede' finds
`bie de' written across two characters: the stored form is searched with a
substring match, and a space in it would break exactly that query."
  (let ((table (triples-fts--pinyin-table)))
    (mapconcat (lambda (ch)
                 (let ((py (gethash (char-to-string ch) table)))
                   (if py py (string ch))))
               text "")))

(defun triples-fts-setup (db &optional force)
  "Ensure DB has an FTS table, where the interface has one.
PostgreSQL has no FTS5 table, so there is nothing to set up there; the query
side is still sqlite-only for now.  FORCE rebuilds, as in the sqlite case."
  (unless (eq triples-database-interface 'pg)
    (triples-fts--sqlite-setup db force)))

(defun triples-fts--sqlite-setup (db force)
  "Ensure DB has a FTS table.
As long as the FTS table exists, this will not try to recreate
it.  If FORCE is non-nil, then the FTS and all triggers will be
recreated and repopulated."
  (unless (eq triples-database-interface 'builtin)
    (error "triples-fts requires the `builtin' interface, that is, the sqlite support built into Emacs 29.1 or later; `triples-database-interface' is currently `%s'"
           triples-database-interface))
  (when triples-fts-extension-path
    (sqlite-load-extension db triples-fts-extension-path))
  (let ((fts-existed (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table' AND name='triples_fts'"))
        ;; Detect the old buggy delete trigger (missing rowid column).
        (had-buggy-triggers
         (sqlite-select db "SELECT 1 FROM sqlite_master WHERE name='triples_fts_delete' AND sql NOT LIKE '%rowid%'")))
    (when (and force fts-existed) (sqlite-execute db "DROP TABLE triples_fts"))
    (sqlite-execute db (format "CREATE VIRTUAL TABLE IF NOT EXISTS triples_fts USING fts5 (subject, predicate, object, content=triples, content_rowid=rowid%s)"
                               (if triples-fts-tokenizer
                                   (format ", tokenize = '%s'" triples-fts-tokenizer)
                                 "")))
    ;; Triggers that will update triples_fts, but only for text objects.
    ;;
    ;; Always drop and recreate rather than using IF NOT EXISTS,
    ;; because earlier versions had bugs (missing rowid in the delete
    ;; trigger, single update trigger that missed text→non-text
    ;; transitions).
    ;;
    ;; New rows:
    (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_insert")
    (sqlite-execute db "CREATE TRIGGER triples_fts_insert AFTER INSERT ON triples
      WHEN new.object IS NOT NULL and typeof(new.object) = 'text'
      BEGIN
        INSERT INTO triples_fts (rowid, subject, predicate, object) VALUES (new.rowid, new.subject, new.predicate, new.object);
      END")
    ;; Updated rows: split into two triggers so we correctly handle
    ;; text→text, text→non-text, and non-text→text transitions.
    (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_update")
    (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_update_delete")
    (sqlite-execute db "CREATE TRIGGER triples_fts_update_delete AFTER UPDATE ON triples
        WHEN old.object IS NOT NULL AND typeof(old.object) = 'text'
        BEGIN
          INSERT INTO triples_fts (triples_fts, rowid, subject, predicate, object) VALUES ('delete', old.rowid, old.subject, old.predicate, old.object);
        END")
    (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_update_insert")
    (sqlite-execute db "CREATE TRIGGER triples_fts_update_insert AFTER UPDATE ON triples
        WHEN new.object IS NOT NULL AND typeof(new.object) = 'text'
        BEGIN
          INSERT INTO triples_fts (rowid, subject, predicate, object) VALUES (new.rowid, new.subject, new.predicate, new.object);
        END")
    ;; Deleted rows:
    (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_delete")
    (sqlite-execute db "CREATE TRIGGER triples_fts_delete AFTER DELETE ON triples
      WHEN old.object IS NOT NULL AND typeof(old.object) = 'text'
      BEGIN
        INSERT INTO triples_fts (triples_fts, rowid, subject, predicate, object) VALUES ('delete', old.rowid, old.subject, old.predicate, old.object);
      END")
    (when (or force (not fts-existed) had-buggy-triggers)
      (triples-fts-rebuild db))))

(defun triples-fts-rebuild (db)
  "Rebuild the FTS table for DB."
  (sqlite-execute db "INSERT INTO triples_fts (triples_fts) VALUES ('rebuild')"))

(defun triples-fts--split-query (query)
  "Return the QUERY split by whitespace, except for quoted strings."
  ;; First, we remove all quoted strings via regexes.
  (let ((quoted-strings '())
        (quoted-strings-re (rx (seq "\"" (group (zero-or-more (not (any "\"")))) "\"")))
        (query-copy (replace-regexp-in-string (rx (seq ?: (zero-or-more space))) ":" query)))
    (while (string-match quoted-strings-re query-copy)
      (push (match-string 1 query-copy) quoted-strings)
      (setq query-copy (replace-match "" t t query-copy)))
    ;; Now we split by whitespace, except for quoted strings.
    (append (split-string query-copy) quoted-strings)))

(defun triples-fts--transform-query (query abbrevs)
  "Rewrite abbreviations in QUERY based on `triples-fts-predicate-abbrevs`.

This returns a list of new queries.  Because each triple is a row, we
have each part of the query matching separately, and then we do an
intersection on the results.

Each element is a cons of the query text and a flag saying whether
`triples-fts-term-expander' is allowed to rewrite it.  Terms turned
into predicate/object pairs must not be rewritten, because the
expansion would destroy that syntax.

Because predicates that we need to match against are

E.g. if `tag' is an abbreviation for `tagged/tag', from the alist
ABBREVS, then: \"tag:foo urgent\" ==> \"predicate:\"tagged/tag\"
object:\"foo\" urgent\"."
  ;; Split by whitespace, except for quoted strings.
  (let ((segments (triples-fts--split-query query)))
    (mapcar
     (lambda (w)
       (if (string-match "^\\([^:]+\\):\\(.*\\)$" w)
           (let* ((prefix (match-string 1 w))
                  (rest   (match-string 2 w))
                  (full   (assoc-default prefix abbrevs)))
             (if (or full (string-match-p "/" prefix))
                 ;; Example: "tag" => "tagged/tag", rest => "foo"
                 (cons (format "predicate:\"%s\" object:\"%s\"" (or full prefix) rest) nil)
               (cons w t)))  ; No known abbreviation; just leave as-is.
         (cons w t)))
     segments)))

(defun triples-fts--term-sql (term)
  "Return the SQL expression matching TERM inside a MATCH clause.

This is the bound parameter \"?\" unless `triples-fts-term-expander'
rewrites TERM into something else."
  (or (and triples-fts-term-expander
           (funcall triples-fts-term-expander term))
      "?"))

(defun triples-fts--pg-sql-quote (text)
  "Return TEXT ready to sit inside a single-quoted SQL string."
  (replace-regexp-in-string "'" "''" text))

(defun triples-fts--pg-term-sql (db term flag)
  "Return a statement selecting subjects matching TERM on PostgreSQL.
FLAG mirrors `triples-fts--transform-query': a nil FLAG means TERM already has
the `predicate:\"P\" object:\"O\"' syntax, otherwise it is a plain substring
search.  `triples-fts-term-expander' is not consulted: it produces FTS5 syntax."
  (if (and (not flag)
           (string-match "predicate:\"\([^\"]*\)\" object:\"\(.*\)\"$" term))
      (concat "SELECT subject FROM triples WHERE object ILIKE '%"
              (triples-fts--pg-sql-quote (match-string 2 term))
              "%' AND predicate = '"
              (triples-fts--pg-sql-quote (match-string 1 term))
              "'")
    (progn
      ;; The pinyin table only exists once something has been stored there; a
      ;; plain CREATE IF NOT EXISTS keeps the query self-contained.
      (emacsql db "CREATE TABLE IF NOT EXISTS triples_pinyin (subject text, pinyin text)")
      (let ((needle (triples-fts--pg-sql-quote
                     (replace-regexp-in-string "[%_]" "" term))))
        (concat "SELECT subject FROM triples WHERE object ILIKE '%" needle "%'"
                " OR subject IN (SELECT subject FROM triples_pinyin"
                " WHERE pinyin ILIKE '%" needle "%')")))))

(defun triples-fts--pg-query-subject (db query &optional abbrevs)
  "LIKE-based fallback for `triples-fts-query-subject' on PostgreSQL.
There is no FTS5 table there, and PostgreSQL's default parser does not split
Chinese, so each transformed term becomes a substring scan over `object',
intersected with the others.  Slower than FTS5 and it carries none of the
tokenizer's pinyin expansion, but search works, and a `pg_trgm' index on
`object' keeps it fast."
  (let ((per-term
         (mapcar
          (lambda (subquery)
            (mapcar #'car
                    (emacsql db
                             (replace-regexp-in-string
                              "%" "%%"
                              (triples-fts--pg-term-sql db (car subquery) (cdr subquery))))))
          (triples-fts--transform-query query abbrevs))))
    ;; No `triples-standardize-result' here: unlike `sqlite-select', emacsql has
    ;; already read the values back into Lisp objects.
    (seq-uniq (cl-reduce #'seq-intersection per-term))))

(defun triples-fts-pg-backfill-pinyin (&optional function)
  "Refresh the pinyin index for every text row of the current pg database.
FUNCTION defaults to `triples-fts-pinyin-function'.  Notes written before the
pinyin table existed, or while it was disabled, get their entry from here.
Returns the number of rows refreshed."
  (unless (eq triples-database-interface 'pg)
    (user-error "Only the pg interface keeps a pinyin table"))
  (let ((function (or function triples-fts-pinyin-function)))
    (unless function
      (user-error "Set `triples-fts-pinyin-function' first"))
    (let ((db (triples-connect))
          (n 0))
      (dolist (row (emacsql db "SELECT subject, object FROM triples WHERE predicate = 'text/text'"))
        (when (stringp (nth 1 row))
          (triples-pg-store-pinyin db (nth 0 row) (nth 1 row))
          (setq n (1+ n))))
      n)))

(defun triples-fts-query-subject (db query &optional abbrevs)
  "Query DB with QUERY, returning only subjects.

QUERY should not have operators such as AND or OR, everything is assumed
to be ANDed together.  Phrases can be in quotes.

Predicates can appear before colons to restrict a query term.  For
example, `person/name:Billy'.  Anything with a slash in it, or matching
an entry in ABBREV will be used to filter by a predicate, otherwise it
is passed to FTS5 as-is.

Terms that are not restricted to a predicate are additionally passed
through `triples-fts-term-expander', if it is set.

ABBREVS is an alist of abbreviations to predicate (both strings).  If
this is populated then we also expand user abbreviations like `tag:xyz`
=> `predicate:\"tagged/tag\" object:\"xyz\"`."
  (if (eq triples-database-interface 'pg)
      (triples-fts--pg-query-subject db query abbrevs)
    (seq-uniq
   (mapcar
    #'triples-standardize-result
    (cl-reduce #'seq-intersection
               (mapcar
                (lambda (subquery)
                  (mapcar #'car
                          (sqlite-select
                           db
                           (format "SELECT subject FROM triples_fts
         WHERE triples_fts MATCH %s
         ORDER BY rank"
                                   (if (cdr subquery)
                                       (triples-fts--term-sql (car subquery))
                                     "?"))
                           (list (car subquery)))))
                (triples-fts--transform-query query abbrevs)))))))

(provide 'triples-fts)

;;; triples-fts.el ends here
