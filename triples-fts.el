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

(defun triples-fts-setup (db &optional force)
  "Ensure DB has a FTS table.
As long as the FTS table exists, this will not try to recreate
it.  If FORCE is non-nil, then the FTS and all triggers will be
recreated and repopulated."
  (unless (eq triples-sqlite-interface 'builtin)
    (error "Emacs 29.1 or later is required for triples-fts"))
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
                (triples-fts--transform-query query abbrevs))))))

(provide 'triples-fts)

;;; triples-fts.el ends here
