;;; triples.el --- A flexible triple-based database for use in apps  -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/triples
;; Package-Requires: ((seq "2.0") (emacs "28.1"))
;; Keywords: triples, kg, data, sqlite, postgres
;; Version: 0.7.0
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
;; Triples is a library implementing a data storage based on the idea of
;; triples: subject, predicate, objects, plus some extra metadata.  This data
;; structure provides a way to store data according to an extensible schema, and
;; provide an API offering two-way links between all information stored.
;;
;; This package requires either Emacs 29 or the emacsql package to be installed.
;; The PostgreSQL backend additionally requires the `pg' (pg.el) package.
;; The `emacsql-pg' backend is part of the `emacsql' package.

(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'subr-x)
(require 'emacsql nil t)

;;; Code:

(defvar emacsql-sqlite-executable)
(declare-function emacsql-with-transaction "emacsql")
(declare-function emacsql-close "emacsql")
(declare-function emacsql-sqlite "emacsql")
(declare-function emacsql "emacsql")
(declare-function emacsql-sqlite-open "emacsql")
(declare-function emacsql-pg "emacsql-pg" (dbname user &rest _))

;; The alias has to be declared before the variable it aliases,
;; otherwise the byte compiler warns that it is late.
(define-obsolete-variable-alias 'triples-sqlite-interface
  'triples-database-interface "0.7"
  "Renamed because the variable now also selects the `pg' interface.")

(defvar triples-database-interface
  (if (and (fboundp 'sqlite-available-p) (sqlite-available-p))
      'builtin
    'emacsql)
  "The interface to the database to use.
Either `builtin', `emacsql', or `pg'.

`builtin' uses the sqlite support built into Emacs 29.1 or later.
`emacsql' uses the emacsql package with a SQLite backend.
`pg' uses the emacsql package with a PostgreSQL backend (via
`emacsql-pg'), connecting to the database specified by
`triples-pg-connection-spec'.

Defaults to builtin when available.  Builtin is available when the
version is Emacs 29 or greater, and emacsql is usable when the
`emacsql' package is installed.")

(defconst triples-sqlite-executable "sqlite3"
  "If using Emacs 29 builtin sqlite, this specifices the executable.
It is invoked to make backups.")

(defconst triples-default-database-filename (locate-user-emacs-file "triples.db")
  "The default filename triples database.

If no database is specified, this file is used.")

(defconst triples-basic-types '(integer float string symbol vector cons)
  "Possible values for :base/type for objects.

Everything here must be returnable by `type-of'.")

(defmacro triples-with-transaction (db &rest body)
  "Create a transaction using DB, executing BODY.
The transaction will abort if an error is thrown."
  (declare (indent 0) (debug t))
  `(triples--with-transaction ,db (lambda () ,@body)))

(defun triples-rebuild-builtin-database (db)
  "Rebuild the builtin database DB.
This is used in upgrades and when problems are detected."
  (triples-with-transaction
    db
    (sqlite-execute db "ALTER TABLE triples RENAME TO triples_old")
    (triples-setup-table-for-builtin db)
    (sqlite-execute db "INSERT INTO triples (subject, predicate, object, properties) SELECT DISTINCT subject, predicate, object, properties FROM triples_old")
    (sqlite-execute db "DROP TABLE triples_old")))

(defun triples-maybe-upgrade-to-builtin (db)
  "Check to see if DB needs to be upgraded from emacsql to builtin."
  ;; Check to see if this was previously an emacsql database, and if so,
  ;; change the property column to be standard for builtin sqlite.
  (when (> (caar (sqlite-select db "SELECT COUNT(*) FROM triples WHERE properties = '(:t t)'"))
           0)
    (if (> (caar (sqlite-select db "SELECT COUNT(*) FROM triples WHERE properties = '()'"))
           0)
        (progn
          (message "triples: detected data written with both builtin and emacsql, upgrading and removing duplicates")
          ;; Where we can, let's just upgrade the old data.  However, sometimes we cannot due to duplicates.
          (sqlite-execute db "UPDATE OR IGNORE triples SET properties = '()' WHERE properties = '(:t t)'")
          ;; Remove any duplicates that we cannot upgrade.
          (sqlite-execute db "DELETE FROM triples WHERE properties = '(:t t)'"))
      (message "triples: detected previously used emacsql database, converting to builtin sqlite")
      (sqlite-execute db "UPDATE triples SET properties = '()' WHERE properties = '(:t t)'"))))

(defun triples-connect (&optional file)
  "Connect to the database FILE and make sure it is populated.
If FILE is nil, use `triples-default-database-filename'.

If `triples-database-interface' is `pg', FILE is ignored and the
connection is made according to `triples-pg-connection-spec'."
  (unless (pcase-exhaustive triples-database-interface
            ('builtin
             (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
            ('emacsql (require 'emacsql nil t))
            ('pg (and (require 'emacsql nil t)
                      (require 'emacsql-pg nil t)
                      ;; `emacsql-pg' only soft-requires `pg', so check it
                      ;; here to fail early with a clear message.
                      (require 'pg nil t))))
    (error "triples: the `%s' interface is not usable; `emacsql' is required, the `pg' interface additionally requires `pg', and the `builtin' interface requires Emacs 29.1 or later"
           triples-database-interface))
  (let ((file (or file triples-default-database-filename)))
    (pcase triples-database-interface
      ('builtin (let* ((db (sqlite-open file)))
                  (unless (sqlitep db)
                    (error "Could not open sqlite database at %s" file))
                  (condition-case nil
                      (progn
                        (triples-setup-table-for-builtin db)
                        (triples-maybe-upgrade-to-builtin db))
                    (error
                     (message "triples: failed to ensure proper database tables and indexes.  Trying an automatic fix.")
                     (triples-rebuild-builtin-database db)
                     (message "triples: fix completed, if this message re-occurs please file a bug report.")))
                  db))
      ('emacsql
       (require 'emacsql)
       (let* ((db (emacsql-sqlite-open file))
              (triple-table-exists
               (emacsql db [:select name
                                    :from sqlite_master
                                    :where (= type table) :and (= name 'triples)])))
         (unless triple-table-exists
           (emacsql db [:create-table triples ([(subject :not-null)
                                                (predicate text :not-null)
                                                (object :not-null)
                                                (properties text :not-null)])])
           (emacsql db [:create-index subject_idx :on triples [subject]])
           (emacsql db [:create-index subject_predicate_idx :on triples [subject predicate]])
           (emacsql db [:create-index predicate_object_idx :on triples [predicate object]])
           (emacsql db [:create-unique-index subject_predicate_object_properties_idx :on triples [subject predicate object properties]]))
         db))
      ('pg (triples-pg-connect)))))

(defvar triples-pg-connection-spec
  (list :database "triples"
        :user (user-login-name)
        :host "localhost"
        :port 5432)
  "Connection spec (a plist) used when `triples-database-interface' is `pg'.

Keys:
- :database -- PostgreSQL database name (string).
- :user     -- PostgreSQL user name (string).
- :host     -- Server host (string, default \"localhost\").  A
               directory such as \"/var/run/postgresql\" connects
               over a Unix domain socket.
- :port     -- Server port (integer, default 5432).
- :password -- Password (string or a function returning a string)
               or nil, e.g. for peer/trust auth.  Note that pg.el
               does not read ~/.pgpass, so a password must be given
               here when the server asks for one.

TLS is negotiated by pg.el when the server requires it; the
`emacsql-pg' backend used here does not expose pg.el's
`tls-options', so no further TLS configuration is possible.")

(defvar triples--pg-emacsql-clear-fixed nil
  "Whether `triples--pg-fix-emacsql-clear' has already run.")

(defun triples--pg-fix-emacsql-clear ()
  "Make `emacsql-clear' do nothing for `emacsql-pg' connections.

`emacsql' calls `emacsql-clear' before every statement, and the
generic implementation erases the connection's process buffer.
pg.el keeps its own integer read position into that buffer
\(`pgcon--position'), so erasing the buffer without resetting the
position makes every later read time out with `pg-timeout': the
connection looks like it has hung, as soon as `emacsql-pg'
connects.

Clearing the buffer is not needed for the pg backend anyway,
because `emacsql-pg' parses the result object returned by
`pg-exec' rather than the process buffer.  This is a workaround
for an incompatibility between `emacsql-pg' and recent pg.el
\(verified with pg.el 20260812); it can be dropped once
`emacsql-pg' stops calling the buffer-erasing `emacsql-clear'."
  (unless triples--pg-emacsql-clear-fixed
    (when (find-class 'emacsql-pg-connection nil)
      (with-no-warnings
        (cl-defmethod emacsql-clear ((_connection emacsql-pg-connection))
          "Do nothing; see `triples--pg-fix-emacsql-clear'."
          nil))
      (setq triples--pg-emacsql-clear-fixed t))))

(defun triples-pg-exists-check (db)
  "Return non-nil if DB has a `triples' table.
The name is resolved with the connection's `search_path', unlike a
hardcoded `information_schema' query filtered on
table_schema = \"public\", which silently fails for a database
whose current schema is not `public'."
  (caar (emacsql db "SELECT to_regclass('triples')")))

(defun triples-pg-connect (&optional spec)
  "Connect to a PostgreSQL database and make sure it is populated.
SPEC is a plist as in `triples-pg-connection-spec', or nil to use
that variable.  Returns the connection object, which can be used
with the rest of the triples API when `triples-database-interface'
is `pg'."
  (require 'emacsql)
  (require 'emacsql-pg)
  ;; Must happen before the `emacsql-pg' call below, which already
  ;; runs a statement of its own while connecting.
  (triples--pg-fix-emacsql-clear)
  (let* ((spec (or spec triples-pg-connection-spec))
         (db (emacsql-pg (plist-get spec :database)
                         (plist-get spec :user)
                         :host (or (plist-get spec :host) "localhost")
                         :password (plist-get spec :password)
                         :port (or (plist-get spec :port) 5432))))
    (unless (triples-pg-exists-check db)
      (triples-setup-table-for-pg db))
    db))

(defun triples-setup-table-for-pg (db)
  "Set up the triples table in PostgreSQL DB.
PostgreSQL needs its own existence check and has no sqlite-specific
constructs, so this is separate from the builtin setup.

The columns deliberately carry no explicit type: `emacsql' gives
them the backend's default TEXT type.  Writing `text' in the
column specification the way `triples-connect' does for the
`emacsql' interface would produce `predicate TEXT text NOT NULL',
which SQLite tolerates but PostgreSQL rejects."
  (emacsql db [:create-table triples ([(subject :not-null)
                                       (predicate :not-null)
                                       (object :not-null)
                                       (properties :not-null)])])
  (emacsql db [:create-index subject_idx :on triples [subject]])
  (emacsql db [:create-index subject_predicate_idx :on triples [subject predicate]])
  (emacsql db [:create-index predicate_object_idx :on triples [predicate object]])
  (emacsql db [:create-unique-index subject_predicate_object_properties_idx :on triples [subject predicate object properties]]))

(defun triples-setup-table-for-builtin (db)
  "Set up the triples table in DB.
This is a separate function due to the need to use it during
upgrades to version 0.3"
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS triples(subject NOT NULL, predicate TEXT NOT NULL, object NOT NULL, properties TEXT NOT NULL)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS subject_idx ON triples (subject)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS subject_predicate_idx ON triples (subject, predicate)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS predicate_object_idx ON triples (predicate, object)")
  (sqlite-execute db "CREATE UNIQUE INDEX IF NOT EXISTS subject_predicate_object_properties_idx ON triples (subject, predicate, object, properties)"))

(defun triples-close (db)
  "Close database DB."
  (pcase triples-database-interface
    ('builtin (sqlite-close db))
    ((or 'emacsql 'pg) (emacsql-close db))))

(defun triples-backup (_ filename num-to-keep)
  "Perform a backup of the db, located at path FILENAME.
The first argument is unused, but later may be used to specify
the running database.

This uses the same backup location and names as configured in
variables such as `backup-directory-alist'.  Due to the fact that
the database is never opened as a buffer, normal backups will not
work, therefore this function must be called instead.

Th DB argument is currently unused, but may be used in the future
if Emacs's native sqlite gains a backup feature.

FILENAME can be nil, if so `triples-default-database-filename'
will be used.  With the `pg' interface FILENAME is only used to
derive the name of the backup file; the database server decides
where the data actually lives.

This also will clear excess backup files, according to
NUM-TO-KEEP, which specifies how many backup files at max should
exist at any time.  Older backups are the ones that are deleted."
  (let ((filename (expand-file-name (or filename triples-default-database-filename))))
    (pcase triples-database-interface
      ('builtin (call-process triples-sqlite-executable nil nil nil filename
                              (format ".backup '%s'" (expand-file-name
                                                      (car (find-backup-file-name
                                                            filename))))))
      ('emacsql (call-process emacsql-sqlite-executable nil nil nil filename
                              (format ".backup '%s'" (expand-file-name
                                                      (car (find-backup-file-name
                                                            filename))))))
      ('pg (triples--pg-backup filename)))
    (let ((backup-files (file-backup-file-names filename)))
      (cl-loop for backup-file in (cl-subseq
                                   backup-files
                                   (min num-to-keep (length backup-files)))
               do (delete-file backup-file)))))

(defun triples--pg-backup (filename)
  "Dump the PostgreSQL database to the backup file for FILENAME.
Uses pg_dump with the connection spec in
`triples-pg-connection-spec'.  The dump is written as a plain SQL
script to the same backup location that `triples-backup' uses for
the other interfaces.

The dump is written to a temporary file first and only renamed
into place once pg_dump succeeds, so that a failed dump does not
leave a truncated file that looks like a usable backup."
  (let* ((spec triples-pg-connection-spec)
         (backup-file (expand-file-name (car (find-backup-file-name filename))))
         (pg-dump (or (executable-find "pg_dump")
                      (error "triples: the `pg_dump' program is required to back up a `pg' database, but it was not found in `exec-path'")))
         ;; Dump next to the final file so that the rename below is atomic.
         (temp-file (make-temp-file (expand-file-name
                                     (concat (file-name-nondirectory backup-file) "-")
                                     (file-name-directory backup-file))))
         (process-environment
          (append (when (plist-get spec :password)
                    (list (format "PGPASSWORD=%s" (plist-get spec :password))))
                  process-environment)))
    (unwind-protect
        (let ((status
               (apply #'call-process pg-dump nil nil nil
                      (append (list (format "--dbname=%s" (plist-get spec :database))
                                    (format "--username=%s" (plist-get spec :user))
                                    "--no-owner"
                                    "--no-privileges"
                                    (format "--file=%s" temp-file))
                              (when (plist-get spec :host)
                                (list (format "--host=%s" (plist-get spec :host))))
                              (when (plist-get spec :port)
                                (list (format "--port=%s" (plist-get spec :port))))))))
          (unless (zerop status)
            (error "triples: pg_dump failed with exit status %s" status))
          (rename-file temp-file backup-file t)
          (setq temp-file nil))
      (when (and temp-file (file-exists-p temp-file))
        (ignore-errors (delete-file temp-file))))))

(defun triples--decolon (sym)
  "Remove colon from SYM."
  (intern (string-replace ":" "" (format "%s" sym))))

(defun triples--encolon (sym)
  "Add a colon to SYM."
  (intern (format ":%s" sym)))

(defun triples-standardize-val (val)
  "If VAL is a string, return it as enclosed in quotes.

This is done to have compatibility with the way emacsql stores
values.  Turn a symbol into a string as well, but not a quoted
one, because sqlite cannot handle symbols.  Integers do not need
to be stringified."
  ;; Do not print control characters escaped - we want to get things out exactly
  ;; as we put them in.
  (let ((print-escape-control-characters nil))
    (pcase val
      ;; Just to save a bit of space, let's use "()" instead of "null", which is
      ;; what it would be turned into by the pcase above.
      ((pred null) "()")
      ((pred integerp) val)
      ((pred floatp) val)
      (_ (format "%S" val)))))

(defun triples-standardize-result (result)
  "Return RESULT in standardized form.
This imitates the way emacsql returns items, with strings
becoming either symbols, lists, or strings depending on whether
the string itself is wrapped in quotes."
  (if (numberp result)
      result
    (read result)))

(defun triples-db-insert (db subject predicate object &optional properties)
  "Insert triple to DB: SUBJECT, PREDICATE, OBJECT with PROPERTIES.
This is a SQL replace operation, because we don't want any
duplicates; if the triple is the same, it has to differ at least
with PROPERTIES.  This is a low-level function that bypasses our
normal schema checks, so should not be called from client programs."
  (unless (symbolp predicate)
    (error "Predicates in triples must always be symbols"))
  (when (and (fboundp 'plistp) (not (plistp properties)))
    (error "Properties stored must always be plists"))
  (pcase triples-database-interface
    ('builtin
     (sqlite-execute db "REPLACE INTO triples VALUES (?, ?, ?, ?)"
                     (list (triples-standardize-val subject)
                           (triples-standardize-val (triples--decolon predicate))
                           (triples-standardize-val object)
                           ;; Properties cannot be null, since in sqlite each null value
                           ;; is distinct from each other, so replace would not replace
                           ;; duplicate triples each with null properties.
                           (triples-standardize-val properties))))
    ('emacsql
     ;; We use a simple small plist '(:t t). Unlike sqlite, we can't insert this
     ;; as a string, or else it will store as something that would come out as a
     ;; string.  And if we use nil, it will actually store a NULL in the cell.
     (emacsql db [:replace :into triples :values $v1]
              (vector subject (triples--decolon predicate) object (or properties '(:t t)))))
    ('pg
     ;; PostgreSQL has no REPLACE INTO.  Since the unique index covers
     ;; every column of the table, a conflicting row is necessarily
     ;; identical to the one being inserted, so skipping it is the same
     ;; as replacing it.  If a column is ever added to the table that is
     ;; *not* part of that index, this has to become a real upsert
     ;; (ON CONFLICT ... DO UPDATE), because DO NOTHING would then keep
     ;; the stale value instead of overwriting it.
     ;;
     ;; The conflict target is spelled out rather than left implicit so
     ;; that this cannot silently swallow a conflict on some other
     ;; constraint, and so that the statement keeps working if a column
     ;; is appended to the table.
     (apply #'emacsql db
            (concat "INSERT INTO triples (subject, predicate, object, properties)"
                    " VALUES ($s1, $s2, $s3, $s4)"
                    " ON CONFLICT (subject, predicate, object, properties) DO NOTHING")
            (list subject (triples--decolon predicate) object (or properties '(:t t)))))))

(defun triples--emacsql-andify (wc)
  "In emacsql where clause WC, insert `:and' between query elements.
Returns the new list with the added `:and.'s.  The first element
MUST be there `:where' clause.  This does reverse the clause
elements, but it shouldn't matter."
  (cons (car wc) ;; the :where clause
        (let ((clauses (cdr wc))
              (result))
          (while clauses
            (push (car clauses) result)
            (if (cdr clauses) (push :and result))
            (setq clauses (cdr clauses)))
          result)))

(defun triples--emacsql-delete (db &optional subject predicate object properties)
  "Delete triples matching SUBJECT, PREDICATE, OBJECT, PROPERTIES from DB.
Shared by the `emacsql' and `pg' interfaces."
  (let ((n 0))
    (apply #'emacsql
           db
           (apply #'vector
                  (append '(:delete :from triples)
                          (when (or subject predicate object properties)
                            (triples--emacsql-andify
                             (append
                              '(:where)
                              (when subject `((= subject ,(intern (format "$s%d" (cl-incf n))))))
                              (when predicate `((= predicate ,(intern (format "$s%d" (cl-incf n))))))
                              (when object `((= object ,(intern (format "$s%d" (cl-incf n))))))
                              (when properties `((= properties ,(intern (format "$s%d" (cl-incf n)))))))))))
           (seq-filter #'identity (list subject predicate object properties)))))

(defun triples-db-delete (db &optional subject predicate object properties)
  "Delete triples matching SUBJECT, PREDICATE, OBJECT, PROPERTIES.

DB is the database to delete from.

If any of these are nil, they will not selected for.  If you set
all to nil, everything will be deleted, so be careful!"
  (pcase triples-database-interface
    ('builtin (sqlite-execute
               db
               (concat "DELETE FROM triples"
                       (when (or subject predicate object properties)
                         (concat " WHERE "
                                 (string-join
                                  (seq-filter #'identity
                                              (list (when subject "SUBJECT = ?")
                                                    (when predicate "PREDICATE = ?")
                                                    (when object "OBJECT = ?")
                                                    (when properties "PROPERTIES = ?")))
                                  " AND "))))
               (mapcar #'triples-standardize-val (seq-filter #'identity (list subject predicate object properties)))))
    ((or 'emacsql 'pg)
     (triples--emacsql-delete db subject predicate object properties))))

(defun triples-db-delete-subject-predicate-prefix (db subject pred-prefix)
  "Delete triples matching SUBJECT and predicates with PRED-PREFIX.

DB is the database to delete from."
  (unless (symbolp pred-prefix)
    (error "Predicates in triples must always be symbols"))
  (pcase triples-database-interface
    ('builtin (sqlite-execute db "DELETE FROM triples WHERE subject = ? AND predicate LIKE ?"
                              (list (triples-standardize-val subject)
                                    (format "%s/%%" (triples--decolon pred-prefix)))))
    ((or 'emacsql 'pg)
     (emacsql db [:delete :from triples :where (= subject $s1) :and (like predicate $r2)]
              subject (format "%s/%%" (triples--decolon pred-prefix))))))

(defconst triples--pg-numeric-regexp "^[+-]?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$"
  "Regexp matching the stored form of a numeric object in a `pg' database.
This accepts both integer and float objects, in simple decimal or
scientific notation, which is what `number-to-string' produces.
An infinite or NaN float is printed as \"1.0e+INF\" or
\"0.0e+NaN\", which this deliberately does not match.

Note that this is PostgreSQL regular expression syntax, which is
POSIX extended, not Emacs syntax: groups are written with bare
parentheses, and a backslash-parenthesis would mean a literal
parenthesis.  Do not run this through `string-match-p'.")

(defconst triples--comparison-operators '(= != < <= > >= like)
  "The comparison operators `triples-db-select-pred-op' accepts.
Restricting OP to this list both gives a clear error for
unsupported operators and keeps the operator from being spliced
into SQL from arbitrary caller input.")

(defun triples-db-select-pred-op (db pred op val &optional properties limit)
  "Select matching predicates with PRED having OP relation to VAL.

DB is the database to select from.

OP is a comparison operator, and VAL is the value to compare.  It
is a symbol for a standard numerical comparison such as `=',
`!=', `>', or, when `val' is a strings, `like'.  All alphabetic
comparison is case insensitive.

If PROPERTIES is given, triples must match the given properties.
If LIMIT is a positive integer, limit the results to that number."
  (unless (symbolp pred)
    (error "Predicates in triples must always be symbols"))
  (unless (memq op triples--comparison-operators)
    (error "Comparison operator %S is not one of %S" op triples--comparison-operators))
  (let ((pred (triples--decolon pred)))
    (pcase triples-database-interface
      ('builtin
       (mapcar (lambda (row) (mapcar #'triples-standardize-result row))
               (sqlite-select
                db
                (concat "SELECT * FROM triples WHERE predicate = ? AND  "
                        (cond ((integerp val) "CAST(object AS INTEGER) ")
                              ((floatp val) "CAST(object AS REAL) ")
                              (t "object COLLATE NOCASE "))
                        (symbol-name op) " ?"
                        (when properties " AND properties = ?")
                        (when (and limit (> limit 0)) (format " LIMIT %d" limit)))
                (append
                 (list (triples-standardize-val pred)
                       (triples-standardize-val val))
                 (when properties (list (triples-standardize-val properties)))))))
      ('emacsql
       (emacsql db
                (append
                 [:select * :from triples :where (= predicate $s1) :and]
                 (pcase op
                   ('< [(< object $s2)])
                   ('<= [(<= object $s2)])
                   ('= [(= object $s2)])
                   ('!= [(!= object $s2)])
                   ('>= [(>= object $s2)])
                   ('> [(> object $s2)])
                   ('like [(like object $s2)]))
                 (when (stringp val) [:collate :nocase])
                 (when properties
                   (list :and '(= properties $s3)))
                 (when (and limit (> limit 0))
                   (list :limit limit)))
                pred val properties))
      ('pg (triples--pg-select-pred-op db pred op val properties limit)))))

(defun triples--pg-select-pred-op-sql (pred op val properties limit)
  "Return the SQL and arguments for `triples--pg-select-pred-op'.
The return value is a cons of the SQL string, which uses `$sN'
placeholders numbered from 1, and the list of arguments to pass
to `emacsql' along with it.

PRED is a decoloned predicate symbol, OP one of
`triples--comparison-operators', and VAL the value to compare.

PostgreSQL has no COLLATE NOCASE, so string comparisons wrap both
sides in LOWER(), giving the same case-insensitive behavior as the
sqlite interfaces.  Integer and float objects are stored as text
in PostgreSQL (everything is TEXT), so numeric comparisons cast
the column with NUMERIC.  That is exact for both integers and
floats, and unlike the sqlite backends' `CAST(object AS
INTEGER/REAL)' it neither truncates a float nor loses precision on
a large integer.

The cast is done inside a CASE expression rather than behind an
`AND object ~ ...' guard: PostgreSQL does not guarantee the
evaluation order of AND operands, so a bare cast can be evaluated
for a non-numeric row and abort the whole query with \"invalid
input syntax for type numeric\".  CASE only evaluates the branch it
returns, so the cast is genuinely unreachable for non-numeric
objects."
  (let* ((n 0)
         (next-placeholder (lambda () (format "$s%d" (cl-incf n))))
         (pred-ph (funcall next-placeholder))
         (val-ph (funcall next-placeholder))
         (properties-ph (when properties (funcall next-placeholder)))
         (limit-ph (when (and limit (> limit 0)) (funcall next-placeholder)))
         (op-name (symbol-name op))
         (comparison
          (cond
           ((eq op 'like)
            ;; LIKE is only meaningful on text, so cast both sides when
            ;; the caller compared against a number.
            (if (or (integerp val) (floatp val))
                (format "CAST(object AS TEXT) LIKE CAST(%s AS TEXT)" val-ph)
              (format "LOWER(object) LIKE LOWER(%s)" val-ph)))
           ((or (integerp val) (floatp val))
            (format "CASE WHEN object ~ '%s' THEN CAST(object AS NUMERIC) END %s %s"
                    triples--pg-numeric-regexp op-name val-ph))
           (t
            (format "LOWER(object) %s LOWER(%s)" op-name val-ph))))
         (sql (concat "SELECT * FROM triples WHERE predicate = " pred-ph
                      " AND " comparison
                      (when properties
                        (concat " AND properties = " properties-ph))
                      (when limit-ph
                        (concat " LIMIT " limit-ph)))))
    (cons sql (append (list pred val)
                      (when properties (list properties))
                      (when limit-ph (list limit))))))

(defun triples--pg-select-pred-op (db pred op val &optional properties limit)
  "SELECT rows from PostgreSQL DB with PRED comparing OP VAL.
PRED is a decoloned predicate symbol; see
`triples--pg-select-pred-op-sql', which builds the statement."
  (let ((sql-args (triples--pg-select-pred-op-sql pred op val properties limit)))
    (apply #'emacsql db (car sql-args) (cdr sql-args))))

(defun triples-db-select-pred-prefix (db subject pred-prefix)
  "Return rows in DB matching SUBJECT and PRED-PREFIX."
  (pcase triples-database-interface
    ('builtin (mapcar (lambda (row) (mapcar #'triples-standardize-result row))
                      (sqlite-select db "SELECT * FROM triples WHERE subject = ? AND predicate LIKE ?"
                                     (list (triples-standardize-val subject)
                                           (format "%s/%%" pred-prefix)))))
    ((or 'emacsql 'pg)
     (emacsql db [:select * :from triples :where (= subject $s1) :and (like predicate $r2)]
              subject (format "%s/%%" pred-prefix)))))

(defun triples--emacsql-select (db &optional subject predicate object properties selector)
  "Return rows in DB matching SUBJECT, PREDICATE, OBJECT, PROPERTIES.
Shared by the `emacsql' and `pg' interfaces.  SELECTOR is a list
of symbols (subject, predicate, object, properties) to retrieve,
or nil for all columns."
  (let ((n 0))
    (apply #'emacsql
           db
           (apply #'vector
                  (append `(:select
                            ,(if selector (apply #'vector selector) '*)
                            :from triples)
                          (when (or subject predicate object properties)
                            (triples--emacsql-andify
                             (append
                              '(:where)
                              (when subject `((= subject ,(intern (format "$s%d" (cl-incf n))))))
                              (when predicate `((= predicate ,(intern (format "$s%d" (cl-incf n))))))
                              (when object `((= object ,(intern (format "$s%d" (cl-incf n))))))
                              (when properties `((= properties ,(intern (format "$s%d" (cl-incf n)))))))))))
           (seq-filter #'identity (list subject predicate object properties)))))

(defun triples-db-select (db &optional subject predicate object properties selector)
  "Return rows matching SUBJECT, PREDICATE, OBJECT, PROPERTIES.

DB is the database to select from.

If any of these are nil, they are not included in the select
statement.  The SELECTOR is list of symbols subject, precicate,
object, properties to retrieve or nil for *."
  (pcase triples-database-interface
    ('builtin (mapcar (lambda (row) (mapcar #'triples-standardize-result row))
                      (sqlite-select db
                                     (concat "SELECT "
                                             (if selector
                                                 (mapconcat (lambda (e) (format "%s" e)) selector ", ")
                                               "*") " FROM triples"
                                             (when (or subject predicate object properties)
                                               (concat " WHERE "
                                                       (string-join
                                                        (seq-filter #'identity
                                                                    (list (when subject "SUBJECT = ?")
                                                                          (when predicate "PREDICATE = ?")
                                                                          (when object "OBJECT = ?")
                                                                          (when properties "PROPERTIES = ?")))
                                                        " AND "))))
                                     (mapcar #'triples-standardize-val (seq-filter #'identity (list subject predicate object properties))))))
    ((or 'emacsql 'pg)
     (triples--emacsql-select db subject predicate object properties selector))))

(defun triples-db-count (db)
  "Return the number of triples in DB."
  (pcase triples-database-interface
    ('builtin (caar (sqlite-select db "SELECT COUNT(*) FROM triples")))
    ((or 'emacsql 'pg) (caar (emacsql db [:select (funcall count *) :from triples])))))

(defun triples-move-subject (db old-subject new-subject)
  "Replace all instance in DB of OLD-SUBJECT to NEW-SUBJECT.
Any references to OLD-SUBJECT as an object are also replaced.
This will throw an error if there is an existing subject
NEW-SUBJECT with at least one equal property (such as type
markers).  But if there are no commonalities, the OLD-SUBJECT is
merged into NEW-SUBJECT."
  (pcase triples-database-interface
    ('builtin
     (condition-case err
         (progn
           (sqlite-transaction db)
           (sqlite-execute db "UPDATE triples SET subject = ? WHERE subject = ?"
                           (list (triples-standardize-val new-subject) (triples-standardize-val old-subject)))
           (sqlite-execute db "UPDATE triples SET object = ? WHERE object = ?"
                           (list (triples-standardize-val new-subject) (triples-standardize-val old-subject)))
           (sqlite-commit db))
       (error (sqlite-rollback db)
              (signal 'error err))))
    ((or 'emacsql 'pg)
     ;; Use the triples transaction wrapper rather than
     ;; `emacsql-with-transaction' directly, so that the `pg' interface
     ;; gets its serialization-failure retries.
     (triples-with-transaction
       db
       (emacsql db [:update triples :set (= subject $s1) :where (= subject $s2)]
                new-subject old-subject)
       (emacsql db [:update triples :set (= object $s1) :where (= object $s2)]
                new-subject old-subject)))))

;; Code after this point should not call sqlite or emacsql directly. If any more
;; calls are needed, put them in a defun, make it work for sqlite and emacsql,
;; and put them above.

(defun triples--subjects (triples)
  "Return all unique subjects in TRIPLES."
  (seq-uniq (mapcar #'car triples)))

(defun triples--group-by-subjects (triples)
  "Return an alist of subject to TRIPLES with that subject."
  (let ((subj-to-triples (make-hash-table :test #'equal)))
    (dolist (triple triples)
      (puthash (car triple)
               (cons triple (gethash (car triple) subj-to-triples))
               subj-to-triples))
    (cl-loop for k being the hash-keys of subj-to-triples using (hash-values v)
             collect (cons k v))))

(defun triples--add (db op)
  "Perform OP on DB."
  (pcase (car op)
    ('replace-subject
     (mapc
      (lambda (sub)
        (triples-db-delete db sub))
      (triples--subjects (cdr op))))
    ('replace-subject-type
     (mapc (lambda (sub-triples)
             (mapc (lambda (type)
                     ;; We have to ignore base, which keeps type information in general.
                     (unless (eq type 'base)
                       (triples-db-delete-subject-predicate-prefix db (car sub-triples) type)))
                   (seq-uniq
                    (mapcar #'car (mapcar #'triples-combined-to-type-and-prop
                                          (mapcar #'cl-second (cdr sub-triples)))))))
           (triples--group-by-subjects (cdr op)))))
  (mapc (lambda (triple)
          (apply #'triples-db-insert db triple))
        (cdr op)))

(defun triples-properties-for-predicate (db cpred)
  "Return the properties in DB for combined predicate CPRED as a plist."
  (mapcan (lambda (row)
            (list (intern (format ":%s" (nth 1 row))) (nth 2 row)))
          (triples-db-select db cpred)))

(defun triples-predicates-for-type (db type)
  "Return all predicates defined for TYPE in DB."
  (mapcar #'car
          (triples-db-select db type 'schema/property nil nil '(object))))

(defun triples-verify-schema-compliant (triples prop-schema-alist)
  "Error if TRIPLES is not compliant with schema in PROP-SCHEMA-ALIST.
PROP-SCHEMA-ALIST is an alist of the relevant properties to the
data stored, in combined type/property form, and their schema
definitions."
  (mapc (lambda (triple)
          (pcase-let ((`(,type . ,_) (triples-combined-to-type-and-prop (nth 1 triple))))
            (unless (or (eq type 'base) (assoc (nth 1 triple) prop-schema-alist))
              (error "Property %s not found in schema" (nth 1 triple)))))
        triples)
  (mapc (lambda (triple)
          (triples--plist-mapc (lambda (pred-prop val)
                                 (let ((f (intern (format "triples-verify-%s-compliant"
                                                          (triples--decolon pred-prop)))))
                                   (if (fboundp f)
                                       (funcall f val triple))))
                               (cdr (assoc (nth 1 triple) prop-schema-alist)))) triples))

(defun triples-add-schema (db type &rest props)
  "Add schema for TYPE and its PROPS to DB.
If a `:base/type' is not specified, it is assumed to be a string."
  ;; First, make sure all props are compliant with `triples-basic-types'.
  (mapc (lambda (prop)
          (when (consp prop)
            (let ((settings (cdr prop)))
              (when (and (plistp settings) (plist-get settings :base/type)
                         (not (member (plist-get settings :base/type) triples-basic-types)))
                (error "Property types must be a in `triples-basic-types'")))))
        props)
  (triples--add db (apply #'triples--add-schema-op type props)))

(defun triples--add-schema-op (type &rest props)
  "Return the operation store schema for TYPE, with PROPS.
PROPS is a list of either property symbols, or lists of
properties of the type and the meta-properties associated with
them."
  (cons 'replace-subject-type
        (cons `(,type base/type schema)
              (cl-loop for p in props
                       nconc
                       (let* ((pname (if (symbolp p) p (car p)))
                              (pprops (when (listp p) (cdr p)))
                              (pcombined (intern (format "%s/%s" type pname))))
                         (cons (list type 'schema/property pname)
                               (seq-filter #'identity
                                           (triples--plist-mapcar
                                            (lambda (k v)
                                              ;; If V is nil, that's the default, so don't
                                              ;; store anything.
                                              (when v
                                                (list pcombined (triples--decolon k) v)))
                                            pprops))))))))

(defun triples-remove-schema-type (db type)
  "Remove the schema for TYPE in DB, and all associated data."
  (triples-with-transaction
    db
    (let ((subjects (triples-subjects-of-type db type)))
      (mapc (lambda (subject)
              (triples-remove-type db subject type))
            subjects)
      (triples-remove-type db type 'schema))))

(defun triples-count (db)
  "Return the number of triples in DB."
  (triples-db-count db))

(defun triples-set-type (db subject type &rest properties)
  "Create operation to replace PROPERTIES for TYPE for SUBJECT in DB.
PROPERTIES is a plist of properties, without TYPE prefixes."
  (let* ((prop-schema-alist
          ;; If the type doesn't exist, there is no schema to check against.
          (when (triples-get-type db type 'schema)
            (triples--plist-mapcar
             (lambda (k v)
               (cons (triples--decolon k) v))
             (triples-properties-for-predicate db (triples-type-and-prop-to-combined type 'schema/property)))
            (mapcar (lambda (prop)
                      (cons (triples--decolon prop)
                            (triples--plist-mapcan
                             (lambda (prop value)
                               (when (or (not (eq prop :base/type))
                                         (member value triples-basic-types))
                                 (list prop value)))
                             (triples-properties-for-predicate
                              db
                              (triples-type-and-prop-to-combined type prop)))))
                    (triples--plist-mapcar (lambda (k _) k) properties))))
         (op (triples--set-type-op subject type properties prop-schema-alist)))
    (triples-verify-schema-compliant
     (cdr op)
     ;; triples-verify-schema-compliant can act on triples from many types, so
     ;; we have to include the type information in our schema property alist.
     (mapcar (lambda (c)
               (cons (triples-type-and-prop-to-combined type (car c))
                     (cdr c))) prop-schema-alist))
    (triples--add db op)))

(defmacro triples--eval-when-fboundp (sym form)
  "Delay macroexpansion to runtime if SYM is not yet `fboundp'.
FORM is the code to delay."
  (declare (indent 1) (debug (symbolp form)))
  (if (fboundp sym)
      form
    `(eval ',form t)))

(defgroup triples nil
  "A database of triples."
  :group 'data
  :prefix "triples-")

(defcustom triples-pg-transaction-retries 5
  "How many times to retry a failed transaction with the `pg' interface.
`emacsql-with-transaction' retries sqlite's `emacsql-locked'
errors, but PostgreSQL reports concurrency conflicts as
serialization failures or deadlocks instead.  Since `emacsql-pg'
connects with `default_transaction_isolation' set to
SERIALIZABLE, those are expected under concurrent writes, so
transactions are retried up to this many times.  Retrying is safe
because `emacsql-with-transaction' requires the body to have no
side effects other than database changes.

Set this to 0 to disable retrying."
  :type 'integer
  :group 'triples)

(defconst triples--pg-retryable-error-regexp
  (concat "could not serialize access"
          "\\|deadlock detected"
          "\\|concurrent update"
          "\\|canceling statement due to conflict")
  "Regexp matching the PostgreSQL errors worth retrying a transaction for.")

(defun triples--pg-retryable-error-p (err)
  "Return non-nil if ERR describes a retryable PostgreSQL conflict.
ERR is the error object bound by `condition-case'."
  (string-match-p triples--pg-retryable-error-regexp (error-message-string err)))

(defun triples--pg-with-transaction (db body-fun)
  "Wrap BODY-FUN in a transaction for DB, retrying on conflicts.
See `triples-pg-transaction-retries'."
  (let ((run (triples--eval-when-fboundp emacsql-with-transaction
               (lambda (db body-fun)
                 (emacsql-with-transaction db (funcall body-fun)))))
        (tries (max 1 (1+ triples-pg-transaction-retries)))
        (done nil)
        result)
    (while (not done)
      (condition-case err
          (setq result (funcall run db body-fun)
                done t)
        (error
         (if (and (> tries 1) (triples--pg-retryable-error-p err))
             (progn
               (setq tries (1- tries))
               (sleep-for 0.05))
           (signal (car err) (cdr err))))))
    result))

(defun triples--with-transaction (db body-fun)
  "Wrap BODY-FUN in a transaction for DB."
  (pcase triples-database-interface
    ('builtin  (condition-case err
                   (progn
                     (sqlite-transaction db)
                     (funcall body-fun)
                     (sqlite-commit db))
                 (error (sqlite-rollback db)
                        (signal (car err) (cdr err)))))
    ('emacsql
     (funcall (triples--eval-when-fboundp emacsql-with-transaction
               (lambda (db body-fun)
                 (emacsql-with-transaction db (funcall body-fun))))
              db body-fun))
    ('pg (triples--pg-with-transaction db body-fun))))

(defun triples-set-types (db subject &rest combined-props)
  "Set all data for types in COMBINED-PROPS in DB for SUBJECT.
COMBINED-PROPS is a plist which takes combined properties such as
:named/name and their values.  All other data related to the types
given in the COMBINED-PROPS will be removed."
  (let ((type-to-plist (make-hash-table)))
    (triples--plist-mapc
     (lambda (cp val)
       (pcase-let ((`(,type . ,prop) (triples-combined-to-type-and-prop cp)))
         (puthash (triples--decolon type)
                  (plist-put (gethash (triples--decolon type) type-to-plist)
                             (triples--encolon prop) val) type-to-plist)))
     combined-props)
    (triples-with-transaction
      db
      (cl-loop for k being the hash-keys of type-to-plist using (hash-values v)
               do (apply #'triples-set-type db subject k v)))))

(defun triples--set-type-op (subject type properties type-schema)
  "Create operation to replace PROPERTIES for TYPE for SUBJECT.
PROPERTIES is a plist of properties, without TYPE prefixes.
TYPE-SCHEMA is an alist of property symbols to their schema,
which is necessary to understand when lists are supposed to be
broken down into separate rows, and when to leave as is."
  (cons 'replace-subject-type
        (cons (list subject 'base/type type)
              (triples--plist-mapcan
               (lambda (prop v)
                 (let ((prop-schema (cdr (assoc (triples--decolon prop) type-schema))))
                   (if (and
                        (listp v)
                        (not (plist-get prop-schema :base/unique)))
                       (cl-loop for e in v for i from 0
                                collect
                                (list subject
                                      (triples-type-and-prop-to-combined type prop)
                                      e
                                      (list :index i)))
                     (list (list subject (triples-type-and-prop-to-combined type prop) v)))))
               properties))))

(defun triples-get-type (db subject type)
  "From DB get data associated with TYPE for SUBJECT."
  (let ((preds (make-hash-table :test #'equal)))
    (mapc (lambda (db-triple)
            (puthash (nth 1 db-triple)
                     (cons (cons (nth 2 db-triple) (nth 3 db-triple))
                           (gethash (nth 1 db-triple) preds))
                     preds))
          (triples-db-select-pred-prefix db subject type))
    (append
     (cl-loop for k being the hash-keys of preds using (hash-values v)
              nconc (list (triples--encolon (cdr (triples-combined-to-type-and-prop k)))
                          (if (and (car v)
                                   (plist-get (cdar v) :index))
                              (mapcar #'car (sort v (lambda (a b)
                                                      (< (plist-get (cdr a) :index)
                                                         (plist-get (cdr b) :index)))))
                            (caar v))))
     (cl-loop for pred in (triples-predicates-for-type db type)
              nconc
              (let ((reversed-prop (plist-get
                                    (triples-properties-for-predicate
                                     db (triples-type-and-prop-to-combined type pred))
                                    :base/virtual-reversed)))
                (when reversed-prop
                  (let ((result
                         (triples-db-select db nil reversed-prop subject nil '(subject))))
                    (when result (cons (triples--encolon pred) (list (mapcar #'car result)))))))))))

(defun triples-remove-type (db subject type)
  "Remove TYPE for SUBJECT in DB, and all associated data."
  (triples-with-transaction
    db
    (triples-db-delete db subject 'base/type type)
    (triples-db-delete-subject-predicate-prefix db subject type)))

(defun triples-get-types (db subject)
  "From DB, get all types for SUBJECT."
  (mapcar #'car
          (triples-db-select db subject 'base/type nil nil '(object))))

(defun triples-get-subject (db subject)
  "From DB return all properties for SUBJECT as a single plist."
  (mapcan (lambda (type)
            (triples--plist-mapcan
             (lambda (k v)
               (list (intern (format ":%s/%s" type (triples--decolon k))) v))
             (triples-get-type db subject type)))
          (triples-get-types db subject)))

(defun triples-set-subject (db subject &rest type-vals-cons)
  "From DB set properties of SUBJECT to TYPE-VALS-CONS data.
TYPE-VALS-CONS is a list of conses, combining a type and a plist of values."
  (triples-with-transaction db
                            (triples-delete-subject db subject)
                            (mapc (lambda (cons)
                                    (apply #'triples-set-type db subject cons))
                                  type-vals-cons)))

(defun triples-delete-subject (db subject)
  "Delete all data in DB associated with SUBJECT.
This usually should not be called, it's better to just delete
data you own with `triples-remove-type'."
  (triples-db-delete db subject))

(defun triples-search (db cpred text &optional limit)
  "Search DB for instances of combined property CPRED with TEXT.
If LIMIT is a positive integer, limit the results to that number."
  (triples-db-select-pred-op db cpred 'like (format "%%%s%%" text) nil limit))

(defun triples-with-predicate (db cpred)
  "Return all triples in DB with CPRED as its combined predicate."
  (triples-db-select db nil cpred))

(defun triples-subjects-with-predicate-object (db cpred obj)
  "Return all subjects in DB with CPRED equal to OBJ.
Subjects will not be returned more than once."
  (seq-uniq (mapcar #'car (triples-db-select db nil cpred obj))))

(defun triples-subjects-of-type (db type)
  "Return a list of all subjects with a particular TYPE in DB."
  (triples-subjects-with-predicate-object db 'base/type type))

(defun triples-combined-to-type-and-prop (combined)
  "Return cons of type and prop that form the COMBINED normal representation.
This is something of form `:type/prop'."
  (let ((s (split-string (format "%s" combined) "/")))
    (cons (triples--decolon (nth 0 s)) (intern (nth 1 s)))))

(defun triples-type-and-prop-to-combined (type prop)
  "Format TYPE and PROP to a combined format - type/prop."
  (intern (format "%s/%s" (triples--decolon type) (triples--decolon prop))))

(defun triples--plist-mapc (fn plist)
  "Map FN over PLIST, for only side effects.
FN must take two arguments: the key and the value."
  (let ((plist-index plist))
    (while plist-index
      (let ((key (pop plist-index)))
        (funcall fn key (pop plist-index))))))

(defun triples--plist-mapcar (fn plist)
  "Map FN over PLIST, returning an element for every property.
FN must take two arguments: the key and the value."
  (let ((plist-index plist)
        (result))
    (while plist-index
      (let ((key (pop plist-index)))
        (push (funcall fn key (pop plist-index)) result)))
    (nreverse result)))

(defun triples--plist-mapcan (fn plist)
  "Map FN over PLIST, nconcing elements together.
FN must take two arguments: the key and the value."
  (let ((plist-index plist)
        (result))
    (while plist-index
      (let ((key (pop plist-index)))
        (setq result (nconc result (funcall fn key (pop plist-index))))))
    result))

;; Standard properties

(defun triples-verify-base/unique-compliant (uniquep triple)
  "Verify that TRIPLE has an index or not, based on UNIQUEP."
  (if uniquep
      (when (member :index (nth 3 triple))
        (error "Invalid triple found: %s, violates base/unique, should be just one value" triple))
    (unless (member :index (nth 3 triple))
      (error "Invalid triple found: %s, violates base/unique, should be a list of values" triple))))

(defun triples-verify-base/type-compliant (type triple)
  "Verify that TRIPLE's object is of TYPE."
  (unless (eq (type-of (nth 2 triple)) type)
    (error "Triple %s has an object with the wrong type: expected type of %s but was %s"
           triple type (type-of (nth 2 triple)))))

(defun triples-verify-base/virtual-reversed-compliant (_ triple)
  "Reject any TRIPLE with a virtual reversed property.

Virtual reversed properties shouldn't be set manually, so are
never compliant."
  (error "Invalid triple found: %s, should not be setting a `base/virtual-reversed' property"
         triple))

(provide 'triples)

;;; triples.el ends here
