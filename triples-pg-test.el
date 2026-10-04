;;; triples-pg-test.el --- Tests for the PostgreSQL backend.  -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the `pg' database interface.
;;
;; The tests for SQL generation only need the `emacsql' and
;; `emacsql-pg' libraries, and no PostgreSQL server, so they run
;; everywhere.  The tests that talk to a real server are skipped
;; unless `triples-pg-test-connection-spec' is non-nil, which
;; happens when the `TRIPLES_TEST_PG_DATABASE' environment variable
;; is set.  See that variable's documentation for the other
;; environment variables that configure the connection.
;;
;; The integration tests empty the `triples' table before and after
;; every test, so point them at a scratch database, never at one
;; holding data you care about.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'triples)
(require 'emacsql nil t)                ;; May be absent.
(require 'emacsql-pg nil t)             ;; May be absent.

(defvar triples-pg-test-connection-spec
  (let ((database (getenv "TRIPLES_TEST_PG_DATABASE")))
    (when (and database (not (string-empty-p database)))
      (append (list :database database
                    :user (or (getenv "TRIPLES_TEST_PG_USER") (user-login-name))
                    :host (or (getenv "TRIPLES_TEST_PG_HOST") "localhost"))
              (when-let* ((port (getenv "TRIPLES_TEST_PG_PORT")))
                (list :port (string-to-number port)))
              (when-let* ((password (getenv "TRIPLES_TEST_PG_PASSWORD")))
                (list :password password)))))
  "Connection spec used by the `pg' integration tests.
Nil, the default, means that no PostgreSQL server is configured,
and the integration tests are skipped.

Set `TRIPLES_TEST_PG_DATABASE' to the name of a scratch database to
run them, and optionally `TRIPLES_TEST_PG_USER',
`TRIPLES_TEST_PG_HOST', `TRIPLES_TEST_PG_PORT' and
`TRIPLES_TEST_PG_PASSWORD' to configure the connection.")

(defmacro triples-pg-test-with-db (&rest body)
  "Run BODY with DB bound to a connected, empty `pg' database.
The integration tests are skipped unless
`triples-pg-test-connection-spec' is non-nil."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless triples-pg-test-connection-spec)
     (skip-unless (and (require 'emacsql nil t)
                       (require 'emacsql-pg nil t)
                       (require 'pg nil t)))
     (let* ((triples-database-interface 'pg)
            (triples-pg-connection-spec triples-pg-test-connection-spec)
            (db (triples-connect)))
       (unwind-protect
           (progn
             (triples-db-delete db)
             ,@body)
         (ignore-errors (triples-db-delete db))
         (ignore-errors (triples-close db))))))

(defmacro triples-pg-test-captured-sql (&rest body)
  "Return the list of SQL statements that BODY generates.
BODY runs with `db' bound to a stubbed `pg' connection, so this
needs neither a PostgreSQL server nor a working pg.el install."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (featurep 'emacsql-pg))
     (let ((triples-pg-test-sql nil)
           (triples-database-interface 'pg)
           (db (make-instance 'emacsql-pg-connection
                              :handle 'stub :pgcon 'stub :dbname "triples")))
       (cl-letf (((symbol-function 'emacsql-clear)
                  (lambda (&rest _) nil))
                 ((symbol-function 'emacsql-send-message)
                  (lambda (_connection message)
                    (push message triples-pg-test-sql)
                    nil))
                 ((symbol-function 'emacsql-parse)
                  (lambda (&rest _) nil)))
         ,@body)
       (nreverse triples-pg-test-sql))))

;;; SQL generation (no server needed)

(ert-deftest triples-pg-test-select-pred-op-string-sql ()
  (should (equal (car (triples--pg-select-pred-op-sql 'pred/foo '= "obj" nil nil))
                 "SELECT * FROM triples WHERE predicate = $s1 AND LOWER(object) = LOWER($s2)"))
  (should (equal (cdr (triples--pg-select-pred-op-sql 'pred/foo '= "obj" nil nil))
                 '(pred/foo "obj")))
  (should (equal (car (triples--pg-select-pred-op-sql 'pred/foo 'like "ob%" nil nil))
                 "SELECT * FROM triples WHERE predicate = $s1 AND LOWER(object) LIKE LOWER($s2)")))

(ert-deftest triples-pg-test-select-pred-op-numeric-sql ()
  ;; The cast must be inside CASE, not guarded by a sibling AND: the
  ;; evaluation order of AND operands is not guaranteed, and a bare
  ;; cast of a non-numeric object aborts the whole query.
  (dolist (val '(5 5.0 1.5 1e+20))
    (let ((sql (car (triples--pg-select-pred-op-sql 'pred/foo '> val nil nil))))
      (should (equal sql
                     (concat "SELECT * FROM triples WHERE predicate = $s1 AND "
                             "CASE WHEN object ~ '^[+-]?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$' "
                             "THEN CAST(object AS NUMERIC) END > $s2")))
      (should-not (string-match-p " AND object ~ " sql))))
  (should (equal (cdr (triples--pg-select-pred-op-sql 'pred/foo '> 5 nil nil))
                 '(pred/foo 5)))
  ;; The regexp is written in PostgreSQL syntax, where groups are bare
  ;; parentheses; a backslash-parenthesis would match a literal one.
  (should-not (string-match-p "\\\\(" triples--pg-numeric-regexp))
  ;; LIKE against a number is a text comparison.
  (should (equal (car (triples--pg-select-pred-op-sql 'pred/foo 'like 5 nil nil))
                 (concat "SELECT * FROM triples WHERE predicate = $s1 AND "
                         "CAST(object AS TEXT) LIKE CAST($s2 AS TEXT)"))))

(ert-deftest triples-pg-test-select-pred-op-properties-and-limit-sql ()
  (let* ((sql-args (triples--pg-select-pred-op-sql 'pred/foo '= "obj" '(:t t) 10))
         (sql (car sql-args)))
    (should (equal sql
                   (concat "SELECT * FROM triples WHERE predicate = $s1 AND LOWER(object) = LOWER($s2)"
                           " AND properties = $s3 LIMIT $s4")))
    (should (equal (cdr sql-args) '(pred/foo "obj" (:t t) 10))))
  ;; Without properties the limit takes $s3, which is the case that a
  ;; hand-numbered placeholder gets wrong.
  (let* ((sql-args (triples--pg-select-pred-op-sql 'pred/foo '= "obj" nil 10))
         (sql (car sql-args)))
    (should (equal sql
                   (concat "SELECT * FROM triples WHERE predicate = $s1 AND LOWER(object) = LOWER($s2)"
                           " LIMIT $s3")))
    (should (equal (cdr sql-args) '(pred/foo "obj" 10))))
  ;; A non-positive limit means no LIMIT clause at all.
  (should-not (string-match-p "LIMIT"
                              (car (triples--pg-select-pred-op-sql 'pred/foo '= "obj" nil 0)))))

(ert-deftest triples-pg-test-select-pred-op-value-escapes ()
  (let ((sql (triples-pg-test-captured-sql
               (triples-db-select-pred-op db 'pred/foo '= "obj" '(:t t) 3))))
    (should (= 1 (length sql)))
    (should (string-match-p "LOWER(object) = LOWER('\"obj\"')" (car sql)))
    (should (string-match-p "properties = '(:t t)'" (car sql)))
    (should (string-match-p "LIMIT 3" (car sql)))))

(ert-deftest triples-pg-test-schema-sql ()
  (let ((sql (triples-pg-test-captured-sql
               (triples-setup-table-for-pg db))))
    (should (equal (length sql) 5))
    (should (string-match-p "CREATE TABLE triples (\\(subject\\|predicate\\|object\\|properties\\)" (car sql)))
    (should (string-match-p "TEXT NOT NULL" (car sql)))
    ;; PostgreSQL rejects the extra bare `text' token that the emacsql
    ;; (sqlite) schema uses.
    (should-not (string-match-p "TEXT text" (car sql)))
    (should (string-match-p "CREATE UNIQUE INDEX subject_predicate_object_properties_idx" (nth 4 sql)))))

(ert-deftest triples-pg-test-insert-uses-explicit-conflict-target ()
  (let ((sql (triples-pg-test-captured-sql
               (triples-db-insert db "sub" 'pred "obj"))))
    (should (= 1 (length sql)))
    (should (string-match-p
             (concat "INSERT INTO triples (subject, predicate, object, properties) VALUES ("
                     ".+ ON CONFLICT (subject, predicate, object, properties) DO NOTHING")
             (car sql)))
    ;; Values are escaped exactly the way the `emacsql' interface escapes
    ;; them, so both backends store the same thing.
    (should (string-match-p "'\"sub\"'" (car sql)))
    (should (string-match-p "'pred'" (car sql)))
    (should (string-match-p "'\"obj\"'" (car sql)))
    (should (string-match-p "'(:t t)'" (car sql)))))

(ert-deftest triples-pg-test-connect-existence-check ()
  ;; `emacsql' appends the statement terminator.
  (let ((sql (triples-pg-test-captured-sql
               (triples-pg-exists-check db))))
    (should (equal sql '("SELECT to_regclass('triples');")))))

;;; Integration tests (need TRIPLES_TEST_PG_DATABASE)

(ert-deftest triples-pg-test-round-trip ()
  (triples-pg-test-with-db
    (triples-db-insert db "sub" 'pred "obj")
    (should (equal (mapcar (lambda (row) (seq-take row 3)) (triples-db-select db))
                   '(("sub" pred "obj"))))
    ;; Inserting the very same triple again must not add a row.
    (triples-db-insert db "sub" 'pred "obj")
    (should (= 1 (triples-db-count db)))
    ;; Different properties are a different row, exactly as with
    ;; REPLACE on the sqlite backends, since the unique index covers
    ;; the properties column too.
    (triples-db-insert db "sub" 'pred "obj" '(:t t :extra t))
    (should (= 2 (triples-db-count db)))
    ;; Deleting is scoped by every argument that is given.
    (triples-db-delete db "sub" 'pred "obj" '(:t t :extra t))
    (should (= 1 (triples-db-count db)))
    (triples-db-delete db "sub" 'pred "obj")
    (should (= 0 (triples-db-count db)))))

(ert-deftest triples-pg-test-numeric-comparison-ignores-non-numeric-objects ()
  "A non-numeric object must not abort a numeric comparison."
  (triples-pg-test-with-db
    (triples-db-insert db "sub" 'pred/num "not-a-number")
    (triples-db-insert db "sub" 'pred/num 5)
    (triples-db-insert db "sub" 'pred/num 10.5)
    (cl-flet ((matching (op val)
                ;; The row order of a SELECT without ORDER BY is not
                ;; defined, so sort before comparing.
                (sort (mapcar #'caddr
                              (triples-db-select-pred-op db 'pred/num op val))
                      #'<)))
      ;; An integer bound still matches a float object, as it does for
      ;; the builtin backend, where the object is cast to a number.
      (should (equal (matching '> 6) '(10.5)))
      (should (equal (matching '>= 5) '(5 10.5)))
      (should (equal (matching '< 6.0) '(5)))
      (should (equal (matching '= 5) '(5)))
      (should (equal (matching '!= 5) '(10.5)))
      ;; Large integers must not lose precision to floating point.
      (triples-db-insert db "sub" 'pred/num 9007199254740993)
      (should (equal (matching '= 9007199254740993) '(9007199254740993))))))

(ert-deftest triples-pg-test-string-comparison-is-case-insensitive ()
  (triples-pg-test-with-db
    (triples-db-insert db "sub" 'pred/name "Foo Bar")
    (should (equal (mapcar #'caddr
                           (triples-db-select-pred-op db 'pred/name '= "foo bar"))
                   '("Foo Bar")))
    (should (equal (mapcar #'caddr
                           (triples-db-select-pred-op db 'pred/name 'like "foo%"))
                   '("Foo Bar")))))

(ert-deftest triples-pg-test-transaction ()
  (triples-pg-test-with-db
    (triples-db-insert db "sub1" 'pred "obj")
    (triples-with-transaction
      db
      (triples-db-insert db "sub2" 'pred "obj"))
    (should (= 2 (triples-db-count db)))
    ;; A failing transaction must roll back.
    (should-error
     (triples-with-transaction
       db
       (triples-db-insert db "sub3" 'pred "obj")
       (error "boom")))
    (should (= 2 (triples-db-count db)))))

(ert-deftest triples-pg-test-public-api ()
  "Exercise the high-level API, not just the low-level database calls."
  (triples-pg-test-with-db
    (triples-add-schema db 'person
                        '(name :base/unique t :base/type string)
                        '(age :base/unique t :base/type integer))
    (triples-set-type db "alice" 'person :name "Alice" :age 41)
    (let ((got (triples-get-type db "alice" 'person)))
      (should (equal (plist-get got :name) "Alice"))
      (should (equal (plist-get got :age) 41)))
    (should (equal (plist-get (triples-get-type db "alice" 'person) :name) "Alice"))
    ;; Schema information is stored in the database itself, so it must
    ;; survive a re-connect.
    (triples-close db)
    (setq db (triples-connect))
    (let ((got (triples-get-type db "alice" 'person)))
      (should (equal (plist-get got :name) "Alice"))
      (should (equal (plist-get got :age) 41)))
    (triples-move-subject db "alice" "bob")
    (should (null (triples-get-subject db "alice")))
    (should (equal (plist-get (triples-get-type db "bob" 'person) :name) "Alice"))
    (triples-delete-subject db "bob")
    (should (null (triples-get-subject db "bob")))))

(ert-deftest triples-pg-test-refuses-fts ()
  (triples-pg-test-with-db
    (require 'triples-fts nil t)
    (skip-unless (featurep 'triples-fts))
    (should-error (triples-fts-setup db) :type 'error)))

(ert-deftest triples-pg-test-backup ()
  (skip-unless (executable-find "pg_dump"))
  (triples-pg-test-with-db
    (triples-db-insert db "sub" 'pred "obj")
    (let* ((dir (make-temp-file "triples-pg-backup" t))
           (backup-directory-alist `(("." . ,dir)))
           (file (expand-file-name "triples.db" dir)))
      (unwind-protect
          (progn
            (triples-backup db file 1)
            ;; The backup file is named by `find-backup-file-name',
            ;; which may or may not use numbered backups.
            (let ((dumps (seq-filter (lambda (f) (string-suffix-p "~" f))
                                     (directory-files dir nil "triples"))))
              (should (= 1 (length dumps)))
              (with-temp-buffer
                (insert-file-contents (expand-file-name (car dumps) dir))
                (goto-char (point-min))
                (should (search-forward "CREATE TABLE" nil t))))
            ;; The temporary dump file must be gone.
            (should (null (directory-files dir nil "triples\\.db-"))))
        (delete-directory dir t)))))

(ert-deftest triples-pg-test-backup-file-kept-on-failure ()
  (skip-unless (executable-find "pg_dump"))
  (triples-pg-test-with-db
    (let* ((dir (make-temp-file "triples-pg-backup" t))
           (triples-pg-connection-spec
            (plist-put (copy-sequence triples-pg-connection-spec)
                       :database "no-such-database-here")))
      (unwind-protect
          (progn
            (should-error (triples-backup db (expand-file-name "triples.db" dir) 1))
            ;; A failed dump must not leave a partial backup, nor the
            ;; temporary file it was written to.
            (should (null (directory-files dir nil "triples"))))
        (delete-directory dir t)))))

(provide 'triples-pg-test)

;;; triples-pg-test.el ends here
