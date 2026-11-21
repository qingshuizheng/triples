;;; triples-test-utils.el --- Test utilities for triples.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

;; Thyis file contiains utilities for testing triples.el, all with the
;; `triples-test' prefix.

;;; Code:

(defvar triples-test-db-file nil
  "The database file used in a test.
This is defined so we can easily debug into it.")

(defmacro triples-test-with-temp-db (&rest body)
  "Run BODY with a temporary database file."
  (declare (indent 0) (debug t))
  `(let ((db-file (make-temp-file "triples-test")))
     (unwind-protect
         (progn
           (let ((db (triples-connect db-file)))
             (setq triples-test-db-file db-file)
             ,@body
             (triples-close db)))
       (delete-file db-file))))


(defvar triples-test-pg-database "ekg_test"
  "PostgreSQL database for the -pg test variants.
The name must end in `_test': the macro below drops the triples table.")

(defmacro triples-test-with-pg-db (&rest body)
  "Run BODY against `triples-test-pg-database' over PostgreSQL."
  (declare (indent 0) (debug t))
  `(let ((triples-database-interface 'pg)
         (triples-pg-connection-spec
          (list :database triples-test-pg-database :user "ekg"
                :host "localhost" :port 5432)))
     (unless (string-suffix-p "_test" triples-test-pg-database)
       (error "Refusing to drop tables in %s: the name must end in _test"
              triples-test-pg-database))
     (let ((db (triples-connect)))
       (emacsql db "DROP TABLE IF EXISTS triples CASCADE")
       (triples-close db))
     (let ((db (triples-connect)))
       (unwind-protect
           (progn ,@body)
         (triples-close db)))))

(defun triples-test-open-db ()
  "Open the database file used in the current test.
This is useful when debugging a test."
  (interactive)
  (sqlite-mode-open-file triples-test-db-file))

(defmacro triples-deftest (name _ &rest body)
  "Create a test exercising variants of `triples-database-interface'.
NAME is the name of the test, and BODY is the test code."
  (declare (debug t) (indent 2))
  (let ((builtin-name (intern (format "%s-builtin" name)))
        (emacsql-name (intern (format "%s-emacsql" name))))
    `(progn
       (ert-deftest ,builtin-name ()
         (let ((triples-database-interface 'builtin))
           (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
           ,@body))
       (ert-deftest ,emacsql-name ()
         (let ((triples-database-interface 'emacsql))
           (skip-unless (featurep 'emacsql))
           ,@body))
       (ert-deftest ,(intern (format "%s-pg" name)) ()
         (unless (getenv "TRIPLES_TEST_PG")
           (ert-skip "set TRIPLES_TEST_PG=1 to run the PostgreSQL variants"))
         (unless (and (require 'emacsql nil t) (require 'triples-psql nil t)
                      (triples-psql-available-p))
           (ert-skip "emacsql and the psql program are not available"))
         (triples-test-with-pg-db ,@body)))))

(provide 'triples-test-utils)

;;; triples-test-utils.el ends here
