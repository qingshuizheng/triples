;;; triples-psql-prototype.el --- run emacsql statements through psql -*- lexical-binding: t; -*-

;;; WORK IN PROGRESS.  It loads and runs, but the argument plumbing is not
;;; finished: `TRIPLES_TEST_PG=1' still fails with (wrong-type-argument stringp
;;; nil) on the connection spec, so this is a saved draft, not a backend yet.
;;
;; Prototype: keep `triples' and `emacsql' as they are, but do the actual talking
;; to PostgreSQL with a short-lived `psql' process instead of `pg.el', which
;; keeps a stateful in-process connection that a timer running inside
;; `accept-process-output' can desynchronise.
;;
;; Load this file after `triples' and it takes over every emacsql statement on
;; the pg interface.  Nothing else changes: the SQL is compiled by emacsql
;; itself, so all the literal-quoting work stays where it is.

;;; Code:

(defvar triples-psql-program "psql"
  "The psql executable.")

(defvar triples-psql--count 0
  "How many statements went through psql (for the prototype's reporting).")

(defun triples-psql--csv-fields (line)
  "Split one CSV LINE into fields, honouring doubled quotes."
  (let ((fields nil) (field "") (i 0) (n (length line)) (in-quotes nil))
    (while (< i n)
      (let ((ch (aref line i)))
        (cond
         (in-quotes
          (if (and (eq ch ?\") (< (1+ i) n) (eq (aref line (1+ i)) ?\"))
              (progn (setq field (concat field "\"")) (setq i (1+ i)))
            (if (eq ch ?\") (setq in-quotes nil) (setq field (concat field (char-to-string ch))))))
         ((eq ch ?\") (setq in-quotes t))
         ((eq ch ?,) (push field fields) (setq field ""))
         (t (setq field (concat field (char-to-string ch))))))
      (setq i (1+ i)))
    (push field fields)
    (nreverse fields)))

(defun triples-psql--parse (text)
  "Turn psql's --csv output TEXT into a list of rows of Lisp values.
Values are read the way `emacsql-parse' reads them, so the rest of triples sees
exactly what it saw with pg.el."
  (let ((lines (split-string text "\n" t)))
    (mapcar (lambda (line)
              (mapcar (lambda (field)
                        (if (string-empty-p field) nil (car (read-from-string field))))
                      (triples-psql--csv-fields line)))
            lines)))

(defun triples-psql-execute (sql)
  "Run SQL through psql and return the rows as Lisp values.
Signals an error when psql reports one, so the caller sees what pg.el would have
raised."
  (setq triples-psql--count (1+ triples-psql--count))
  (let* ((spec triples-pg-connection-spec)
         (out (get-buffer-create " *triples-psql*"))
         (err (get-buffer-create " *triples-psql-err*"))
         (process-environment
          (if-let ((password (plist-get spec :password)))
              (cons (format "PGPASSWORD=%s" password) process-environment)
            process-environment))
         (args (list "-X" "-q" "--csv" "-v" "ON_ERROR_STOP=1"
                     "-d" (plist-get spec :database)
                     "-U" (plist-get spec :user)
                     "-h" (or (plist-get spec :host) "localhost")
                     "-p" (number-to-string (or (plist-get spec :port) 5432))
                     "-c" sql))
         (status (progn
                   (with-current-buffer out (erase-buffer))
                   (with-current-buffer err (erase-buffer))
                   (apply #'call-process triples-psql-program nil out err nil args))))
    (unless (eq status 0)
      (error "psql failed (%s): %s" status
             (string-trim (with-current-buffer err (buffer-string)))))
    (with-current-buffer out
      (triples-psql--parse (buffer-string)))))

(defun triples-psql--emacsql (orig connection sql &rest args)
  "Run the statement through psql when the pg interface is active.
ORIG, CONNECTION, SQL and ARGS are as in `emacsql'."
  (if (eq triples-database-interface 'pg)
      (triples-psql-execute (apply #'emacsql-compile connection sql args))
    (apply orig connection sql args)))

(advice-add 'emacsql :around #'triples-psql--emacsql)

(provide 'triples-psql-prototype)
;;; triples-psql-prototype.el ends here
