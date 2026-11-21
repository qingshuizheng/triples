;;; triples-psql.el --- PostgreSQL backend for triples through psql  -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The `pg' interface talks to PostgreSQL through the `psql' command line
;; program, not through `pg.el'.
;;
;; `pg.el' keeps a stateful connection inside Emacs.  Any timer or process
;; filter that runs while Emacs is inside `accept-process-output' waiting for a
;; reply can send another statement on that same connection, and the replies
;; interleave: the next statement then reads the previous one's rows.  sqlite
;; tolerates that; PostgreSQL does not.  In ekg it showed up as "Property
;; tagged/tag not found in schema" on save, and as timeouts after that.
;;
;; `psql' moves the connection into a child process, but it is still one
;; session with one reply stream, so it has the same hazard.  This backend
;; closes that hole instead of hoping it does not happen: a statement issued
;; while another one on the same connection is in flight -- which can only be
;; seen from inside `accept-process-output', i.e. from a timer or a process
;; filter -- is run on its own short-lived `psql' process.  The session's reply
;; stream is never touched while it is waiting, and the nested statement gets a
;; real answer rather than being dropped.  Such a statement is, by
;; construction, outside any transaction that is open on the session; that is
;; what ekg's asynchronous writers want, and a lock timeout keeps it from
;; deadlocking against the transaction it cannot see.
;;
;; Everything else -- compiling the emacsql s-expressions, quoting values,
;; reading them back -- is emacsql's own work, so the rest of triples does not
;; know which program is underneath.

;;; Code:

(require 'emacsql)
(require 'emacsql-psql)
(require 'subr-x)

(defvar triples-pg-connection-spec)     ; Defined in triples.el, which loads us.

(defvar triples-psql-program "psql"
  "The psql executable used for the `pg' interface.")

(defconst triples-psql--prompt "triples-psql-ready>"
  "Prompt that tells a finished reply from a paused one.
`emacsql-psql' uses a literal \"]\", which is fine for small results but not
for a multi-megabyte one: the wait would end as soon as any streamed chunk
happened to end in \"]\", and the truncated reply then fails to parse.  This
token is long enough that only psql itself puts it at the end, and the parser
looks for its last occurrence, so a value containing it is harmless.")

(defvar triples-psql--in-flight nil
  "Connections that have a statement in flight, for detouring nested ones.
Dynamically bound to a list of connection objects while `emacsql' runs a
statement on them.  A statement that arrives for a connection on this list
cannot use that connection's session; see `triples-psql--emacsql'.")

(defun triples-psql-available-p ()
  "Return non-nil if the `psql' program can be found."
  (and (executable-find triples-psql-program) t))

(defun triples-psql--password (spec)
  "Return the password from SPEC, calling it if it is a function."
  (let ((password (plist-get spec :password)))
    (cond ((functionp password) (funcall password))
          (t password))))

(defclass triples-psql-connection (emacsql-psql-connection)
  ((spec :accessor triples-psql-spec :initarg :spec
         :documentation "The connection plist this connection was opened from."))
  "A `pg' connection to PostgreSQL through a `psql' child process.")

(cl-defun triples-psql-connect (&optional spec)
  "Open a `psql' session for SPEC.
SPEC is a plist as in `triples-pg-connection-spec'; nil means that variable.
Return the connection object.  The session speaks emacsql's psql protocol:
unaligned, tuples only, one row per line, values printed for `read'.  The
prompt is `triples-psql--prompt' rather than `emacsql-psql's \"]\", because a
long reply can contain that character and would be mistaken for finished."
  (let* ((spec (or spec triples-pg-connection-spec))
         (database (plist-get spec :database))
         (user (plist-get spec :user))
         (host (or (plist-get spec :host) "localhost"))
         (port (or (plist-get spec :port) 5432))
         (password (triples-psql--password spec)))
    (unless (and (stringp database) (not (string-empty-p database)))
      (error "triples: the `pg' connection spec needs a :database string, got %S"
             (plist-get spec :database)))
    (unless (and (stringp user) (not (string-empty-p user)))
      (error "triples: the `pg' connection spec needs a :user string, got %S"
             (plist-get spec :user)))
    (let* ((process-environment
            (if password
                (cons (format "PGPASSWORD=%s" password) process-environment)
              process-environment))
           (args (list "-X" "-q" "-n"
                       "-d" database
                       "-U" user
                       "-h" host
                       "-p" (number-to-string port)))
           (command (mapconcat #'shell-quote-argument
                               (cons triples-psql-program args) " "))
           (buffer (generate-new-buffer " *triples-psql*"))
           ;; A raw terminal keeps the pty from echoing what we send; the
           ;; replies are what `emacsql-psql' parses, not the input.
           (process (start-process-shell-command
                     "triples-psql" buffer (concat "stty raw && " command)))
           (connection (make-instance 'triples-psql-connection
                                      :handle process
                                      :dbname database
                                      :spec spec)))
      (set-process-query-on-exit-flag process nil)
      (mapc (apply-partially #'emacsql-send-message connection)
            (list "\\pset pager off"
                  "\\pset null nil"
                  "\\a"
                  "\\t"
                  "\\f ' '"
                  ;; NOTICEs (from every `IF NOT EXISTS' in the setup code)
                  ;; would otherwise land in the reply buffer and be read as
                  ;; rows.
                  "SET client_min_messages TO ERROR;"
                  (concat "\\set PROMPT1 " triples-psql--prompt)))
      ;; The startup lines produce prompts but no rows, so waiting for the
      ;; first prompt is enough; the first `emacsql' clears the buffer anyway.
      (emacsql-wait connection)
      (emacsql connection [:set (= default-transaction-isolation 'SERIALIZABLE)])
      (emacsql-register connection))))

(cl-defmethod emacsql-close ((connection triples-psql-connection))
  "Close CONNECTION's psql session and discard its buffer."
  (let* ((process (oref connection handle))
         (buffer (and process (process-buffer process))))
    (when (process-live-p process)
      (ignore-errors (process-send-string process "\\q\n")))
    (when (buffer-live-p buffer)
      (ignore-errors (delete-process process))
      (ignore-errors (kill-buffer buffer)))))

(cl-defmethod emacsql-waiting-p ((connection triples-psql-connection))
  "Return non-nil when `triples-psql--prompt' ends the reply.
Signal the server's message instead when psql has died."
  (unless (process-live-p (oref connection handle))
    (error "triples: the psql process exited: %s"
           (string-trim
            (with-current-buffer (emacsql-buffer connection)
              (buffer-string)))))
  (with-current-buffer (emacsql-buffer connection)
    (let ((end (point-max))
          (n (length triples-psql--prompt)))
      (and (>= (buffer-size) n)
           (string= triples-psql--prompt
                    (buffer-substring (- end n) end))))))

(cl-defmethod emacsql-parse ((connection triples-psql-connection))
  "Parse the reply of CONNECTION, up to the prompt.
The inherited parser stops at a literal \"]\" because that is emacsql's psql
prompt.  Here the prompt is `triples-psql--prompt', and its last occurrence is
taken as the end of the reply, so a value that happens to contain it is not
mistaken for the end."
  (emacsql-check-error connection)
  (with-current-buffer (emacsql-buffer connection)
    (goto-char (point-max))
    (unless (search-backward triples-psql--prompt nil t)
      (error "triples: psql prompt missing from the reply"))
    (let ((end (point))
          (rows nil)
          (row nil))
      (goto-char (point-min))
      (while (< (point) end)
        (if (looking-at "\n")
            (progn
              (push (nreverse row) rows)
              (setq row nil)
              (forward-char 1))
          (push (read (current-buffer)) row)))
      (when row (push (nreverse row) rows))
      (nreverse rows))))

(cl-defmethod emacsql :around ((connection triples-psql-connection) sql &rest args)
  "Run SQL on CONNECTION, detouring a statement that cannot share its session.
A psql session has one reply stream, so two statements must not overlap.  When
one is already waiting, the caller can only be a timer or a process filter
running inside `accept-process-output'; waiting for the session would deadlock
it against the statement it interrupted, so it gets a separate, short-lived
connection instead."
  (if (memq connection triples-psql--in-flight)
      (triples-psql--detached connection sql args)
    (let ((triples-psql--in-flight (cons connection triples-psql--in-flight)))
      (cl-call-next-method))))

(defun triples-psql--detached (connection sql args)
  "Run SQL with ARGS for CONNECTION on a separate, short-lived connection.
The statement cannot see a transaction that is open on CONNECTION, so it is
given a lock timeout: failing is better than deadlocking against the
transaction that is waiting for this very call to return."
  (let ((detached (triples-psql-connect (triples-psql-spec connection))))
    (unwind-protect
        (progn
          (emacsql detached "SET lock_timeout = '2s'")
          (apply #'emacsql detached sql args))
      (ignore-errors (emacsql-close detached)))))

(provide 'triples-psql)
;;; triples-psql.el ends here
