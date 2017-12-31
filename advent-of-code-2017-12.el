;;; -*- lexical-binding: t -*-
(require 'ert)

;; would be better to use https://en.wikipedia.org/wiki/Disjoint-set_data_structure

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun parse-line (line)
  (string-match "^\\([0-9]+\\) <-> \\(.*\\)$" line)
  (cons (string-to-number (match-string 1 line))
        (seq-map #'string-to-number (split-string (match-string 2 line) ",\\s-" t))))

(defun add-connection (net connection)
  (hash-table-put (car connection) (cdr connection) net))

(defun net ()
  (thread-last (read-lines (current-data-file))
    (seq-map #'parse-line)
    (funcall (lambda (connections) (seq-reduce #'add-connection connections (make-hash-table))))))

(defun hash-table-put (key value hashtable)
  (puthash key value hashtable)
  hashtable)

(defun hash-table-get (key hashtable)
  (gethash key hashtable))

(defun hash-table-remove (key hashtable)
  (remhash key hashtable)
  hashtable)

(defun seq-without (sequence to-remove)
  (seq-remove (lambda (e) (equal e to-remove)) sequence))

(defun seq-union (seql seqr)
  (seq-uniq (seq-concatenate 'list seql seqr)))

(defun count-in-group (program net)
  (let ((count 0)
        (so-far '())
        (connections (hash-table-get program net)))
    (while (not (seq-empty-p connections))
      (seq-each (lambda (program)
                  (setq connections (seq-without connections program))
                  (unless (seq-contains so-far program)
                    (setq count (1+ count)
                          connections (seq-union connections (hash-table-get program net))
                          so-far (cons program so-far))))
                connections))
    (cons count so-far)))

;; first question
(car (count-in-group 0 (net)))

(defun count-groups (net)
  (let ((groups '()) so-far)
    (seq-do (lambda (key)
              (unless (seq-contains so-far key)
                (let* ((group (cdr (count-in-group key net)))
                       (group-id (seq-min group)))
                  (setq so-far (seq-union so-far group)
                        groups (seq-union groups (list group-id))))))
            (hash-table-keys net))
    (seq-length groups)))

;; second question
(count-groups (net))
