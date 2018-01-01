;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun firewall ()
  (seq-map (lambda (line)
             (let ((tokens (split-string line ":")))
               (cons (string-to-number (car tokens))
                     (string-to-number (cadr tokens)))))
           (read-lines (current-data-file))))

(defun period-of-range (n)
  (+ (* (- n 2) 2) 2))

(ert-deftest period-of-range-test ()
  (should (equal (period-of-range 3) 4))
  (should (equal (period-of-range 4) 6)))

(defun caught-p (dept range)
  (equal (mod dept (period-of-range range)) 0))

(ert-deftest caught-p-test ()
  (should (equal (caught-p 0 3) t))
  (should (equal (caught-p 1 2) nil))
  (should (equal (caught-p 4 4) nil))
  (should (equal (caught-p 6 4) t)))

(defun penalty (dept range)
  (* dept range))

(defun step (scanner)
  (if (caught-p (car scanner) (cdr scanner))
      (penalty (car scanner) (cdr scanner))
    0))

(defun seq-sum (sequence)
  (seq-reduce #'+ sequence 0))

(defun severity-of-trip-through (firewall)
  (thread-last (seq-copy firewall)
    (seq-map #'step)
    (seq-sum)))

;;; first question
(severity-of-trip-through (firewall))

(defun delay-n (n firewall)
  (seq-map (lambda (scanner)
             (cons (+ n (car scanner)) (cdr scanner)))
           firewall))

;;; second question
(let ((firewall (firewall))
      (n 1))
  (while (> (severity-of-trip-through (delay-n n firewall)) 0)
    (setq n (1+ n)))
  n)
