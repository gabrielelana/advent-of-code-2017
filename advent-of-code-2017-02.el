;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun parse-line (line)
  (seq-map #'string-to-number (split-string line "\\s-" t)))

(defun spreadsheet ()
  (seq-map #'parse-line (read-lines (current-data-file))))

(defun seq-sum (l)
  (seq-reduce '+ l 0))

(defun biggest-difference (l)
  (- (seq-max l) (seq-min l)))

(defun checksum (spreadsheet)
  (seq-sum (seq-map 'biggest-difference spreadsheet)))

;; first question
(checksum (spreadsheet))

(defun ordered-pairs (l)
  (let (result (i 1))
    (dolist (n l result)
      (dolist (m (nthcdr i l) result)
        (if (> n m)
            (setq result (cons (cons m n) result))
          (setq result (cons (cons n m) result))))
      (setq i (1+ i)))))

(ert-deftest pairs-test ()
  (should (equal (ordered-pairs '(1 2)) '((1 . 2))))
  (should (equal (ordered-pairs '(1 2 3)) '((2 . 3) (1 . 3) (1 . 2))))
  (should (equal (ordered-pairs '(1)) '()))
  (should (equal (ordered-pairs '()) '())))

(defun pair-evenly-divisible-p (pair)
  (equal 0 (mod (cdr pair) (car pair))))

(defun pair-evenly-divisible (l)
  (car (seq-filter #'pair-evenly-divisible-p (ordered-pairs l))))

(defun pair-divide (pair)
  (/ (cdr pair) (car pair)))

;; second question
(thread-last (spreadsheet)
  (seq-map #'pair-evenly-divisible)
  (seq-map #'pair-divide)
  (seq-sum))
