;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun string-to-list-of-characters (string)
  (split-string string "" t))

(defun string-to-numbers (string)
  (mapcar #'string-to-number (string-to-list-of-characters string)))

(defun captcha ()
  (string-to-numbers (read-file (current-data-file))))

(defun seq-sum (l)
  (seq-reduce #'+ l 0))

(defun seq-zip (ll lr)
  (let ((l))
    (while (and (not (seq-empty-p ll)) (not (seq-empty-p lr)))
      (setq l (cons (cons (car ll) (car lr)) l)
            ll (cdr ll)
            lr (cdr lr)))
    (seq-reverse l)))

(defun rotate-left (l)
  (if (seq-empty-p l)
      l
    (append (cdr l) (list (car l)))))

(ert-deftest rotate-left-test ()
  (should (equal (rotate-left '(1 2 3 4)) '(2 3 4 1)))
  (should (equal (rotate-left '(2 3 4 1)) '(3 4 1 2)))
  (should (equal (rotate-left '(3 4 1 2)) '(4 1 2 3)))
  (should (equal (rotate-left '(4 1 2 3)) '(1 2 3 4)))
  (should (equal (rotate-left '(1)) '(1)))
  (should (equal (rotate-left '()) '())))

(defun rotate-left-n (l n)
  (let ((rotated l))
    (dotimes (i n rotated)
      (setq rotated (rotate-left rotated)))))

(ert-deftest rotate-left-n-test ()
  (should (equal (rotate-left-n '(1 2 3 4) 1) '(2 3 4 1)))
  (should (equal (rotate-left-n '(1 2 3 4) 2) '(3 4 1 2)))
  (should (equal (rotate-left-n '(1 2 3 4) 3) '(4 1 2 3)))
  (should (equal (rotate-left-n '(1 2 3 4) 4) '(1 2 3 4)))
  (should (equal (rotate-left-n '(1 2 3 4) 5) '(2 3 4 1)))
  (should (equal (rotate-left-n '(1 2 3 4) 9) '(2 3 4 1))))

(defun equal-pair-p (pair)
  (equal (car pair) (cdr pair)))

;; first question
(thread-last (seq-zip (captcha) (rotate-left (captcha)))
  (seq-filter #'equal-pair-p)
  (seq-map #'car)
  (seq-sum))

;; second question
(let* ((captcha (captcha))
       (middle-of-captcha (/ (length captcha) 2)))
  (thread-last (seq-zip captcha (rotate-left-n captcha middle-of-captcha))
    (seq-filter #'equal-pair-p)
    (seq-map #'car)
    (seq-sum)))
