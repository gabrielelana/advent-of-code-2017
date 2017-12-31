;;; -*- lexical-binding: t -*-
(require 'ert)

(defun seq-index-of (n l)
  (let ((i 0))
    (while (not (equal (nth i l) n))
      (setq i (1+ i)))
    i))

(defun make-set (&optional ks)
  (let ((s (make-hash-table :test 'equal)))
    (seq-do (lambda (k) (set-add k s)) ks)
    s))

(defun set-add (k s)
  (puthash k t s)
  s)

(defun set-member-p (k s)
  (gethash k s))

(ert-deftest set-test ()
  (should (set-member-p :a (set-add :a (make-set))))
  (should (not (set-member-p :b (set-add :a (make-set))))))

(defun reallocate (banks)
  ;; if we remove 'seq-copy things don't work maybe because the list
  ;; used as key in set changes in place during computation, but also
  ;; the test 'reallocate-test doesn't work... that's a mistery
  (let* ((banks (seq-copy banks))
         (blocks-to-reallocate (seq-max banks))
         (bank-to-reallocate (seq-index-of blocks-to-reallocate banks))
         (current-index bank-to-reallocate)
         (number-of-banks (seq-length banks)))
    (setf (nth current-index banks) 0)
    (dotimes (_ blocks-to-reallocate banks)
      (setq current-index (mod (1+ current-index) number-of-banks))
      (setf (nth current-index banks) (1+ (nth current-index banks))))))

(ert-deftest reallocate-test ()
  (should (equal (reallocate '(0 2 7 0)) '(2 4 1 2)))
  (should (equal (reallocate '(2 4 1 2)) '(3 1 2 3)))
  (should (equal (reallocate '(3 1 2 3)) '(0 2 3 4)))
  (should (equal (reallocate '(0 2 3 4)) '(1 3 4 1)))
  (should (equal (reallocate '(1 3 4 1)) '(2 4 1 2))))

(defun cycles-until-repetition (banks)
  (let ((seen-so-far (make-set banks))
        (current-banks (reallocate banks))
        (counter 1))
    (while (not (set-member-p current-banks seen-so-far))
      (setq seen-so-far (set-add current-banks seen-so-far)
            counter (1+ counter)
            current-banks (reallocate current-banks)))
    (cons current-banks counter)))

(defun cycles-until-repetition-of (banks)
  (let ((pattern (seq-copy banks))
        (current-banks (reallocate banks))
        (counter 1))
    (while (not (equal current-banks pattern))
      (setq current-banks (reallocate current-banks)
            counter (1+ counter)))
    counter))

(ert-deftest cycles-until-repetition-test ()
  (should (equal (cycles-until-repetition '(0 2 7 0)) 5)))

;; first question
(cdr (cycles-until-repetition '(0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11)))

;; second question
(thread-last '(0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11)
  (cycles-before-repetition)
  (car)
  (cycles-until-repetition-of))
