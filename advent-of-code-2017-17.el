;;; -*- lexical-binding: t -*-
(require 'ert)

(defun seq-splice (elt position sequence)
  (seq-concatenate 'list (seq-take sequence (1+ position)) (list elt) (seq-drop sequence (1+ position))))

(ert-deftest seq-splice ()
  (should (equal (seq-splice :x 0 '(0)) '(0 :x)))
  (should (equal (seq-splice :x 0 '(0 1)) '(0 :x 1)))
  (should (equal (seq-splice :x 1 '(0 1 2)) '(0 1 :x 2)))
  (should (equal (seq-splice :x 2 '(0 1 2)) '(0 1 2 :x))))

(defun make-spinlock (n)
  (list :index 0
        :values '(0)
        :steps n))

(defun spinlock--field-get (spinlock field-name)
  (plist-get spinlock field-name))

(defun spinlock--field-put (spinlock field-name field-value)
  (plist-put spinlock field-name field-value))

(defun spinlock-spin (spinlock)
  (let* ((index (spinlock--field-get spinlock :index))
         (values (spinlock--field-get spinlock :values))
         (steps (spinlock--field-get spinlock :steps))
         (length (seq-length values)))
    (setq index (mod (+ index steps) length))
    (spinlock--field-put spinlock :values (seq-splice length index values))
    (spinlock--field-put spinlock :index (1+ index)))
  spinlock)

(ert-deftest spinlock-spin-test ()
  (let ((spinlock (make-spinlock 3)))
    (spinlock-spin spinlock)
    (should (equal (spinlock--field-get spinlock :values) '(0 1)))
    (spinlock-spin spinlock)
    (should (equal (spinlock--field-get spinlock :values) '(0 2 1)))
    (spinlock-spin spinlock)
    (should (equal (spinlock--field-get spinlock :values) '(0 2 3 1)))
    (spinlock-spin spinlock)
    (should (equal (spinlock--field-get spinlock :values) '(0 2 4 3 1)))))

(defun spinlock-value-at (spinlock position)
  (let ((values (spinlock--field-get spinlock :values)))
    (nth (mod position (seq-length values)) values)))

(defun spinlock-next-value (spinlock)
  (spinlock-value-at spinlock (1+ (spinlock--field-get spinlock :index))))

(defun spinlock-next-value-of (spinlock value)
  (let ((position (seq-position (spinlock--field-get spinlock :values) value)))
    (spinlock-value-at spinlock (1+ position))))

;;; first question
(let ((spinlock (make-spinlock 355)))
  (dotimes (_ 2017 (spinlock-next-value spinlock))
    (spinlock-spin spinlock)))

;;; the following should have been the answer to the second question
;;; but unfortunately it's too slow, if you have a few weeks you can try...
;;; this is a valuable solution for other languages but not for elisp so...
(let ((spinlock (make-spinlock 355)))
  (dotimes (i 50000000 (spinlock-next-value-of spinlock 0))
    (spinlock-spin spinlock)))

;;; second question
;;; the question is "what's the number following `0` in the spinlock?"
;;; since `0` can only be the initial number in the spinlock (the number
;;; inserted are incrementing integers starting from 1) and it will be
;;; always at position `0` then what we want is the number in position `1`
;;; and it's the only thing that we should care about
(let ((steps 355)
      (current-index 0)
      (current-value 0)
      after-zero)
  (dotimes (n 50000000 after-zero)
    (setq current-value (1+ n)
          current-index (1+ (mod (+ current-index steps) current-value)))
    (when (= current-index 1)
      (setq after-zero current-value))))
