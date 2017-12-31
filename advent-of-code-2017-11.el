;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-coordinates (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (car (split-string (buffer-string) "\n" t)) "," t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun coordinates ()
  (read-coordinates (current-data-file)))

(defun step (coordinates direction)
  "Starting from some COORDINATES and a DIRECTION, retuns the new coordinates"
  (cond
   ((equal direction "ne") (cons (1+ (car coordinates)) (1- (cdr coordinates))))
   ((equal direction "e") (cons (1+ (car coordinates)) (cdr coordinates)))
   ((equal direction "se") (cons (car coordinates) (1+ (cdr coordinates))))
   ((equal direction "sw") (cons (1- (car coordinates)) (1+ (cdr coordinates))))
   ((equal direction "w") (cons (1- (car coordinates)) (cdr coordinates)))
   ((equal direction "nw") (cons (car coordinates) (1- (cdr coordinates))))
   (t coordinates)))

(ert-deftest step-test ()
  (should (equal (step '(0 . 0) "ne") '(1 . -1)))
  (should (equal (step '(0 . 0) "e") '(1 . 0)))
  (should (equal (step '(0 . 0) "se") '(0 . 1)))
  (should (equal (step '(0 . 0) "sw") '(-1 . 1)))
  (should (equal (step '(0 . 0) "w") '(-1 . 0)))
  (should (equal (step '(0 . 0) "nw") '(0 . -1))))

(defun convert-direction (direction)
  "Convert DIRECTION counter clockwise"
  (cond
   ((equal direction "n") "e")
   ((equal direction "ne") "se")
   ((equal direction "nw") "ne")
   ((equal direction "s") "w")
   ((equal direction "sw") "nw")
   ((equal direction "se") "sw")
   (t direction)))

(defun steps (coordinates directions)
  (seq-reduce #'step directions coordinates))

(defun distance (a b)
  (/ (+ (abs (- (car a) (car b)))
        (abs (- (+ (car a) (cdr a)) (car b) (cdr b)))
        (abs (- (cdr a) (cdr b))))
     2))

(ert-deftest steps-test ()
  (should (equal (steps '(0 . 0) '("ne" "ne" "ne")) '(3 . -3))))

(defun seq-prefixes (l)
  (let ((prefixes '()))
    (seq-reduce (lambda (a e)
                  (let ((prefix (append a (list e))))
                    (setq prefixes (append prefixes (list prefix)))
                    prefix))
                l
                '())
    prefixes))

(ert-deftest seq-prefixes-test ()
  (should (equal (seq-prefixes nil) nil))
  (should (equal (seq-prefixes '()) nil))
  (should (equal (seq-prefixes '(1)) '((1))))
  (should (equal (seq-prefixes '(1 2)) '((1) (1 2))))
  (should (equal (seq-prefixes '(1 2 3)) '((1) (1 2) (1 2 3)))))

;; first question
(thread-last (coordinates)
  (seq-map #'convert-direction)
  (steps '(0 . 0))
  (distance '(0 . 0)))

;; second question
(thread-last (coordinates)
  (seq-map #'convert-direction)
  (seq-prefixes)
  (seq-map (lambda (directions) (steps '(0 . 0) directions)))
  (seq-map (lambda (prefix-coordinates) (distance '(0 . 0) prefix-coordinates)))
  (seq-max))
