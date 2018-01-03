;;; -*- lexical-binding: t -*-
(require 'ert)

(defmacro delay (expr)
  `(lambda () ,expr))

(defun force (delayed-object)
  (funcall delayed-object))

(defmacro cons-stream (x y)
  `(cons ,x (delay ,y)))

(defmacro stream-dry-p (s)
  `(eq :halt ,s))

(defmacro with-stream (s &rest body)
  (declare (indent defun))
  `(if (stream-dry-p ,s)
       ,s
     ,@body))

(defun stream-car (s)
  (with-stream s
    (car s)))

(defun stream-cdr (s)
  (with-stream s
    (force (cdr s))))

(defun integers (&optional n)
  (setq n (or n 0))
  (cons-stream n (integers (1+ n))))

(defun stream-from-sequence (sequence)
  (if (seq-empty-p sequence)
      :halt
    (cons-stream (car sequence)
                 (stream-from-sequence (cdr sequence)))))

(defun stream-take (n s)
  (let (result (results '()))
    (while (> n 0)
      (setq result (stream-car s)
            n (1- n))
      (when (not (eq result :halt))
        (setq results (cons result results)
              s (stream-cdr s))))
    (seq-reverse results)))

(ert-deftest stream-take-test ()
  (should (equal (stream-take 0 (integers)) '()))
  (should (equal (stream-take 1 (integers)) '(0)))
  (should (equal (stream-take 2 (integers)) '(0 1)))
  (should (equal (stream-take 10 (integers)) '(0 1 2 3 4 5 6 7 8 9)))
  (should (equal (stream-take 2 (integers 1)) '(1 2))))

(defun stream-run (s)
  (let ((result (stream-car s)) (results '()))
    (while (not (eq result :halt))
      (setq results (cons result results)
            s (stream-cdr s)
            result (stream-car s)))
    (seq-reverse results)))

(ert-deftest stream-run-test ()
  (should (equal (stream-run (stream-from-sequence '(0 1 2))) '(0 1 2))))

(defun stream-limit (n s)
  (with-stream s
    (if (= n 0)
        :halt
      (cons-stream (stream-car s)
                   (stream-limit (1- n) (stream-cdr s))))))

(ert-deftest stream-limit ()
  (should (equal (stream-run (stream-limit 3 (integers))) '(0 1 2)))
  (should (equal (stream-run (stream-limit 3 (stream-limit 2 (integers)))) '(0 1))))

(defun stream-zip (s1 s2)
  (with-stream s1
    (with-stream s2
      (cons-stream (cons (stream-car s1) (stream-car s2))
                   (stream-zip (stream-cdr s1) (stream-cdr s2))))))

(ert-deftest stream-zip-test ()
  (should (equal (stream-take 2 (stream-zip (integers) (integers))) '((0 . 0) (1 . 1)))))

(defun stream-map (f s)
  (with-stream s
    (cons-stream (funcall f (stream-car s))
                 (stream-map f (stream-cdr s)))))

(ert-deftest stream-map-test ()
  (should (equal (stream-take 10 (stream-map #'1+ (integers))) '(1 2 3 4 5 6 7 8 9 10))))

(defun stream-filter (f s)
  (with-stream s
    (let ((e (stream-car s)))
      (while (and (not (stream-dry-p s)) (not (funcall f e)))
        (setq s (stream-cdr s)
              e (stream-car s)))
      (if (not (stream-dry-p e))
          (cons-stream e (stream-filter f (stream-cdr s)))
        s))))

(ert-deftest stream-filter-test ()
  (should (equal (stream-run (stream-filter #'stringp (stream-from-sequence '("a" 0 :b)))) '("a"))))

(defun generator (previous-value factor)
  (let ((next-value (mod (* previous-value factor) 2147483647)))
    (cons-stream next-value (generator next-value factor))))

(defun generator-a (initial-value)
  (generator initial-value 16807))

(defun generator-b (initial-value)
  (generator initial-value 48271))

(ert-deftest generator-test ()
  ;; from the values given in the example
  (should (equal (stream-take 5 (generator-a 65)) '(1092455 1181022009 245556042 1744312007 1352636452)))
  (should (equal (stream-take 5 (generator-b 8921)) '(430625591 1233683848 1431495498 137874439 285222916))))

(defun match-p (pair)
  (when pair
      (equal (logand #xFFFF (car pair))
             (logand #xFFFF (cdr pair)))))

(ert-deftest match-p-test ()
  (should (match-p '(245556042 . 1431495498))))

(defun stream-index (s &optional n)
  (with-stream s
    (cons-stream (or n 1) (stream-index (stream-cdr s) (1+ (or n 1))))))

(defun stream-last (s &optional last)
  (while (not (stream-dry-p s))
    (setq last (stream-car s)
          s (stream-cdr s)))
  last)

(defun stream-inspect (s)
  (with-stream s
    (let ((e (stream-car s)))
      (message "%S" e)
      (cons-stream e (stream-inspect (stream-cdr s))))))

;;; first question
(thread-last (stream-zip (generator-a 277) (generator-b 349))
  (stream-limit 40000000)
  (stream-filter #'match-p)
  (stream-index)
  (stream-inspect)
  (stream-last))

(defun stream-filter-mod (n g)
  (stream-filter (lambda (e) (= 0 (mod e n))) g))

;;; second question
(thread-last (stream-zip
              (stream-filter-mod 4 (generator-a 277))
              (stream-filter-mod 8 (generator-b 349)))
  (stream-limit 5000000)
  (stream-filter #'match-p)
  (stream-index)
  (stream-inspect)
  (stream-last))
