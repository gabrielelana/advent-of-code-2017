;;; -*- lexical-binding: t -*-
(require 'ert)

;;; from advent-of-code-2017-10.el
(defun swap-in-list (l n m)
  (let (v1 v2)
    (setq v1 (nth (mod n (length l)) l)
          v2 (nth (mod m (length l)) l))
    (setf (nth (mod n (length l)) l) v2
          (nth (mod m (length l)) l) v1))
  l)

(defun reverse-slice (l start length)
  (while (> length 1)
    (setq l (swap-in-list l start (+ start (1- length)))
          start (1+ start)
          length (- length 2)))
  l)

(defun knot-hash-round (lengths hash &optional current-position skip-size)
  (unless current-position (setq current-position 0))
  (unless skip-size (setq skip-size 0))
  (setq hash (seq-reduce (lambda (l length)
                           (setq l (reverse-slice l current-position length)
                                 current-position (+ current-position length skip-size)
                                 skip-size (1+ skip-size))
                           l)
                         lengths
                         hash))
  (list hash current-position skip-size))

(defun string-to-bytes (s)
  (thread-last
      (split-string s "" t)
    (seq-map 'string-to-char)))

(defun knot-hash-lengths (s)
  (append (string-to-bytes s) '(17 31 73 47 23)))

(defun knot-hash-rounds (n lengths hash)
  (let ((current-position 0) (skip-size 0) result)
    (dotimes (i 64 i)
      (setq result (knot-hash-round lengths hash current-position skip-size)
            hash (nth 0 result)
            current-position (nth 1 result)
            skip-size (nth 2 result))))
  hash)

(defun knot-hash-reduce (sparse-hash)
  (seq-map (lambda (block)
             (seq-reduce 'logxor (cdr block) (car block)))
           (seq-partition sparse-hash 16)))

(defun knot-hash-canonicalize (dense-hash)
  (thread-last dense-hash
    (seq-map (lambda (e) (format "%02x" e)))
    (apply #'concat)))

(defun knot-hash (s)
  (thread-last (number-sequence 0 255)
    (knot-hash-rounds 64 (knot-hash-lengths s))
    (knot-hash-reduce)
    (knot-hash-canonicalize)))

;;; today's functions
(defun hex-to-binary-string (hex-number)
  (let ((digits 1)
        (n (string-to-number hex-number 16))
        (result ""))
    (while (<= digits 4)
      (setq result (concat (if (= 1 (logand n 1)) "1" "0") result)
            n (lsh n -1)
            digits (1+ digits)))
    result))

(ert-deftest hex-to-binary-string-test ()
  (should (equal (hex-to-binary-string "0") "0000"))
  (should (equal (hex-to-binary-string "1") "0001"))
  (should (equal (hex-to-binary-string "2") "0010"))
  (should (equal (hex-to-binary-string "4") "0100"))
  (should (equal (hex-to-binary-string "8") "1000"))
  (should (equal (hex-to-binary-string "f") "1111")))

(defun hex-string-to-binary-string (hex-string)
  (apply #'concat (seq-map #'hex-to-binary-string (split-string hex-string "" t))))

(defun string-count (string character)
  (seq-count (lambda (c)
               (equal c character))
             (split-string string "" t)))

(defun seq-sum (sequence)
  (seq-reduce '+ sequence 0))

(defun rows ()
  (let ((seed "oundnydw"))
    (seq-map (lambda (i) (format "%s-%d" seed i))
             (number-sequence 0 127))))

;;; first question
(thread-last (rows)
  (seq-map #'knot-hash)
  (seq-map #'hex-string-to-binary-string)
  (seq-map (lambda (string) (string-count string "1")))
  (seq-sum))

(defun reduce-rows-with-coordinates (reducer initial-value rows)
  (let ((x 0) (y 0))
    (seq-reduce (lambda (accumulator row)
                  (let ((result))
                    (setq result (seq-reduce (lambda (accumulator element)
                                               (let ((result))
                                                 (setq result (funcall reducer accumulator element (cons x y))
                                                       x (1+ x))
                                                 result))
                                             row
                                             accumulator)
                          x 0
                          y (1+ y))
                    result))
                rows
                initial-value)))

(defun coordinates-less-p (c1 c2)
  (if (equal (car c1) (car c2))
      (if (equal (cdr c1) (cdr c2))
          t
        (< (cdr c1) (cdr c2)))
    (< (car c1) (car c2))))

(ert-deftest coordinates-less-p ()
  (should (coordinates-less-p '(0 . 0) '(1 . 0)))
  (should (coordinates-less-p '(0 . 0) '(0 . 1)))
  (should (coordinates-less-p '(0 . 0) '(0 . 0)))
  (should (not (coordinates-less-p '(1 . 0) '(0 . 0))))
  (should (not (coordinates-less-p '(0 . 1) '(0 . 0)))))

(defun make-disjoint-set (sort-predicate &optional equal-predicate)
  (setq equal-predicate (or equal-predicate #'equal))
  (cons (make-hash-table :test equal-predicate) sort-predicate))

(defun disjoint-set-insert (e s)
  (puthash e e (car s))
  s)

(defun disjoint-set-find (e1 e2 s)
  (equal (gethash e1 (car s)) (gethash e2 (car s))))

(defun disjoint-set-member-p (e s)
  (let ((beacon (list t)))
    (not (eq (gethash e (car  s) beacon) beacon))))

(defun disjoint-set--replace-values (from-value to-value s)
  (seq-do (lambda (k)
            (when (equal (gethash k (car s)) from-value)
              (puthash k to-value (car s))))
          (hash-table-keys (car s))))

(defun disjoint-set-union (e1 e2 s)
  (when (and (disjoint-set-member-p e1 s)
             (disjoint-set-member-p e2 s)
             (not (disjoint-set-find e1 e2 s)))
    (let ((e1-value (gethash e1 (car s)))
          (e2-value (gethash e2 (car s)))
          (less-p (cdr s)))
      (if (funcall less-p e1-value e2-value)
          (disjoint-set--replace-values e2-value e1-value s)
        (disjoint-set--replace-values e1-value e2-value s))))
  s)

(defun disjoint-set-union-all (e es s)
  (seq-do (lambda (ei) (disjoint-set-union e ei s)) es))

(defun disjoint-set-clusters (s)
  (seq-uniq (hash-table-values (car s))))

(defun disjoint-set-count-clusters (s)
  (seq-length (disjoint-set-clusters s)))

(ert-deftest disjoint-set-test ()
  (let ((s (make-disjoint-set #'coordinates-less-p)))
    (disjoint-set-insert '(0 . 0) s)
    (disjoint-set-insert '(1 . 0) s)
    (should (not (disjoint-set-find '(0 . 0) '(1 . 0) s)))
    (should (equal 2 (disjoint-set-count-clusters s)))
    (disjoint-set-union '(0 . 0) '(1 . 0) s)
    (should (disjoint-set-find '(0 . 0) '(1 . 0) s))
    (should (equal 1 (disjoint-set-count-clusters s)))))

(ert-deftest disjoint-set-union-between-clusters-test ()
  (let ((s (make-disjoint-set #'coordinates-less-p)))
    (disjoint-set-insert '(0 . 0) s)
    (disjoint-set-insert '(1 . 0) s)
    (disjoint-set-insert '(1 . 1) s)
    (should (equal 3 (disjoint-set-count-clusters s)))
    (disjoint-set-union '(0 . 0) '(1 . 0) s)
    (should (equal 2 (disjoint-set-count-clusters s)))
    (disjoint-set-union '(1 . 0) '(1 . 1) s)
    (should (equal 1 (disjoint-set-count-clusters s)))
    ))

(defun coordinates-of-adjacents (coordinates)
  (list (cons (1+ (car coordinates)) (cdr coordinates))
        (cons (1- (car coordinates)) (cdr coordinates))
        (cons (car coordinates) (1+ (cdr coordinates)))
        (cons (car coordinates) (1- (cdr coordinates)))))

(ert-deftest coordinates-of-adjacents-test ()
  (should (equal (coordinates-of-adjacents '(0 . 0)) '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))))

(defun insert-used-clusters (s element coordinates)
  (when (equal (char-to-string element) "1")
    (disjoint-set-insert coordinates s))
  s)

(defun union-with-adjacents (s element coordinates)
  (when (equal (char-to-string element) "1")
    (disjoint-set-union-all coordinates (coordinates-of-adjacents coordinates) s))
  s)

;;; second question
(let ((rows (thread-last (rows)
              (seq-map #'knot-hash)
              (seq-map #'hex-string-to-binary-string)))
      (s (make-disjoint-set #'coordinates-less-p)))
  (reduce-rows-with-coordinates #'insert-used-clusters s rows)
  (reduce-rows-with-coordinates #'union-with-adjacents s rows)
  (disjoint-set-count-clusters s))
