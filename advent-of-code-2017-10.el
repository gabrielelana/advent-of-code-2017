;;; -*- lexical-binding: t -*-
(require 'ert)

(defun swap-in-list (l n m)
  (let (v1 v2)
    (setq v1 (nth (mod n (length l)) l)
          v2 (nth (mod m (length l)) l))
    (setf (nth (mod n (length l)) l) v2
          (nth (mod m (length l)) l) v1))
  l)

(ert-deftest swap-in-list-test ()
  (should (equal (swap-in-list '(0 1) 0 1) '(1 0)))
  (should (equal (swap-in-list '(0 1 2) 1 2) '(0 2 1)))
  (should (equal (swap-in-list '(0 1 2 3 4) 0 3) '(3 1 2 0 4)))
  (should (equal (swap-in-list '(0 1 2 3 4) 2 8) '(0 1 3 2 4))))

(defun reverse-slice (l start length)
  (while (> length 1)
    (setq l (swap-in-list l start (+ start (1- length)))
          start (1+ start)
          length (- length 2)))
  l)

(ert-deftest reverse-slice-test ()
  (should (equal (reverse-slice '(0 1 2 3 4) 0 1) '(0 1 2 3 4)))
  (should (equal (reverse-slice '(0 1 2 3 4) 0 2) '(1 0 2 3 4)))
  (should (equal (reverse-slice '(0 1 2 3 4) 0 3) '(2 1 0 3 4)))
  (should (equal (reverse-slice '(0 1 2 3 4) 0 4) '(3 2 1 0 4)))
  (should (equal (reverse-slice '(0 1 2 3 4) 0 5) '(4 3 2 1 0)))
  (should (equal (reverse-slice '(0 1 2 3 4) 3 3) '(3 1 2 0 4)))
  (should (equal (reverse-slice '(0 1 2 3 4) 3 4) '(4 3 2 1 0)))
  (should (equal (reverse-slice '(2 1 0 3 4) 3 4) '(4 3 0 1 2)))
  )

(defun seq-prod (l)
  (seq-reduce '* l 1))

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

(defun knot-hash-checksum (hash)
  (thread-first hash (car) (seq-take 2) (seq-prod)))

;; example
(knot-hash-checksum (knot-hash-round '(3 4 1 5) (number-sequence 0 4)))

;; first question
(knot-hash-checksum (knot-hash-round '(206 63 255 131 65 80 238 157 254 24 133 2 16 0 1 3)
                                     (number-sequence 0 255)))

;; First, from now on, your input should be taken not as a list of
;; numbers, but as a string of bytes instead. Unless otherwise
;; specified, convert characters to bytes using their ASCII
;; codes. This will allow you to handle arbitrary ASCII strings, and
;; it also ensures that your input lengths are never larger than
;; 255. For example, if you are given 1,2,3, you should convert it
;; to the ASCII codes for each character: 49,44,50,44,51.

(defun string-to-bytes (s)
  (thread-last
      (split-string s "" t)
    (seq-map 'string-to-char)))

(string-to-bytes "1,2,3")

;; Once you have determined the sequence of lengths to use, add the
;; following lengths to the end of the sequence: 17, 31, 73, 47,
;; 23. For example, if you are given 1,2,3, your final sequence of
;; lengths should be 49,44,50,44,51,17,31,73,47,23 (the ASCII codes
;; from the input string combined with the standard length suffix
;; values).

(defun knot-hash-lengths (s)
  (append (string-to-bytes s) '(17 31 73 47 23)))

(knot-hash-lengths "1,2,3")

;; Second, instead of merely running one round like you did above,
;; run a total of 64 rounds, using the same length sequence in each
;; round. The current position and skip size should be preserved
;; between rounds. For example, if the previous example was your
;; first round, you would start your second round with the same
;; length sequence (3, 4, 1, 5, 17, 31, 73, 47, 23, now assuming
;; they came from ASCII codes and include the suffix), but start
;; with the previous round's current position (4) and skip size (4).

(defun knot-hash-rounds (n lengths hash)
  (let ((current-position 0) (skip-size 0) result)
    (dotimes (i 64 i)
      (setq result (knot-hash-round lengths hash current-position skip-size)
            hash (nth 0 result)
            current-position (nth 1 result)
            skip-size (nth 2 result))))
  hash)

;; Once the rounds are complete, you will be left with the numbers
;; from 0 to 255 in some order, called the sparse hash. Your next
;; task is to reduce these to a list of only 16 numbers called the
;; dense hash. To do this, use numeric bitwise XOR to combine each
;; consecutive block of 16 numbers in the sparse hash (there are 16
;; such blocks in a list of 256 numbers). So, the first element in
;; the dense hash is the first sixteen elements of the sparse hash
;; XOR'd together, the second element in the dense hash is the
;; second sixteen elements of the sparse hash XOR'd together, etc.

(defun knot-hash-reduce (sparse-hash)
  (seq-map (lambda (block)
             (seq-reduce 'logxor (cdr block) (car block)))
           (seq-partition sparse-hash 16)))

;; Finally, the standard way to represent a Knot Hash is as a single
;; hexadecimal string; the final output is the dense hash in
;; hexadecimal notation. Because each number in your dense hash will
;; be between 0 and 255 (inclusive), always represent each number as
;; two hexadecimal digits (including a leading zero as
;; necessary). So, if your first three numbers are 64, 7, 255, they
;; correspond to the hexadecimal numbers 40, 07, ff, and so the
;; first six characters of the hash would be 4007ff. Because every
;; Knot Hash is sixteen such numbers, the hexadecimal representation
;; is always 32 hexadecimal digits (0-f) long.

(defun knot-hash-canonicalize (dense-hash)
  (thread-last dense-hash
    (seq-map (lambda (e) (format "%02x" e)))
    (apply #'concat)))

(defun knot-hash (s)
  (thread-last (number-sequence 0 255)
    (knot-hash-rounds 64 (knot-hash-lengths s))
    (knot-hash-reduce)
    (knot-hash-canonicalize)))

;; The empty string becomes a2582a3a0e66e6e86e3812dcb672a272.
;; AoC 2017 becomes 33efeb34ea91902bb2f59c9920caa6cd.
;; 1,2,3 becomes 3efbe78a8d82f29979031a4aa0b16a9d.
;; 1,2,4 becomes 63960835bcdc130f0b66d7ff4f6a5a8e.

(ert-deftest knot-hash-test ()
  (should (equal (knot-hash "") "a2582a3a0e66e6e86e3812dcb672a272"))
  (should (equal (knot-hash "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd"))
  (should (equal (knot-hash "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d"))
  (should (equal (knot-hash "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e")))

;; second question
(knot-hash "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3")
