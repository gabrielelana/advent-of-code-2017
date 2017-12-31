;;; -*- lexical-binding: t -*-
(require 'ert)

(defun seq-index-of (e l)
  (let (index)
    (dotimes (i (length l) index)
      (when (equal (nth i l) e)
        (setq index i)))))

(defun nth-odd-number (n)
  (+ 1 (* (- n 1) 2)))

(defun ring-sequence (n)
  (let ((start-at (+ 1 (expt (nth-odd-number n) 2)))
        (end-at (expt (nth-odd-number (+ 1 n)) 2)))
    (number-sequence start-at end-at)))

(defun ring-number (n)
  (ceiling (- (sqrt (/ n 4.0)) 0.5)))

(defun ring-side-length (n)
  (+ (* n 2) 1))

(defun ring-length (n)
  (- (* (ring-side-length n) 4) 4))

(defun ring-distances (n)
  (let ((distances `()) (distance (+ n (- n 1))) (direction #'-))
    (dotimes (e (ring-length n) (reverse distances))
      (setq distances (cons distance distances))
      (setq distance (apply direction `(,distance, 1)))
      (when (equal distance n) (setq direction #'+))
      (when (equal distance (* n 2)) (setq direction #'-)))))

(defun distance (n)
  (let* ((ring-number (ring-number n))
         (ring-sequence (ring-sequence ring-number))
         (position-in-ring (seq-index-of n ring-sequence)))
    (nth position-in-ring (ring-distances ring-number))))

;; first question
(distance 265149)

;; second question
;; look at https://oeis.org/A141481/b141481.txt
