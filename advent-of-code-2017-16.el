;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-dance-moves (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (car (split-string (buffer-string) "\n" t)) "," t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun moves ()
  (seq-map #'parse-move (read-dance-moves (current-data-file))))

(defun parse-move (move)
  (string-match "^\\([sxp]\\)\\(?:\\([0-9]+\\)\\|\\([0-9]+\\)/\\([0-9]+\\)\\|\\([a-p]\\)/\\([a-p]\\)\\)?$" move)
  (pcase (match-string 1 move)
    (`"s" (list "s" (string-to-number (match-string 2 move))))
    (`"x" (list "x" (string-to-number (match-string 3 move)) (string-to-number (match-string 4 move))))
    (`"p" (list "p" (intern (match-string 5 move)) (intern (match-string 6 move))))))

(defun move (programs move)
  (pcase move
    (`("s" ,n) (spin programs n))
    (`("x" ,a ,b) (exchange programs a b))
    (`("p" ,c ,d) (partner programs c d))))

(defun spin (l n)
  "Makes N programs move from the end to the front, but maintain their order otherwise."
  (append
   (seq-drop l (- (seq-length l) n))
   (seq-take l (- (seq-length l) n))))

(ert-deftest spin-test ()
  (should (equal (spin '(0 1 2 3 4) 1) '(4 0 1 2 3)))
  (should (equal (spin '(0 1 2 3 4) 2) '(3 4 0 1 2)))
  (should (equal (spin '(0 1 2 3 4) 3) '(2 3 4 0 1)))
  (should (equal (spin '(0 1 2 3 4) 4) '(1 2 3 4 0))))

(defun exchange (l a b)
  "Makes the programs at positions A and B swap places."
  (let ((va (nth a l))
        (vb (nth b l)))
    (setf (nth a l) vb
          (nth b l) va))
  l)

(ert-deftest exchange-test ()
  (should (equal (exchange '(0 1 2 3 4) 0 3) '(3 1 2 0 4)))
  (should (equal (exchange '(0 1 2 3 4) 0 4) '(4 1 2 3 0)))
  (should (equal (exchange '(0 1 2 3 4) 1 4) '(0 4 2 3 1)))
  (should (equal (exchange '(a b c d e) 1 4) '(a e c d b))))

(defun partner (l a b)
  "Makes the programs named A and B swap places."
  (let ((pa (seq-position l a))
        (pb (seq-position l b)))
    (exchange l pa pb)))

(ert-deftest partner-test ()
  (should (equal (partner '(0 1 2 3 4) 0 4) '(4 1 2 3 0)))
  (should (equal (partner '(0 1 2 3 4) 0 1) '(1 0 2 3 4)))
  (should (equal (partner '(0 1 2 3 4) 2 3) '(0 1 3 2 4)))
  (should (equal (partner '(0 1 2 3 4) 3 2) '(0 1 3 2 4))))

(defun dance (programs moves)
  (seq-reduce #'move moves programs))

;; first question
(dance '(a b c d e f g h i j k l m n o p) (moves))

(defun period (programs moves)
  (let ((original-programs (seq-copy programs))
        (current-programs (seq-copy programs))
        (counter 0))
    (while (not (and (equal current-programs original-programs) (> counter 0)))
      (setq current-programs (dance current-programs moves)
            counter (1+ counter)))
      counter))

(defun dance-n (n programs moves)
  (dotimes (_ n programs)
    (setq programs (dance programs moves))))

;; second question
(let ((programs '(a b c d e f g h i j k l m n o p))
      (moves (moves)))
  (dance-n (mod 1000000000 (period programs moves))
           programs
           moves))
