;;; -*- lexical-binding: t -*-
(require 'ert)

(defun particles ()
  (seq-map #'index-particle (seq-with-index (seq-map #'parse-particle (read-lines (current-data-file))))))

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun parse-particle (string)
  (string-match "p=<\\([^>]+\\)>, v=<\\([^>]+\\)>, a=<\\([^>]+\\)>" string)
  (let ((p (match-string 1 string))
        (v (match-string 2 string))
        (a (match-string 3 string)))
    (list 'p (parse-vector p)
          'v (parse-vector v)
          'a (parse-vector a))))

(defun parse-vector (string)
  (seq-map #'string-to-number (split-string string "," t)))

(defun index-particle (pair)
  (plist-put (cdr pair) 'i (car pair))
  (cdr pair))

(ert-deftest parse-particle-test ()
  (should (equal (parse-particle "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>") '(p (3 0 0) v (2 0 0) a (-1 0 0))))
  (should (equal (parse-particle "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>") '(p (4 0 0) v (0 0 0) a (-2 0 0)))))

(defun index (particle)
  (plist-get particle 'i))

(defun position (particle)
  (plist-get particle 'p))

(defun position-set (particle velocity)
  (plist-put particle 'p velocity))

(defun velocity (particle)
  (plist-get particle 'v))

(defun velocity-set (particle velocity)
  (plist-put particle 'v velocity))

(defun acceleration (particle)
  (plist-get particle 'a))

(defun vector-add (v1 v2)
  (seq-map (lambda (pair) (+ (car pair) (cdr pair)))
           (seq-zip v1 v2)))

(ert-deftest vector-add-test ()
  (should (equal (vector-add '(0 0 0) '(1 1 1)) '(1 1 1)))
  (should (equal (vector-add '(1 2 3) '(3 2 1)) '(4 4 4))))

(defun seq-zip (s1 s2)
  (let (s)
    (while (and (not (seq-empty-p s1))
                (not (seq-empty-p s2)))
      (setq s (cons (cons (car s1) (car s2)) s)
            s1 (cdr s1)
            s2 (cdr s2)))
    (seq-reverse s)))

(ert-deftest seq-zip-test ()
  (should (equal (seq-zip '(1 2) '(3 4)) '((1 . 3) (2 . 4)))))

(defun seq-with-index (s)
  (let (si (i 0))
    (while (not (seq-empty-p s))
      (setq si (cons (cons i (car s)) si)
            s (cdr s)
            i (1+ i)))
    (seq-reverse si)))

(ert-deftest seq-with-index-test ()
  (should (equal (seq-with-index '(3 4 5 2 2 1 1)) '((0 . 3) (1 . 4) (2 . 5) (3 . 2) (4 . 2) (5 . 1) (6 . 1)))))

(defun tick (particle)
  (velocity-set particle (vector-add (velocity particle) (acceleration particle)))
  (position-set particle (vector-add (position particle) (velocity particle)))
  particle)

p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

(ert-deftest tick-test ()
  (should (equal (tick '(p (3 0 0) v (2 0 0) a (-1 0 0))) '(p (4 0 0) v (1 0 0) a (-1 0 0))))
  (should (equal (tick '(p (4 0 0) v (1 0 0) a (-1 0 0))) '(p (4 0 0) v (0 0 0) a (-1 0 0))))
  (should (equal (tick '(p (4 0 0) v (0 0 0) a (-1 0 0))) '(p (3 0 0) v (-1 0 0) a (-1 0 0)))))

(defun distance-from-origin (p)
  (manhattan-distance (position p) '(0 0 0)))

(defun manhattan-distance (c1 c2)
  (+ (abs (- (nth 0 c1)
             (nth 0 c2)))
     (abs (- (nth 1 c1)
             (nth 1 c2)))
     (abs (- (nth 2 c1)
             (nth 2 c2)))))

(defun sort-particles (particles)
  (seq-sort (lambda (p1 p2)
              (< (distance-from-origin p1)
                 (distance-from-origin p2)))
            particles))

;;; first question
(let ((particles (particles)))
  (dotimes (_ 1000 (index (car particles)))
    (message "closest particle is #%d with distance %d" (index (car particles)) (distance-from-origin (car particles)))
    (setq particles (seq-map #'tick particles)
          particles (sort-particles particles))))

(defun destroy-by-collision (particles)
  (thread-last particles
    (seq-group-by #'position)
    (seq-remove (lambda (e) (> (seq-length e) 2)))
    (seq-map (lambda (e) (nth 1 e)))))

;;; second question
(let ((particles (particles)))
  (dotimes (_ 1000 (seq-length particles))
    (message "number of particles: %d" (seq-length particles))
    (setq particles (destroy-by-collision particles)
          particles (seq-map #'tick particles))))
