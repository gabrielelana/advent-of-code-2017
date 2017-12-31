;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun parse-program (line)
  (string-match "^\\([a-z]+\\) (\\([0-9]+\\))\\(?: -> \\(.*\\)\\)?$" line)
  (let ((program (list (match-string 1 line) (match-string 2 line))))
    (when (match-string 3 line)
      (setq program (append program (list (split-string (match-string 3 line) ",\\s-")))))
    program))

(defun programs ()
  (seq-map #'parse-program (read-lines (current-data-file))))

;; utility functions
(defun caddr (list)
  (car (cddr list)))

(defun seq-flatten (sequence)
  (let ((result '()))
    (seq-do (lambda (e)
              (setq result (append result e)))
            sequence)
    result))

(defun seq-remove-all (element sequence)
  (seq-remove (lambda (e) (equal e element)) sequence))

(defun seq-difference (source to-remove)
  (seq-do (lambda (e)
            (setq source (seq-remove-all e source)))
          to-remove)
  source)

(defun seq-sum (l)
  (seq-reduce #'+ l 0))

(defun seq-all-equals-p (l)
  (let ((r (car l)))
    (seq-reduce
     (lambda (all-equals e) (and all-equals (equal e r)))
     (cdr l)
     t)))

;; program manipulation functions
(defun weight-of (program)
  (string-to-number (cadr program)))

(defun holding? (program)
  (> (seq-length program) 2))

(defun name-of (program)
  (car program))

(defun held-by (program)
  (caddr program))

(defun held-by-at (n program)
  (nth n (held-by program)))

(defun filter-holding (programs)
  (seq-filter #'holding? programs))

(defun holding-program-names (programs)
  (seq-map #'car (filter-holding (programs))))

(defun held-program-names (programs)
  (seq-flatten (seq-map #'caddr (filter-holding (programs)))))

(defun weights-of-each-held (program programs)
  (seq-map (lambda (program-name) (weight program-name programs)) (held-by program)))

(defun weight-of-held (program programs)
  (seq-sum (weights-of-each-held program programs)))

(defun weight (program-name programs)
  (let ((program (fetch-program program-name programs)))
    (if (holding? program)
        (+ (weight-of program)
           (weight-of-held program programs))
      (weight-of program))))

(defun fetch-program (program-name programs)
  (seq-find (lambda (program) (equal program-name (name-of program))) programs))

(defun expected-weight (weights)
  (seq-find
   (lambda (w1)
     (> (seq-count (lambda (w2) (equal w1 w2)) weights) 1))
   weights))

(defun unexpected-weight (weights)
  (seq-find
   (lambda (w1)
     (= (seq-count (lambda (w2) (equal w1 w2)) weights) 1))
   weights))

(defun index-of-unexpected-weight (weights)
  (seq-position weights (unexpected-weight weights)))

(defun unbalanced-program-name (programs)
  (seq-find
   (lambda (program-name)
     (let* ((program (fetch-program program-name programs))
            (weights (weights-of-each-held program programs)))
       (not (seq-all-equals? weights))))
   (holding-program-names programs)))

;; first question
(let ((programs (programs)))
  (car (seq-difference
        (holding-program-names programs)
        (held-program-names programs))))

;; second question
(let* ((programs (programs))
       (unbalanced-program-name (unbalanced-program-name programs))
       (unbalanced-program (fetch-program unbalanced-program-name programs))
       (unbalanced-weights (weights-of-each-held unbalanced-program programs))
       (program-with-unexpected-weight (fetch-program (held-by-at (index-of-unexpected-weight unbalanced-weights) unbalanced-program) programs)))
  (- (expected-weight unbalanced-weights) (weight-of-held program-with-unexpected-weight programs)))
