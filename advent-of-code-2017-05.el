;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun instructions ()
  (seq-map #'string-to-number (read-lines (current-data-file))))

(defun inc-nth (n list m)
  (setf (nth n list) (+ (nth n list) m))
  list)

(defun simple-increment (n instructions m)
  (inc-nth n instructions 1))

(defun strange-increment (n instructions m)
  (if (>= m 3)
      (inc-nth n instructions -1)
    (inc-nth n instructions 1)))

(defun execute (instructions instruction-counter steps increment)
  (let ((current-instruction (nth instruction-counter instructions)))
    (while (not (equal current-instruction nil))
      (setq instructions (funcall increment instruction-counter instructions current-instruction)
            instruction-counter (+ instruction-counter current-instruction)
            steps (1+ steps)
            current-instruction (nth instruction-counter instructions)))
    steps))

(ert-deftest execute-test ()
  (should (equal (execute '(0 3 0 1 -3) 0 0 #'simple-increment) 5))
  (should (equal (execute '(0 3 0 1 -3) 0 0 #'strange-increment) 10)))

;; first question
(execute (instructions) 0 0 #'simple-increment)

;; second question
(execute (instructions) 0 0 #'strange-increment)
