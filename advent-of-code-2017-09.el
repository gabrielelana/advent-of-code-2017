;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-content (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun pick-first (s)
  (if (string-empty-p s)
      ""
    (substring s 0 1)))

(defun skip-first (s)
  (if (string-empty-p s)
      ""
    (substring s 1)))

(defun skip-garbage (input)
  (let ((count-garbage 0))
    (while (not (equal ">" (pick-first input)))
      (if (equal "!" (pick-first input))
          (setq input (skip-first (skip-first input)))
        (setq input (skip-first input)
              count-garbage (1+ count-garbage))))
    (cons (skip-first input) count-garbage)))

(defun score-group (input score level)
  (let ((count-garbage 0))
    (while (not (string-empty-p input))
      (pcase (pick-first input)
        ("}" (setq input (skip-first input) score (+ score level) level (1- level)))
        ("{" (setq input (skip-first input) level (1+ level)))
        ("," (setq input (skip-first input)))
        ("<"
         (let ((result (skip-garbage (skip-first input))))
           (setq input (car result)
                 count-garbage (+ count-garbage (cdr result)))))
        (_   (setq input (skip-first input)))))
    (cons score count-garbage)))

(defun score (input)
  (car (score-group input 0 0)))

(ert-deftest score-test ()
  (should (equal (score "{}") 1))
  (should (equal (score "{{{}}}") 6))
  (should (equal (score "{{},{}}") 5))
  (should (equal (score "{{{},{},{{}}}}") 16))
  (should (equal (score "{<a>,<a>,<a>,<a>}") 1))
  (should (equal (score "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9))
  (should (equal (score "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9))
  (should (equal (score "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3)))

;; first question
(score (read-content (current-data-file)))

(defun count-garbage (input)
  (cdr (score-group input 0 0)))

(ert-deftest count-garbage-test ()
  (should (equal (count-garbage "<>") 0))
  (should (equal (count-garbage "<random characters>") 17))
  (should (equal (count-garbage "<<<<>") 3))
  (should (equal (count-garbage "<{!>}>") 2))
  (should (equal (count-garbage "<!!>") 0))
  (should (equal (count-garbage "<!!!>>") 0))
  (should (equal (count-garbage "<{o\"i!a,<{i<a>") 10)))

;; second question
(count-garbage (read-content (current-data-file)))
