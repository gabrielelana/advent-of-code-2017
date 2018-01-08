;;; -*- lexical-binding: t -*-
(require 'ert)

;;; for the first time I will solve the problem visually loading the
;;; data file in a buffer and executing the funciton below as a
;;; command

;;; first and second question
(defun adv-201719 ()
  (interactive)
  (goto-start)
  (run-at-time 0 nil (lambda () (follow-path 'down '() 0))))

(defun goto-start ()
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (search-forward "|")
    (backward-char)
    (point)))

(defun follow-path (direction trail steps)
  (with-current-buffer (current-buffer)
    (let ((current-char (char-after)))
      (cond
       ((and (>= current-char ?A) (<= current-char ?Z))
        (progn
          (setq trail (cons (char-to-string current-char) trail))
          (message "after %d steps trail is %S" steps (seq-reverse trail))
          (follow-direction direction)
          (run-at-time 0 nil (lambda () (follow-path direction trail (1+ steps))))))
       ((equal current-char ?+)
        (progn
          (setq direction (next-direction direction))
          (follow-direction direction)
          (run-at-time 0 nil (lambda () (follow-path direction trail (1+ steps))))))
       ((equal current-char 32)
        (message "after %d steps trail is %S" steps (seq-reverse trail)))
       (t
        (progn
          (follow-direction direction)
          (run-at-time 0 nil (lambda () (follow-path direction trail (1+ steps))))))))))

(defun follow-direction (direction)
  (pcase direction
    ('down (go-down))
    ('up (go-up))
    ('right (go-right))
    ('left (go-left))))

(defun next-direction (direction)
  (thread-last
      (list (when (character-down-p) 'down)
            (when (character-up-p) 'up)
            (when (character-right-p) 'right)
            (when (character-left-p) 'left))
    (seq-compact)
    (seq-without (inverse-of direction))
    (car)))

(defun inverse-of (direction)
  (pcase direction
    ('down 'up)
    ('up 'down)
    ('right 'left)
    ('left 'right)))

(defmacro character-direction-p (direction)
  (let ((function-name (intern (concat "character-" (symbol-name direction) "-p")))
        (go-function-name (intern (concat "go-" (symbol-name direction)))))
    `(defun ,function-name ()
      (with-current-buffer (current-buffer)
        (save-excursion
          (,go-function-name)
          (let ((current-character (char-after)))
            (looking-at "[-A-Z|+]")))))))

(character-direction-p down)
(character-direction-p up)
(character-direction-p right)
(character-direction-p left)

(defun go-down ()
  (with-current-buffer (current-buffer)
    (let ((c (current-column)))
      (forward-line)
      (move-to-column c))))

(defun go-up ()
  (with-current-buffer (current-buffer)
    (let ((c (current-column)))
      (forward-line -1)
      (move-to-column c))))

(defun go-right ()
  (with-current-buffer (current-buffer)
    (move-to-column (1+ (current-column)))))

(defun go-left ()
  (with-current-buffer (current-buffer)
    (move-to-column (1- (current-column)))))

(defun seq-without (to-remove sequence)
  (seq-remove (lambda (e) (equal e to-remove)) sequence))

(defun seq-compact (sequence)
  (seq-filter #'identity sequence))
