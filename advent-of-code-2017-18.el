;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun parse-register-or-number (register-or-number)
  (if (string-match-p "^[a-z]$" register-or-number)
      (intern register-or-number)
    (string-to-number register-or-number)))

(defun parse-instruction (instruction-string)
  (cond
   ((string-match "\\(set\\|add\\|mul\\|mod\\|jgz\\) \\([a-z]\\) \\([-a-z0-9]+\\)" instruction-string)
    (list (match-string 1 instruction-string)
          (parse-register-or-number (match-string 2 instruction-string))
          (parse-register-or-number (match-string 3 instruction-string))))
   ((string-match "\\(rcv\\|snd\\) \\([a-z]\\)" instruction-string)
    (list (match-string 1 instruction-string)
          (parse-register-or-number (match-string 2 instruction-string))))
   (t nil)))

(defun register-set (r v registers)
  (plist-put registers r v))

(defun register-get (r registers)
  (if-let ((v (plist-get registers r)))
      v
    0))

(defun register-get-or-number (r registers)
  (if (numberp r)
      r
    (register-get r registers)))

(defun make-runtime ()
  '(0 nil nil ()))

(defun execute-instruction (runtime instruction)
  (let ((instruction-pointer (nth 0 runtime))
        (last-played-sound (nth 1 runtime))
        (recovered-sound (nth 2 runtime))
        (registers (nth 3 runtime)))
    (setq instruction-pointer (1+ instruction-pointer))
    (pcase instruction
      ;; set X Y sets register X to the value of Y.
      (`("set" ,r1 ,r2) (setq registers (register-set r1 (register-get-or-number r2 registers) registers)))
      ;; add X Y increases register X by the value of Y.
      (`("add" ,r1 ,r2) (setq registers (register-set r1 (+ (register-get r1 registers) (register-get-or-number r2 registers)) registers)))
      ;; mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
      (`("mul" ,r1 ,r2) (setq registers (register-set r1 (* (register-get r1 registers) (register-get-or-number r2 registers)) registers)))
      ;; mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
      (`("mod" ,r1 ,r2) (setq registers (register-set r1 (mod (register-get r1 registers) (register-get-or-number r2 registers)) registers)))
      ;; jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
      (`("jgz" ,r1 ,r2) (when (/= (register-get-or-number r1 registers) 0)
                          (setq instruction-pointer (+ instruction-pointer (1- (register-get-or-number r2 registers))))))
      ;; snd X plays a sound with a frequency equal to the value of X.
      (`("snd" ,r1) (setq last-played-sound (register-get-or-number r1 registers)))
      ;; rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
      (`("rcv" ,r1) (when (and last-played-sound (/= (register-get-or-number r1 registers) 0))
                      (setq recovered-sound last-played-sound
                            instruction-pointer -1))))
    (list instruction-pointer last-played-sound recovered-sound registers)))

(defun fetch-instruction (runtime instructions)
  (let ((instruction-pointer (car runtime)))
    (if (< instruction-pointer 0)
        nil
      (nth (car runtime) instructions))))

(defun execute (runtime instructions)
  (let ((instruction (fetch-instruction runtime instructions)))
    (while instruction
      (setq runtime (execute-instruction runtime instruction)
            instruction (fetch-instruction runtime instructions))))
  runtime)

(defun instructions ()
  (seq-map #'parse-instruction (read-lines (current-data-file))))

;;; first question
(execute (make-runtime) (instructions))
