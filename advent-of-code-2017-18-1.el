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
   ((string-match "\\(set\\|add\\|mul\\|mod\\|jgz\\) \\([a-z]\\) \\([a-z]\\|[-0-9]+\\)" instruction-string)
    (list (match-string 1 instruction-string)
          (parse-register-or-number (match-string 2 instruction-string))
          (parse-register-or-number (match-string 3 instruction-string))))
   ((string-match "\\(rcv\\|snd\\) \\([a-z]\\|[-0-9]+\\)" instruction-string)
    (list (match-string 1 instruction-string)
          (parse-register-or-number (match-string 2 instruction-string))))
   (t nil)))

(defun make-runtime (&rest registers)
  (list 0 nil nil (or registers '())))

(defun register-set (runtime r v)
  (setf (nth 3 runtime) (plist-put (nth 3 runtime) r v))
  runtime)

(defun register-get (runtime r)
  (if (numberp r)
      r
    (if-let ((v (plist-get (nth 3 runtime) r)))
        v
      0)))

(defun increment-instruction-pointer (runtime &optional n)
  (setf (nth 0 runtime) (+ (or n 1) (nth 0 runtime)))
  runtime)

(defun play-sound (runtime v)
  (setf (nth 1 runtime) v)
  runtime)

(defun recover-last-played-sound (runtime)
  (setf (nth 2 runtime) (nth 1 runtime))
  runtime)

(defun last-played-sound (runtime)
  (nth 1 runtime))

(defun terminate (runtime)
  (setf (nth 0 runtime) -1)
  runtime)

;; set X Y sets register X to the value of Y.
(defun execute-set (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (register-get runtime r2))
    (increment-instruction-pointer)))

;; add X Y increases register X by the value of Y.
(defun execute-add (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (+ (register-get runtime r1) (register-get runtime r2)))
    (increment-instruction-pointer)))

;; mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
(defun execute-mul (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (* (register-get runtime r1) (register-get runtime r2)))
    (increment-instruction-pointer)))

;; mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
(defun execute-mod (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (mod (register-get runtime r1) (register-get runtime r2)))
    (increment-instruction-pointer)))

;; jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
(defun execute-jgz (runtime r1 r2)
  (if (/= (register-get runtime r1) 0)
      (increment-instruction-pointer runtime (register-get runtime r2))
    (increment-instruction-pointer runtime)))

;; snd X plays a sound with a frequency equal to the value of X.
(defun execute-snd (runtime r)
  (thread-first runtime
    (play-sound (register-get runtime r))
    (increment-instruction-pointer)))

;; rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
(defun execute-rcv (runtime r)
  (if (and (last-played-sound runtime) (/= (register-get runtime r) 0))
      (progn (recover-last-played-sound runtime)
             (terminate runtime))
    (increment-instruction-pointer runtime)))

(defun execute-instruction (runtime instruction)
  (pcase instruction
    (`("set" ,r1 ,r2) (execute-set runtime r1 r2))
    (`("add" ,r1 ,r2) (execute-add runtime r1 r2))
    (`("mul" ,r1 ,r2) (execute-mul runtime r1 r2))
    (`("mod" ,r1 ,r2) (execute-mod runtime r1 r2))
    (`("jgz" ,r1 ,r2) (execute-jgz runtime r1 r2))
    (`("snd" ,r) (execute-snd runtime r))
    (`("rcv" ,r) (execute-rcv runtime r))))

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
(cadr (execute (make-runtime) (instructions)))
