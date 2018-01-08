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

(defun instructions ()
  (seq-map #'parse-instruction (read-lines (current-data-file))))

(defun make-runtime (send-buffer receive-buffer process-id)
  (list 0 send-buffer receive-buffer `(,(intern "p") ,process-id)))

(defun instruction-pointer (runtime)
  (nth 0 runtime))

(defun send-buffer (runtime)
  (nth 1 runtime))

(defun receive-buffer (runtime)
  (nth 2 runtime))

(defun registers (runtime)
  (nth 3 runtime))

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

;;; set X Y sets register X to the value of Y.
(defun execute-set (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (register-get runtime r2))
    (increment-instruction-pointer)))

;;; add X Y increases register X by the value of Y.
(defun execute-add (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (+ (register-get runtime r1) (register-get runtime r2)))
    (increment-instruction-pointer)))

;;; mul X Y sets register X to the result of multiplying the value
;;; contained in register X by the value of Y.
(defun execute-mul (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (* (register-get runtime r1) (register-get runtime r2)))
    (increment-instruction-pointer)))

;;; mod X Y sets register X to the remainder of dividing the value
;;; contained in register X by the value of Y (that is, it sets X to
;;; the result of X modulo Y).
(defun execute-mod (runtime r1 r2)
  (thread-first runtime
    (register-set r1 (mod (register-get runtime r1) (register-get runtime r2)))
    (increment-instruction-pointer)))

;;; jgz X Y jumps with an offset of the value of Y, but only if the
;;; value of X is greater than zero. (An offset of 2 skips the next
;;; instruction, an offset of -1 jumps to the previous instruction, and
;;; so on.)
(defun execute-jgz (runtime r1 r2)
  (if (/= (register-get runtime r1) 0)
      (increment-instruction-pointer runtime (register-get runtime r2))
    (increment-instruction-pointer runtime)))

;;; snd X sends the value of X to the other program. These values wait
;;; in a queue until that program is ready to receive them. Each
;;; program has its own message queue, so a program can never receive
;;; a message it sent.
(defun execute-snd (runtime r)
  (pipe-send (nth 1 runtime) (register-get runtime r))
  (thread-first runtime
    (register-set :sent-count (1+ (register-get runtime :sent-count)))
    (increment-instruction-pointer)))

;;; rcv X receives the next value and stores it in register X. If no
;;; values are in the queue, the program waits for a value to be sent
;;; to it. Programs do not continue to the next instruction until they
;;; have received a value. Values are received in the order they are
;;; sent.
(defun execute-rcv (runtime r)
  (if-let ((received (pipe-receive (nth 2 runtime))))
      (thread-first runtime
        (register-set r received)
        (register-set :waiting nil)
        (increment-instruction-pointer))
    (register-set runtime :waiting t)))

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

(defun make-pipe (name)
  (with-current-buffer (get-buffer-create name)
    (erase-buffer)
    (current-buffer)))

(defun pipe-send (pipe v)
  (with-current-buffer pipe
    (goto-char (point-max))
    (insert (format "%d" v))
    (newline)))

(defun pipe-receive (pipe)
  (with-current-buffer pipe
    (if (= (buffer-size) 0)
        nil
      (goto-char (point-min))
      (let ((current-line (thing-at-point 'line t)))
        (kill-whole-line)
        (string-to-number current-line))
      )))

(defun pipe-empty-p (pipe)
  (with-current-buffer pipe
    (= (buffer-size) 0)))

(defun waiting-p (runtime)
  (eq t (register-get runtime :waiting)))

(defun stuck-p (runtime)
  (and (waiting-p runtime) (pipe-empty-p (receive-buffer runtime))))

(defun finished-p (runtime instructions)
  (not (fetch-instruction runtime instructions)))

(defun next-p (runtime instructions)
  (and (not (finished-p runtime instructions))
       (not (stuck-p runtime))))

(defun deadlock-p (process-0 process-1 instructions)
  (and (not (next-p process-0 instructions)) (not (next-p process-1 instructions))))

(defun run (runtime instructions)
  (let (instruction)
    (while (next-p runtime instructions)
      (setq instruction (fetch-instruction runtime instructions)
            registers-before (seq-copy (registers runtime))
            runtime (execute-instruction runtime instruction))))
  runtime)

(defun sample-instructions ()
  (seq-map #'parse-instruction
          '("snd 1"
            "snd 2"
            "snd p"
            "rcv a"
            "rcv b"
            "rcv c"
            "rcv d"
            )))

(let* ((buffer-0 (make-pipe "*buffer-0*"))
       (buffer-1 (make-pipe "*buffer-1*"))
       (process-0 (make-runtime buffer-0 buffer-1 0))
       (process-1 (make-runtime buffer-1 buffer-0 1))
       (instructions (instructions)))
  (while (not (deadlock-p process-0 process-1 instructions))
    (setq process-0 (run process-0 instructions)
          process-1 (run process-1 instructions))
    (message "process-1: %S" process-1))
  (register-get process-1 :sent-count))
