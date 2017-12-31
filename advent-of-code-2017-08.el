;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun seq-max-or (sequence default)
  (if (seq-empty-p sequence) default (seq-max sequence)))

(defun register-get (name registers)
  (if-let (value (assoc name registers))
      (cdr value)
    0))

(defun register-delete (name registers)
  (seq-remove (lambda (register) (equal (car register) name)) registers))

(defun register-set (name value registers)
  (cons (cons name value) (register-delete name registers)))

(defun register-name (register)
  (car register))

(defun register-value (register)
  (cdr register))

(defun string-to-operator (string)
  (if (equal string "inc") '+ '-))

(defun string-to-comparator (string)
  (cond
   ((equal string "==") '=)
   ((equal string "!=") '/=)
   (t (intern string))))

(defun parse-instruction (instruction)
  (string-match "\\([a-z]+\\) \\(dec\\|inc\\) \\(-?[0-9]+\\) if \\([a-z]+\\) \\(==\\|<=?\\|>=?\\|!=\\) \\(-?[0-9]+\\)" instruction)
  (list
   (match-string 1 instruction)
   (string-to-operator (match-string 2 instruction))
   (string-to-number (match-string 3 instruction))
   (match-string 4 instruction)
   (string-to-comparator (match-string 5 instruction))
   (string-to-number (match-string 6 instruction))))

(defun instruction-destination-register (instruction)
  (nth 0 instruction))

(defun instruction-operator (instruction)
  (nth 1 instruction))

(defun instruction-operator-number (instruction)
  (nth 2 instruction))

(defun instruction-check-register (instruction)
  (nth 3 instruction))

(defun instruction-comparator (instruction)
  (nth 4 instruction))

(defun instruction-comparator-number (instruction)
  (nth 5 instruction))

(defun instruction-to-lambda (instruction)
  `(lambda (registers)
     (if (,(instruction-comparator instruction) (register-get ,(instruction-check-register instruction) registers) ,(instruction-comparator-number instruction))
         (register-set ,(instruction-destination-register instruction) (,(instruction-operator instruction) (register-get ,(instruction-destination-register instruction) registers) ,(instruction-operator-number instruction)) registers)
       registers)))

(defun compile-instruction (line)
  (instruction-to-lambda (parse-instruction line)))

(defun run-instruction (registers instruction)
  (funcall instruction registers))

(defun instructions ()
  (seq-map #'compile-instruction (read-lines (current-data-file))))

;; first question
(thread-last '()
  (seq-reduce #'run-instruction (instructions))
  (seq-map #'register-value)
  (seq-max))

;; second question
(seq-reduce (lambda (accumulator instruction)
              (let ((max-value (car accumulator))
                    (registers (cdr accumulator)))
                (setq registers (run-instruction registers instruction))
                (setq max-value (max (seq-max-or (seq-map #'register-value registers) 0) max-value))
                (cons max-value registers)))
            (instructions)
            (cons 0 '()))
