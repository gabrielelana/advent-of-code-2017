;;; -*- lexical-binding: t -*-
(require 'ert)

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun current-data-file ()
  (concat (file-name-base (buffer-name (current-buffer))) ".data"))

(defun words-from (phrase)
  (split-string phrase))

(defun passphrases ()
  (seq-map #'words-from (read-lines (current-data-file))))

(defun letters-from (word)
  (split-string word "" t))

(defun normalize-word (word)
  (sort (letters-from word) #'string-lessp))

(defun anagrams-p (lw rw)
  (equal (normalize-word lw)
         (normalize-word rw)))

(defun ordered-pairs (l)
  (let (result (i 1))
    (dolist (n l result)
      (dolist (m (nthcdr i l) result)
        (setq result (cons (cons n m) result)
              i (1+ i))))))

(ert-deftest ordered-pairs-test ()
  (should (equal (ordered-pairs '(1 2 3 4)) '((1 . 4) (1 . 3) (1 . 2))))
  (should (equal (ordered-pairs '("a" "b" "c")) '(("a" . "c") ("a" . "b"))))
  (should (equal (ordered-pairs '("a" "b" "a")) '(("a" . "a") ("a" . "b")))))

(defun apply-binary-predicate-to-pair (pair binary-predicate)
  (funcall binary-predicate (car pair) (cdr pair)))

(defun contains-words-p (words predicate)
  (not (seq-empty-p (seq-filter
                     (lambda (pair)
                       (apply-binary-predicate-to-pair pair predicate))
                     (ordered-pairs words)))))

(ert-deftest contains-words-p-test ()
  (should (equal (contains-words-p '("a" "b") #'equal) nil))
  (should (equal (contains-words-p '("a" "b" "c") #'equal) nil))
  (should (equal (contains-words-p '("a" "b" "a") #'equal) t))
  (should (equal (contains-words-p '("a" "a") #'equal) t))
  (should (equal (contains-words-p '("a") #'equal) nil))
  (should (equal (contains-words-p '() #'equal) nil)))

(defun count-passphrases-with-words-not (predicate)
  (length (seq-filter (lambda (passphrase)
                        (not (contains-words-p passphrase predicate)))
                      (passphrases))))

;; first question
(count-passphrases-with-words-not #'equal)

;; second question
(count-passphrases-with-words-not #'anagrams-p)
