;;;; utils.lisp

(in-package #:cl-oneliner)

(defun hashcount (string)
  (let ((hc (make-hash-table :test 'equal))
        (words (split-sequence #\Space string)))
    (dolist (w words)
      (let ((low (string-downcase (regex-replace-all "[!-@]" w ""))))
        (if (gethash low hc)
            (incf (gethash low hc))
            (setf (gethash low hc) 1))))
    hc))

(defun hash-to-alist (hash)
  (loop for val being the hash-values of hash
     and key being the hash-keys of hash
     collect (cons key val)))

(defun most-frequent (hash)
  (let ((alist (hash-to-alist hash)))
    (car (reduce (lambda (x y) (if (> (cdr x) (cdr y)) x y )) alist))))
