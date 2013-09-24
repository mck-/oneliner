;;;; Oneliner takes a piece of text (article) and extracts a oneliner
;;;; 1. Take the most frequent word
;;;; 2. Take the most frequent next word
;;;; 3. Repeat until no more words come next (avoid repetition of words)

(in-package #:cl-oneliner)

(defun oneliner (string)
  "Given a string, summarize the piece of text in a oneliner"
  (let ((alist (count-words string)))
    (labels ((iter (alist word oneliner)
               "One iteration will take the word from alist and append to the sentence, while recursively taking the most frequent next word that occurs after"
               (if (or (null alist) (null word))
                   (string-trim '(#\Space) oneliner)
                   (iter (remove-word alist word)
                         (get-next-word alist word)
                         (concatenate 'string oneliner word " ")))))
      (iter alist (car (alist-most-frequent alist)) ""))))

(defun get-next-word (alist word)
  (alist-most-frequent-next (assoc word alist :test #'equal)))

(defun remove-word (alist word)
  "Given an alist-count and current word, return alist without word -- and make sure they are also removed from all next counts"
  (cond ((null alist) nil)
        ((string= (caar alist) word)
         (remove-word (cdr alist) word))
        (t
         (cons (list (caar alist)
                     (cons 'NEXT (remove word (cdadar alist) :test #'equal :key #'car))
                     (caddar alist))
               (remove-word (cdr alist) word)))))
