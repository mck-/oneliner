;;;; Unit tests for utilities

(in-package #:cl-oneliner)

;;; Simple hash count

(define-test hashcount
  "Test the hashcount util"
  (:tag :util)
  (assert-equal 1 (gethash "this" (hashcount "this is a test")))
  (assert-equal 2 (gethash "this" (hashcount "this is this")))
  (assert-equal 3 (gethash "this" (hashcount "not this but this is this")))
  (assert-equal 4 (gethash "this" (hashcount "this THIS This this")))
  (assert-equal 5 (gethash "this" (hashcount "this, THIS This. this; THIS!"))))

(define-test hash-to-alist
  "Convert a hash to an alist"
  (:tag :util)
  (let ((strhash (make-hash-table :test 'equal)))
    (setf (gethash "hello" strhash) 2)
    (setf (gethash "world" strhash) 3)
    (setf (gethash "this" strhash) 10)
    (assert-equal '(("hello" . 2) ("world" . 3) ("this" . 10)) (hash-to-alist strhash))))

(define-test highest-occurence
  (:tag :util)
  (assert-equal "this" (most-frequent (hashcount "this is this test")))
  (assert-equal "is" (most-frequent (hashcount "Is this is this test is?")))
  (assert-equal "test" (most-frequent (hashcount "TEST test, testing; test!"))))

;;; Simple alist count

(define-test alist-count
  "Test the alist-count util"
  (:tag :util)
  (assert-equal 1 (get-count "this" (alist-count "this is a test")))
  (assert-equal 2 (get-count "this" (alist-count "this is this")))
  (assert-equal 3 (get-count "this" (alist-count "not this but this is this")))
  (assert-equal 4 (get-count "this" (alist-count "this THIS This this")))
  (assert-equal 5 (get-count "this" (alist-count "this, THIS This. this; THIS!"))))

(define-test alist-highest-occurence
  "Test return of alist-most-frequent, to include next counts too"
  (:tag :util)
  (assert-equal '(("test" . 1)("is" . 1))
                (val (list (alist-most-frequent (count-words "this is this test")))
                     "this" 'next))
  (assert-equal 2 (val (list (alist-most-frequent (count-words "Is this is this test is?")))
                     "is" 'next "this"))
  (assert-equal '(("testing" . 1)("test" . 1))
                (val (list (alist-most-frequent (count-words "TEST test, testing; test!")))
                     "test" 'next)))

(define-test next-counts
  "Test counting of next words, given a word and a string"
  (:tag :util)
  (assert-equal 2 (aval "is" (next-counts "this" "this is a test and this is another test")))
  (assert-equal 1 (aval "a" (next-counts "is" "this is a test and this is another test")))
  (assert-equal '(("and" . 1)) (next-counts "test" "this is a test and this is another test"))
  (assert-equal 1 (aval "and" (next-counts "test" "this is a test and this is another test"))))
