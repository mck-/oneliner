;;;; Unit tests for utilities

(in-package #:cl-oneliner)

(define-test hashcount
  (assert-equal 1 (gethash "this" (hashcount "this is a test")))
  (assert-equal 2 (gethash "this" (hashcount "this is this")))
  (assert-equal 3 (gethash "this" (hashcount "not this but this is this")))
  (assert-equal 4 (gethash "this" (hashcount "this THIS This this")))
  (assert-equal 5 (gethash "this" (hashcount "this, THIS This. this; THIS!"))))

(define-test hash-to-alist
  (let ((strhash (make-hash-table :test 'equal)))
    (setf (gethash "hello" strhash) 2)
    (setf (gethash "world" strhash) 3)
    (setf (gethash "this" strhash) 10)
    (assert-equal '(("hello" . 2) ("world" . 3) ("this" . 10)) (hash-to-alist strhash))))

(define-test highest-occurence
  (assert-equal "this" (most-frequent (hashcount "this is this test")))
  (assert-equal "is" (most-frequent (hashcount "Is this is this test is?")))
  (assert-equal "test" (most-frequent (hashcount "TEST test, testing; test!"))))
