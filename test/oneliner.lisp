;;;; Unit tests for oneliner algorithm

(in-package #:cl-oneliner)

(define-test short-sentence
  "Test the algorithm on short sentence"
  (:tag :oneliner)
  (assert-equal "" (oneliner ""))
  (assert-equal "hello world" (oneliner "hello world hello"))
  (assert-equal "this test and not" (oneliner "this test is this test and not this one, this test and!")))
