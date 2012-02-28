#lang racket

(require rackunit rackunit/gui "io.rkt" "file.rkt")

(define all-tests
  (test-suite
   "all tests"
   io-tests
   file-tests))

(test/gui all-tests)
