#lang racket

(require rackunit
         racket/contract
         racket/path
         racket/runtime-path
         (planet dherman/test:1/test)
         mzlib/etc
         "../../file.rkt")

(define root-directory
  (normalize-path
   (build-path (this-expression-source-directory)
               'up 'up)))

(define this-directory-relative-path
  (build-path "private" "tests"))

(define this-file-relative-path
  (build-path this-directory-relative-path "file.rkt"))

(define-syntax-rule (in-root-directory e1 e2 ...)
  (parameterize ([current-directory root-directory])
    e1 e2 ...))

(define-check (check-contract-passes contract x)
  (check-true ((flat-contract-predicate contract) x)))
(define-check (check-contract-fails contract x)
  (check-false ((flat-contract-predicate contract) x)))

(define contract-tests
  (test-suite
   "contract tests"
   (test-case "relative is relative (path)"
              (check-contract-passes relative-path/c (build-path 'same)))
   (test-case "relative is relative (string)"
              (check-contract-passes relative-path/c (path->string (build-path 'same))))
   (test-case "complete is not relative (path)"
              (check-contract-fails relative-path/c (path->complete-path (current-directory))))
   (test-case "complete is not relative (string)"
              (check-contract-fails relative-path/c
                                    (path->string (path->complete-path (current-directory)))))
   (test-case "relative is not complete (path)"
              (check-contract-fails complete-path/c (build-path 'same)))
   (test-case "relative is not complete (string)"
              (check-contract-fails complete-path/c (path->string (build-path 'same))))
   (test-case "complete is complete (path)"
              (check-contract-passes complete-path/c (path->complete-path (current-directory))))
   (test-case "complete is complete (string)"
              (check-contract-passes complete-path/c
                                     (path->string (path->complete-path (current-directory)))))
   (test-case "relative file"
              (in-root-directory
               (check-contract-passes relative-file-path/c
                                      (build-path "file.rkt"))))
   (test-case "relative non-file"
              (in-root-directory
               (check-contract-fails relative-file-path/c
                                     (build-path "does-not-exist.txt"))))
   (test-case "non-relative file"
              (in-root-directory
               (check-contract-fails relative-file-path/c
                                     (path->complete-path
                                      (build-path "file.rkt")))))
   (test-case "complete file"
              (in-root-directory
               (check-contract-passes complete-file-path/c
                                      (path->complete-path
                                       (build-path "file.rkt")))))
   (test-case "complete non-file"
              (in-root-directory
               (check-contract-fails complete-file-path/c
                                     (path->complete-path
                                      (build-path "does-not-exist.txt")))))
   (test-case "non-complete file"
              (in-root-directory
               (check-contract-fails complete-file-path/c
                                     (build-path "file.rkt"))))
   (test-case "relative directory"
              (in-root-directory
               (check-contract-passes relative-directory-path/c
                                      (build-path 'same))))
   (test-case "relative non-directory"
              (in-root-directory
               (check-contract-fails relative-directory-path/c
                                     (build-path "file.rkt"))))
   (test-case "non-relative directory"
              (in-root-directory
               (check-contract-fails relative-directory-path/c
                                     (path->complete-path
                                      (build-path 'same)))))
   (test-case "complete directory"
              (in-root-directory
               (check-contract-passes complete-directory-path/c
                                      (path->complete-path
                                       (build-path 'same)))))
   (test-case "complete non-directory"
              (in-root-directory
               (check-contract-fails complete-directory-path/c
                                     (path->complete-path
                                      (build-path "file.rkt")))))
   (test-case "non-complete directory"
              (in-root-directory
               (check-contract-fails complete-directory-path/c
                                     (build-path 'same))))
   ))

(define (remove-first x ls [equiv? eq?])
  (let loop ([ls ls] [result '()])
    (cond
      [(null? ls) #f]
      [(equiv? (car ls) x) (append (reverse result) (cdr ls))]
      [else (loop (cdr ls) (cons (car ls) result))])))

(define path-manipulation-tests
  (test-suite
   "path manipulations"
   (test-case "path->relative-path (. directory)"
              (check path=?
                     (path->relative-path
                      (path->complete-path (build-path 'same)))
                     (build-path 'same)))
   (test-case "path->relative-path"
              (in-new-directory "sandbox"
                                (let ([e (build-path "a" "b" "c" "d" "e")])
                                  (make-directory* e)
                                  (let ([complete (path->complete-path e)])
                                    (check path=? (path->relative-path complete) e)))))
   (test-case "explode-relative-path"
              (in-new-directory "sandbox"
                                (let ([e (build-path "a" "b" "c" "d" "e")])
                                  (make-directory* e)
                                  (check (lambda (ls1 ls2)
                                           (list-permutation? ls1 ls2 path=?))
                                         (explode-relative-path e)
                                         (map build-path (list "a" "b" "c" "d" "e"))))))
   (test-case "telescope-relative-path"
              (in-new-directory "sandbox"
                                (let ([e (build-path "a" "b" "c" "d" "e")])
                                  (make-directory* e)
                                  (check (lambda (ls1 ls2)
                                           (list-permutation? ls1 ls2 path=?))
                                         (telescope-relative-path e)
                                         (list (build-path "a")
                                               (build-path "a" "b")
                                               (build-path "a" "b" "c")
                                               (build-path "a" "b" "c" "d")
                                               (build-path "a" "b" "c" "d" "e"))))))
   ))

(define (make-file path)
  (with-output-to-file path
    (lambda ()
      (printf "hello, world~n"))))

(define filesystem-tests
  (test-suite
   "filesystem tests"
   (test-case "directory-list/all"
              (in-new-directory "sandbox"
                                (let ([dir1 (build-path "a" "b")]
                                      [dir2 (build-path "c")])
                                  (make-directory* dir1)
                                  (make-directory* dir2)
                                  (make-file (build-path "a" "file1.txt"))
                                  (make-file (build-path "a" "b" "file2.txt"))
                                  (make-file (build-path "c" "file3.txt"))
                                  (check (lambda (ls1 ls2)
                                           (list-permutation? ls1 ls2 path=?))
                                         (directory-list/all)
                                         (list (build-path "a")
                                               (build-path "a" "b")
                                               (build-path "c")
                                               (build-path "a" "file1.txt")
                                               (build-path "a" "b" "file2.txt")
                                               (build-path "c" "file3.txt"))))))
   ))

(define path-comparison-tests
  (test-suite
   "path comparisons"
   (test-case "path-normalized=? normalizes paths before checking"
              (check-true
               (in-root-directory
                (path-normalized=?
                 (build-path this-directory-relative-path
                             'up 'up "private" "tests")
                 this-directory-relative-path))))
   (test-case "relative path=?"
              (check-true
               (in-root-directory
                (path=? (build-path "private")
                        (build-path "private")))))
   (test-case "a file is distinct from its parent directory"
              (check-false
               (in-root-directory
                (path=?
                 this-directory-relative-path
                 this-file-relative-path))))
   ))

(define file-tests
  (test-suite
   "All file.ss tests"
   contract-tests
   path-manipulation-tests
   path-comparison-tests
   filesystem-tests
   ))

(provide file-tests)
