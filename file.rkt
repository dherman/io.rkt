#lang racket/base

(require racket/contract
         racket/path
         (except-in srfi/1/list any))

;; =============================================================================
;; CONTRACTS
;; =============================================================================

;; TODO: expanded-path?, simple-path?

;; A relative-path is a path for which relative-path? is #t. A
;; relative-file-path or relative-directory-path is a relative-path for
;. which file-exists? or directory-exists? is #t, respectively.

;; relative-path/c : contract
(define relative-path/c
  (and/c (or/c path? string?) relative-path?))

;; relative-file-path/c : contract
(define relative-file-path/c
  (and/c relative-path/c file-exists?))

;; relative-directory-path/c : contract
(define relative-directory-path/c
  (and/c relative-path/c directory-exists?))

;; A complete-path is a path for which complete-path? is #t. A
;; complete-file-path or complete-directory-path is a complete-path for
;. which file-exists? or directory-exists? is #t, respectively.

;; complete-path/c : contract
(define complete-path/c
  (and/c (or/c path? string?) complete-path?))

;; complete-file-path/c : contract
(define complete-file-path/c
  (and/c complete-path/c file-exists?))

;; complete-directory-path/c : contract
(define complete-directory-path/c
  (and/c complete-path/c directory-exists?))

;; =============================================================================
;; PROCEDURES
;; =============================================================================

;; path->relative-path : (or string path) [(or string path)] -> relative-path
;; converts a path to a relative path
(define (path->relative-path path [relative-to (current-directory)])
  (let-values ([(base name must-be-dir?) (split-path path)])
    (let loop ([base base] [name name] [rest (list name)])
      (if (path=? base relative-to)
          (apply build-path rest)
          (let-values ([(base name must-be-dir?) (split-path base)])
            (loop base name (cons name rest)))))))

;; explode-relative-path : relative-path -> (listof path)
;; computes the list of directories in a relative path
(define (explode-relative-path path)
  (let loop ([path path] [rest '()])
    (let-values ([(base name dir?) (split-path path)])
      (cond
        [(eq? base 'relative) (cons name rest)]
        [(or (not (path? name)) (not path))
         (raise-type-error 'explode-relative-path
                           "relative-path in normal form"
                           path)]
        [else (loop base (cons name rest))]))))

;; telescope-exploded-path : (listof path) -> (listof path)
(define (telescope-exploded-path alop)
  (fold (lambda (this rest)
          (cons (if (null? rest)
                    this
                    (build-path (car rest) this))
                rest))
        null
        alop))

;; telescope-path : complete-path -> (listof complete-path)
(define (telescope-path path)
  (telescope-exploded-path (explode-path path)))

;; telescope-relative-path : relative-path -> (listof relative-path)
(define (telescope-relative-path path)
  (telescope-exploded-path (explode-relative-path path)))

;; path=?/2 : (or path #f 'relative) (or path #f 'relative) -> boolean
(define (path=?/2 path1 path2)
  (or (and (not path1) (not path2))
      (and (eq? path1 'relative) (eq? path2 'relative))
      (and (path? path1)
           (path? path2)
           (let-values ([(base1 name1 dir1?) (split-path path1)]
                        [(base2 name2 dir2?) (split-path path2)])
             (and (or (and (symbol? name1) (symbol? name2) (eq? name1 name2))
                      (and (path? name1) (path? name2)
                           (bytes=? (path->bytes name1) (path->bytes name2))))
                  (path=?/2 base1 base2))))))

;; path=? : path path ... -> boolean
;; determines whether two paths contain exactly the same elements
(define (path=? path1 path2 . paths)
  (andmap (lambda (path2)
            (path=?/2 path1 path2))
          (cons path2 paths)))

;; path-normalized=? : path path ... -> boolean
;; determines whether two paths refer to the same normalized path
(define (path-normalized=? path1 path2 . paths)
  (apply bytes=? (map (compose path->bytes normalize-path)
                      (cons path1 (cons path2 paths)))))

;; directory-list/all : [(or string path)] -> (listof relative-path)
;; returns the list of all files and subdirectories, relative to base-dir
(define (directory-list/all [base-dir (current-directory)])
  (let all-from ([dir base-dir] [prefix #f])
    (append-map (lambda (p)
                  (let ([p* (if prefix (build-path prefix p) p)]
                        [entry (build-path dir p)])
                    (if (directory-exists? entry)
                        (cons p* (all-from entry p*))
                        (list p*))))
                (directory-list dir))))

;; empty-directory? : (or string path) -> boolean
(define (empty-directory? p)
  (and (directory-exists? p)
       (null? (directory-list p))))

;; dirname : path -> path
(define (dirname p)
  (let-values ([(parent name must-be-dir?) (split-path p)])
    (cond
      [(not parent) (build-path p)]
      [(eq? parent 'relative) (build-path 'same)]
      [else parent])))

;; basename : path -> relative-path
(define (basename p)
  (let-values ([(parent name must-be-dir?) (split-path p)])
    (if (symbol? name)
        (build-path name)
        name)))

(provide/contract
 [relative-path/c contract?]
 [relative-file-path/c contract?]
 [relative-directory-path/c contract?]
 [complete-path/c contract?]
 [complete-file-path/c contract?]
 [complete-directory-path/c contract?])

;; TODO: can we make these contracts a little more precise? -- namely, the
;;       relative-paths must be simplified and expanded

(provide/contract
 [dirname ((or/c string? path?) . -> . path?)]
 [basename ((or/c string? path?) . -> . path?)]
 [empty-directory? ((or/c string? path?) . -> . boolean?)]
 [directory-list/all (() ((or/c string? path?)) . ->* . (listof relative-path/c))]
 [path->relative-path (((or/c string? path?))
                       ((or/c string? path?))
                       . ->* .
                       relative-path/c)]
 [explode-relative-path (relative-path/c . -> . (listof path?))]
 [telescope-path (complete-path/c . -> . (listof complete-path/c))]
 [telescope-relative-path (relative-path/c . -> . (listof relative-path/c))]
 [path=? ((path? path?) (listof path?) . ->* . boolean?)]
 [path-normalized=? ((path? path?) (listof path?) . ->* . boolean?)]
 )
