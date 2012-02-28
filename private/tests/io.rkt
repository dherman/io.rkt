#lang racket

(require racket/async-channel
         racket/match
         rackunit
         (except-in (planet dherman/test:1/test) in-this-directory)
         mzlib/etc
         racket/port
         file/gunzip
         racket/runtime-path
         "../../main.rkt")

(require/expose "../../main.rkt" (ones-mask make-filter-input-port/debug))

(define-runtime-path here ".")

(define-syntax-rule (in-this-directory e1 e2 ...)
  (parameterize ([current-directory here])
    e1 e2 ...))

(define test:ones-mask
  (test-suite
   "ones-mask"
   (test-case "zero"
              (check = (ones-mask 0) 0))
   (test-case "one through ten"
              (let loop ([i 1])
                (unless (>= i 10)
                  (let ([ans (ones-mask i)]
                        [expected (build-string (* 8 i) (lambda (x) #\1))])
                    (check string=? (format "~b" ans) expected)))))))

(define test:bit-set?
  (test-suite
   "bit-set?"
   (test-case ""
              (check-true (bit-set? 0 #b1)))
   (test-case ""
              (check-true (bit-set? 1 #b10)))
   (test-case ""
              (check-true (bit-set? 2 #b100)))
   (test-case ""
              (check-true (bit-set? 9 #b1000000000)))
   (test-case ""
              (check-false (bit-set? 0 #b11111111110)))
   (test-case ""
              (check-false (bit-set? 1 #b11111111101)))
   (test-case ""
              (check-false (bit-set? 2 #b11111111011)))
   (test-case ""
              (check-false (bit-set? 9 #b10111111111)))
   ))

(define test:stretch-bytes
  (test-suite
   "stretch-bytes"
   (test-case "stretch big-endian"
              (check bytes=? (stretch-bytes #"dave" 8 #t (char->integer #\X))
                     #"XXXXdave"))
   (test-case "stretch small-endian"
              (check bytes=? (stretch-bytes #"dave" 8 #f (char->integer #\X))
                     #"daveXXXX"))
   (test-case "stretch big-endian with default fill"
              (check bytes=? (stretch-bytes #"dave" 8 #t)
                     #"\0\0\0\0dave"))
   (test-case "stretch small-endian with default fill"
              (check bytes=? (stretch-bytes #"dave" 8 #f)
                     #"dave\0\0\0\0"))
   (test-case "stretch none big-endian"
              (check bytes=? (stretch-bytes #"dave" 4 #t)
                     #"dave"))
   (test-case "stretch none small-endian"
              (check bytes=? (stretch-bytes #"dave" 4 #f)
                     #"dave"))
   ;; TODO: why isn't this an exn:fail:contract?
   (test-case "stretch not enough big-endian"
              (check-exn exn:fail?
                         (lambda ()
                           (stretch-bytes #"dave" 2 #t))))
   (test-case "stretch not enough small-endian"
              (check-exn exn:fail?
                         (lambda ()
                           (stretch-bytes #"dave" 3 #f))))
   ))

(define (integer->bytes->integer n big-endian?)
  (bytes->integer (integer->integer-bytes n 4 #f big-endian?)
                  #t
                  big-endian?))

(define test:bytes->integer/unsigned
  (test-suite
   "bytes->integer (unsigned)"
   (test-case "No bytes - big-endian"
              (check = (bytes->integer (bytes) #f #t) 0))
   (test-case "No bytes - small-endian"
              (check = (bytes->integer (bytes) #f #f) 0))
   (test-case "Simple test 1 - big-endian"
              (check = (bytes->integer (bytes 2 1) #f #t) 513))
   (test-case "Simple test 1 - small-endian"
              (check = (bytes->integer (bytes 2 1) #f #f) 258))
   (test-case "Reverse endianness"
              (check = (bytes->integer (bytes 24 28 200 12) #f #t)
                     (bytes->integer (bytes 12 200 28 24) #f #f)))
   (test-case "compatible with integer->integer-bytes"
              (check = (integer->bytes->integer 2461357 #t) 2461357))
   ))

(define test:bytes->integer/signed
  (test-suite
   "bytes->integer (signed)"
   (test-case "-1 in one byte"
              (check = (bytes->integer #"\377" #t #t) -1))
   (test-case "-1 in two bytes"
              (check = (bytes->integer #"\377\377" #t #t) -1))
   (test-case "-1 in four bytes"
              (check = (bytes->integer #"\377\377\377\377" #t #t) -1))
   (test-case "-20"
              (check = (bytes->integer #"\377\354" #t #t) -20))
   (test-case "-38274773"
              (check = (bytes->integer #"\375\267\371+" #t #t) -38274773))
   (test-case "-3333333333333333333"
              (check =
                     (bytes->integer #"\321\275\236\376|\262\252\253" #t #t)
                     -3333333333333333333))
   ))

(define test:integer->bytes/unsigned
  (test-suite
   "integer->bytes (unsigned)"
   (test-case "1"
              (check bytes=? (integer->bytes 1 #f #t 1) #"\001"))
   (test-case "255"
              (check bytes=? (integer->bytes 255 #f #t 2) #"\000\377"))
   (test-case "3527688"
              (check bytes=? (integer->bytes 3527688 #f #t 4) #"\0005\324\b"))
   ))

(define test:integer->bytes/signed
  (test-suite
   "integer->bytes (signed)"
   (test-case "-1"
              (check bytes=? (integer->bytes -1 #t #t 1) #"\377"))
   (test-case "-255"
              (check bytes=? (integer->bytes -255 #t #t 2) #"\377\1"))
   (test-case "-3527688"
              (check bytes=? (integer->bytes -3527688 #t #t 4) #"\377\312+\370"))
   ))

(struct running-script (thread channel port file-port))

;; interrupt-script : running-script -> any
;; interferes with a running script by closing its underlying file port
(define (interrupt-script running)
  (close-input-port (running-script-file-port running)))

;; start-script : (union path string) -> running-script
;; NOTE: *only* works with big.zip and broken.zip
(define (start-script path)
  (let* ([history (make-async-channel 100)]
         [file-in (open-input-file path)]
         [filter-in (make-filter-input-port/debug inflate file-in #f history)])
    (file-position file-in #x26)
    (running-script
     (thread
      (lambda ()
        (let ([handler (lambda (e)
                         (sleep 1)
                         (async-channel-put history `(exn main-thread ,e)))])
          (with-handlers ([exn? handler])
            (async-channel-put history 'trying-to-read)
            (let loop ([i 0])
              (when (> i 4440)
                (async-channel-put history `(reading-line ,i)))
              (cond
                [(read-line filter-in 'any)
                 => (lambda (line)
                      (unless (eof-object? line)
                        (loop (add1 i))))]))
            (async-channel-put history 'done-reading)))))
     history
     filter-in
     file-in)))

;; script-wait : running-script -> (listof event)
;; waits for a running script to finish, cleans up, and returns the transcript
(define (script-wait running)
  (thread-wait (running-script-thread running))
  (close-input-port (running-script-port running))
  (close-input-port (running-script-file-port running))
  (let loop ([result '()])
    (let ([event (async-channel-try-get (running-script-channel running))])
      (if (not event)
          (reverse result)
          (loop (cons event result))))))

;; transcript-element=? : event event -> boolean
(define (transcript-element=? act exp)
  (match exp
    [(list 'reading-line i)
     (match act
       [(list 'reading-line j) (= i j)]
       [_ #f])]
    [(list 'exn context correct-type?)
     (match act
       [(list 'exn context* val)
        (and (eq? context context*)
             (correct-type? val))]
       [_ #f])]
    [_ (eq? exp act)]))

;; transcript=? : (listof event) (listof event) -> boolean
(define (transcript=? act exp)
  (and (= (length act) (length exp))
       (andmap transcript-element=? act exp)))

;; check-script : running-script (listof event) -> ?
(define (check-script running expected)
  (let ([transcript (script-wait running)])
    (check transcript=? transcript expected
           (format "expected: ~v, actual: ~v" expected transcript))))

(define (make-broken-copy from to k)
  (with-input-from-file from
    (lambda ()
      (let ([in (make-limited-input-port (current-input-port) k)])
        (with-output-to-file to
          (lambda ()
            (copy-port in (current-output-port))))))))

(define test:make-filter-input-port
  (test-suite
   "make-filter-input-port tests (ooooh.. concurrency..)"
   (test-case "relatively big file"
              (in-this-directory
               (check-script
                (start-script (build-path "examples" "big.zip"))
                '(trying-to-read
                  done-transform
                  (reading-line 4441)
                  (reading-line 4442)
                  (reading-line 4443)
                  (reading-line 4444)
                  done-reading))))
   (test-case "interrupting mid-transform"
              (in-this-directory
               (let ([running (start-script (build-path "examples" "big.zip"))])
                 (interrupt-script running)
                 (check-script running
                               `(trying-to-read
                                 (exn transformer ,exn:fail?)
                                 (exn main-thread ,exn:fail?))))))
   (test-case "exception mid-transform"
              (in-this-directory
               (in-new-directory "sandbox"
                                 (make-broken-copy (build-path 'up "examples" "big.zip")
                                                   "broken.zip"
                                                   42440)
                                 (check-script
                                  (start-script "broken.zip")
                                  `(trying-to-read
                                    (exn transformer ,exn:fail?)
                                    (exn main-thread ,exn:fail?))))))
   ))

(define io-tests
  (test-suite
   "All io.ss tests"
   test:ones-mask
   test:bit-set?
   test:stretch-bytes
   test:bytes->integer/unsigned
   test:bytes->integer/signed
   test:integer->bytes/unsigned
   test:integer->bytes/signed
   test:make-filter-input-port
   ))

(provide io-tests)
