#lang racket/base

(require (except-in (planet cobbe/contract-utils:1/contract-utils) predicate/c)
         racket/contract
         racket/async-channel
         racket/path)

;; =============================================================================
;; UTILITIES
;; =============================================================================

;; with-temporary-file
;; creates a temporary file and automatically deletes it when finished
(define-syntax-rule (with-temporary-file file (args ...) e1 e2 ...)
  (let ([file (make-temporary-file args ...)])
    (dynamic-wind
     void
     (lambda () e1 e2 ...)
     (lambda ()
       (when (file-exists? file)
         (delete-file file))))))

;; seekable-port? : port -> boolean
(define (seekable-port? port)
  (and (file-stream-port? port)
       (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)]
                       [exn:fail:contract? (lambda (exn) #f)])
         (and (file-position port (file-position port))
              #t))))

;; This procedure must create a separate thread because clients may read
;; from the input port lazily, but the transform procedure eagerly reads
;; all input from the underlying port.

;; However, if an exception is raised by the transformer, that exception
;; should be propagated to the client thread. So we catch the exception,
;; send it to a shared channel, and hang onto it to be rereaised in the
;; client thread when the client reaches that point in the input.

(struct filter-error (value))

(define (make-filter-input-port/debug transform in close-orig? history)
  (let-values ([(pipe-input pipe-output) (make-pipe)])
    (let* ([chan (make-channel)]
           [handler (lambda (exn)
                      (close-output-port pipe-output)
                      (channel-put chan (filter-error exn))
                      (when history
                        (async-channel-put history `(exn transformer ,exn))))])
      (thread (lambda ()
                (with-handlers ([(lambda (exn) #t) handler])
                  (transform in pipe-output)
                  (when history
                    (async-channel-put history 'done-transform))
                  (close-output-port pipe-output))))
      (make-input-port (object-name in)
                       (lambda (buffer)
                         (let ([count (read-bytes-avail!* buffer pipe-input)])
                           (cond
                             [(and (eof-object? count) (channel-try-get chan))
                              => (lambda (err)
                                   (raise (filter-error-value err)))]
                             [else count])))
                       #f
                       (lambda ()
                         (close-input-port pipe-input)
                         (when close-orig? (close-input-port in)))))))

;; make-filter-input-port : (input-port output-port -> any) [input-port] [boolean] -> input-port
(define (make-filter-input-port transform [in (current-input-port)] [close-orig? #f])
  (make-filter-input-port/debug transform in close-orig? #f))

;; TODO: make-filter-output-port

;(define (make-filter-output-port transform [out (current-output-port)])
;  ...)

(define exact-integer/c
  (and/c integer? exact?))

(define (log-base-2 n)
  (/ (log n) (log 2)))

(define (next-power-of-2 n)
  (inexact->exact
   (expt 2 (ceiling (log-base-2 n)))))

(define (next-multiple-of-8 n)
  (inexact->exact
   (* 8 (ceiling (/ n 8)))))

;; =============================================================================
;; BIT TWIDDLING
;; =============================================================================

;; ones-mask : nat -> exact-integer
;; creates an integer of n bytes all set to #xff
(define (ones-mask n)
  (sub1 (arithmetic-shift 1 (* 8 n))))

;; bit-set? : nat exact-integer -> boolean
;; determines whether the given bit (zero-indexed) is enabled
(define (bit-set? i n)
  (not (zero? (bitwise-and n (arithmetic-shift 1 i)))))

;; stretch-bytes : bytes nat [boolean] [byte] -> bytes
(define (stretch-bytes bytes len [big-endian? (system-big-endian?)] [fill-byte 0])
  (let ([real-len (bytes-length bytes)])
    (cond
      [(= real-len len) bytes]
      [(< real-len len)
       (let ([extra (make-bytes (- len real-len) fill-byte)])
         (if big-endian?
             (bytes-append extra bytes)
             (bytes-append bytes extra)))]
      [else (error 'stretch-bytes "too many bytes: ~a" real-len)])))

;; negative-bytes? : bytes boolean -> boolean
;; tests if a byte string represents a negative two's-complement integer
(define (negative-bytes? bytes start-k end-k big-endian?)
  (bit-set? 7 (bytes-ref bytes
                         (if big-endian? start-k (sub1 end-k)))))

;; bytes->integer : bytes boolean [boolean] [nat] [nat] -> exact-integer
(define (bytes->integer bytes signed? [big-endian? (system-big-endian?)] [start-k 0] [end-k (bytes-length bytes)])
  (let ([unsigned (bytes->unsigned bytes start-k end-k big-endian?)])
    (if (and signed? (negative-bytes? bytes start-k end-k big-endian?))
        (- (add1 (bitwise-xor unsigned (ones-mask (- end-k start-k)))))
        unsigned)))

;; bytes->unsigned : bytes nat nat boolean -> nat
;; interprets a byte string as an unsigned integer
(define (bytes->unsigned bytes start-k end-k big-endian?)
  (let* ([end (bytes-length bytes)]
         [goal (if big-endian? (sub1 start-k) end-k)]
         [step (if big-endian? sub1 add1)])
    (let loop ([i (if big-endian? (sub1 end-k) start-k)] [n 0] [mult 1])
      (if (= i goal)
          n
          (loop (step i)
                (+ n (* mult (bytes-ref bytes i)))
                (* mult 256))))))

;; fits? : exact-integer nat boolean -> boolean
;; determines whether num fits in n-bytes bytes
(define (fits? num n-bytes signed?)
  (if signed?
      (or (and (negative? num)
               (bit-set? (sub1 (* n-bytes 8)) num)
               ;; TODO: is that right?
               (< (- num) (arithmetic-shift 1 (* 8 n-bytes))))
          (and (not (negative? num))
               (not (bit-set? (sub1 (* n-bytes 8)) num))
               (< num (arithmetic-shift 1 (* 8 n-bytes)))))
      (and (not (negative? num))
           (< num (arithmetic-shift 1 (* 8 n-bytes))))))

;; minimum-bytes : exact-integer -> nat
;; the minimum number of bytes needed to encode n in two's-complement
(define (minimum-bytes n)
  (let ([bit-count (next-multiple-of-8
                    (ceiling (log-base-2 (add1 (abs n)))))])
    (next-power-of-2
     (if (or (and (negative? n) (not (bit-set? (sub1 bit-count) n)))
             (and (not (negative? n)) (bit-set? (sub1 bit-count) n)))
         (add1 (/ bit-count 8))
         (/ bit-count 8)))))

;; integer->bytes : exact-integer boolean [(optional nat)] [boolean] -> bytes
(define (integer->bytes n signed? [big-endian? (system-big-endian?)] [size-n #f])
  (when (and size-n (not (fits? n size-n signed?)))
    (raise
     (exn:fail:contract
      (format
       "integer-bytes: integer does not fit into ~a signed byte~a: ~a"
       size-n (if (= size-n 1) "" "s") n))))
  (let* ([size-n (or size-n (next-power-of-2 (minimum-bytes n)))]
         [bytes (make-bytes size-n (if (negative? n) 255 0))]
         [start-k (if big-endian? (sub1 size-n) 0)]
         [end-k (if big-endian? -1 size-n)]
         [step (if big-endian? sub1 add1)])
    (let loop ([n n] [i start-k])
      (if (= i end-k)
          bytes
          (begin
            (bytes-set! bytes i (bitwise-and n #xff))
            (loop (arithmetic-shift n -8) (step i)))))))

;; TODO: integer->bytes!

;; =============================================================================
;; INPUT
;; =============================================================================

;; skip-bytes : nat [input-port] -> any
;; skips the given number of bytes from an input port
(define (skip-bytes k [in (current-input-port)])
  (read-bytes k in)
  (void))

;; read-c-string : [input-port] -> bytes
;; reads a byte string until reaching #\nul or EOF
(define (read-c-string [in (current-input-port)])
  (let loop ([result null])
    (let ([b (read-byte in)])
      (if (or (eof-object? b) (zero? b))
          (list->bytes (reverse result))
          (loop (cons b result))))))

;; read-c-string! : bytes [input-port] [nat] [nat] -> (or eof nat)
;; reads a byte string destructively until reaching #\nul or EOF
(define (read-c-string! b [in (current-input-port)] [s-k 0] [e-k (bytes-length b)])
  (let loop ([read 0] [i s-k])
    (let ([byte (read-byte in)])
      (cond
        [(and (zero? read) (eof-object? byte)) byte]
        [(or (eof-object? byte)
             (zero? byte)
             (= i e-k))
         read]
        [else
         (bytes-set! i byte)
         (loop (add1 read) (add1 i))]))))

;; read-integer : nat boolean [input-port] [boolean] -> exact-integer
;; reads a two's-complement integer from an input port
(define (read-integer k signed? [in (current-input-port)] [big-endian? (system-big-endian?)])
  (bytes->integer (read-bytes k in) signed? big-endian?))

;; peek-integer : nat boolean [input-port] [boolean] -> exact-integer
;; reads a two's-complement integer from an input port without advancing
(define (peek-integer k signed? [in (current-input-port)] [big-endian? (system-big-endian?)])
  (bytes->integer (peek-bytes k 0 in) signed? big-endian?))

;; read-chars : nat [input-port] -> (listof char)
;; reads a fixed number of characters from an input port
(define (read-chars k [in (current-input-port)])
  (build-list k (lambda (i) (read-char in))))

;; peek-chars : nat [input-port] -> (listof char)
;; reads a fixed number of characters from an input port without advancing
(define (peek-chars k [in (current-input-port)])
  (string->list (peek-string k 0 in)))

;; read-lines : [input-port mode-symbol] -> (listof string)
(define (read-lines [in (current-input-port)] [mode-symbol 'linefeed])
  (let loop ([result '()])
    (let ([line (read-line in mode-symbol)])
      (if (eof-object? line)
          (reverse result)
          (loop (cons line result))))))

;; =============================================================================
;; OUTPUT
;; =============================================================================

;; write-c-string : bytes [output-port] [nat] [nat] -> any
;; writes a C-style (#\nul-terminated) string to an output port
(define (write-c-string b [out (current-output-port)] [s-k 0] [e-k (bytes-length b)])
  (write-bytes b out s-k e-k)
  (write-byte 0 out))

;; write-integer : exact-integer boolean [output-port] [boolean] [(optional nat)] -> any
;; writes the binary representation of an integer to an output port
(define (write-integer n signed? [out (current-output-port)] [big-endian? (system-big-endian?)] [size-n #f])
  (let ([bytes (integer->bytes n signed? big-endian? size-n)])
    (write-bytes bytes out)))

;; write-chars : (listof char) [output-port] -> any
;; writes a sequence of characters to an output port
(define (write-chars chars [out (current-output-port)])
  (for ([c chars])
    (write-char c out)))

;; write-lines : (listof string) [output-port] -> any
;; writes a sequence of strings to an output port
(define (write-lines lines [out (current-output-port)])
  (for ([line lines])
    (display line out)
    (newline out)))

(provide with-temporary-file)

(define mode-symbol/c
  (symbols 'linefeed 'return 'return-linefeed 'any 'any-one))

(provide/contract
 [make-filter-input-port (((input-port? output-port? . -> . any))
                          (input-port?)
                          . ->* .
                          input-port?)]
 [stretch-bytes (([bytes bytes?]
                  [len (bytes) (and/c natural-number/c (>=/c (bytes-length bytes)))])
                 ([big-endian? boolean?]
                  [fill-byte byte?])
                 . ->i .
                 [_ bytes?])]
 [bit-set? (natural-number/c exact-integer/c . -> . boolean?)]
 [bytes->integer ((bytes? boolean?)
                  (boolean? natural-number/c natural-number/c)
                  . ->* .
                  exact-integer/c)]
 [integer->bytes ((exact-integer/c boolean?)
                  (boolean? (optional/c natural-number/c))
                  . ->* .
                  bytes?)]
 [seekable-port? (port? . -> . boolean?)]
 [skip-bytes ((natural-number/c)
              (input-port?)
              . ->* .
              any)]
 [read-chars ((natural-number/c)
              (input-port?)
              . ->* .
              (listof char?))]
 [peek-chars ((natural-number/c)
              (input-port?)
              . ->* .
              (listof char?))]
 [read-c-string (()
                 (input-port?)
                 . ->* .
                 bytes?)]
 [read-c-string! ((bytes?)
                  (input-port? natural-number/c natural-number/c)
                  . ->* .
                  (or/c eof-object? natural-number/c))]
 [read-integer ((natural-number/c boolean?)
                (input-port? boolean?)
                . ->* .
                exact-integer/c)]
 [read-lines (() (input-port? mode-symbol/c) . ->* . (listof string?))]
 [peek-integer ((natural-number/c boolean?)
                (input-port? boolean?)
                . ->* .
                exact-integer/c)]
 [write-chars (((listof char?))
               (input-port?)
               . ->* .
               any)]
 [write-integer ((exact-integer/c boolean?)
                 (output-port? boolean? (optional/c natural-number/c))
                 . ->* .
                 any)]
 [write-c-string ((bytes?)
                  (output-port? natural-number/c natural-number/c)
                  . ->* .
                  any)]
 [write-lines (((listof string?)) (output-port?) . ->* . any)])
