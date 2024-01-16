;; Property-based testing extension for SRFI 64.
;; SPDX-License-Identifier: MIT
;; Copyright 2024 Antero Mejr <antero@mailbox.org>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (property-test)
  (import (scheme base)
          (scheme case-lambda)
          (scheme complex)
          (srfi 1)
          (srfi 64)
          (srfi 158)
          (srfi 194))
  (export test-property test-property-expect-fail test-property-skip
          test-property-error test-property-error-type
          property-test-runner
          boolean-generator bytevector-generator
          char-generator complex-generator
          exact-generator exact-complex-generator
          inexact-generator inexact-complex-generator
          integer-generator number-generator rational-generator real-generator
          string-generator symbol-generator
          list-generator-of pair-generator-of procedure-generator-of
          vector-generator-of)
  (begin

    ;; Constants

    ;; These values may be implementation-dependent, but should be reasonably
    ;; high numbers.
    ;; Number of property tests to run by default.
    (define default-runs 100)
    ;; Maximum absolute value of a number for random generators.
    (define max-int 1000000000000000001)
    ;; Maximum size for random bytevector/vector/lists generators.
    (define max-size 1001)
    ;; Maximum character supported by integer->char.
    (define max-char (cond-expand (full-unicode #x10FFFF) (else 127)))

    ;; Omit values that are not distinguished in the implementation.
    (cond-expand
     (gauche ; Not distinguished: -0, -0.0, exact-complex
      (define special-number '(;; exact
                               0 1 -1
                               ;; inexact
                               0.0 0.5 -0.5 1.0 -1.0
                               ;; inexact-complex
                               0.0+1.0i 0.0-1.0i
                               0.5+0.5i 0.5-0.5i -0.5+0.5i -0.5-0.5i
                               1.0+1.0i 1.0-1.0i -1.0+1.0i -1.0-1.0i
                               ;; other
                               +inf.0 -inf.0 +nan.0 -nan.0)))
     (else
      (define special-number '(;; integer
                               0 -0 1 -1
                               ;; inexact
                               0.0 -0.0 0.5 -0.5 1.0 -1.0
                               ;; exact-complex
                               0+i 0-i 1+i 1-i -1+i -1-i
                               ;; inexact-complex
                               0.0+1.0i 0.0-1.0i -0.0+1.0i -0.0-1.0i
                               0.5+0.5i 0.5-0.5i -0.5+0.5i -0.5-0.5i
                               1.0+1.0i 1.0-1.0i -1.0+1.0i -1.0-1.0i
                               ;; other
                               +inf.0 -inf.0 +nan.0 -nan.0))))

    ;; Generator procedures

    (define (inexact-complex? x)
      (and (complex? x) (inexact? (imag-part x)) (inexact? (real-part x))))

    (define (exact-complex? x)
      (and (complex? x) (exact? (imag-part x)) (exact? (real-part x))))

    (define (boolean-generator)
      (gcons* #t #f (make-random-boolean-generator)))

    (define (bytevector-generator)
      (let ((gen (make-random-u8-generator)))
        (gcons* (bytevector)
                (gmap (lambda (len)
                        (apply bytevector (generator->list gen len)))
                      (make-random-integer-generator 0 max-size)))))

    (define (char-generator)
      (gcons* #\null
              (gmap integer->char
                    (gfilter (lambda (x)
                               (or (< x #xd800) (> x #xdfff)))
                             (make-random-integer-generator 0 max-char)))))

    (define (complex-generator)
      (gappend (gfilter complex? special-number)
               (make-random-rectangular-generator (- max-int) max-int
                                                  (- max-int) max-int)))

    (define (exact-generator)
      (gappend (gfilter exact? special-number)
               (make-random-integer-generator (- max-int) max-int)))

    (define (exact-complex-generator)
      (cond-expand (exact-complex
                    (gappend (gfilter exact-complex? special-number)
                             (gmap make-rectangular
                                   (make-random-integer-generator
                                    (- max-int) max-int)
                                   (make-random-integer-generator
                                    (- max-int) max-int))))
                   (else (error "Exact complex is not supported."))))

    (define (inexact-complex-generator)
      (gappend (gfilter inexact-complex? special-number)
               (make-random-rectangular-generator (- max-int) max-int
                                                  (- max-int) max-int)))

    (define (inexact-generator)
      (gappend (gfilter inexact? special-number)
               (make-random-real-generator (- max-int) max-int)))

    (define (integer-generator)
      (gappend (gfilter integer? special-number)
               (make-random-integer-generator (- max-int) max-int)))

    (define (number-generator)
      ;; TODO: May need to be modified for unusual implementation-specific
      ;; number types, like Kawa's quaternion.
      (gappend (apply generator special-number)
               (complex-generator)))

    (define (rational-generator)
      (gappend (gfilter rational? special-number)
               (make-random-real-generator (- max-int) max-int)))

    (define (real-generator)
      (gappend (gfilter real? special-number)
               (make-random-real-generator (- max-int) max-int)))

    (define (string-generator)
      (gcons* ""
              (gmap (lambda (n)
                      (generator->string (char-generator) n))
                    (make-random-integer-generator 1 max-size))))

    (define (symbol-generator)
      (gmap string->symbol (string-generator)))

    ;; Special generators for collection types

    (define list-generator-of
      (case-lambda
        ((gen)
         (gcons* '()
                 (gmap (lambda (len)
                         (generator->list gen len))
                       (make-random-integer-generator 1 max-size))))
        ((gen max-length)
         (gcons* '()
                 (gmap (lambda (len)
                         (generator->list gen len))
                       (make-random-integer-generator 1 max-length))))))

    (define pair-generator-of
      (case-lambda
        ((gen1) (gmap cons gen1 gen1))
        ((gen1 gen2) (gmap cons gen2 gen2))))

    (define (procedure-generator-of gen)
      ;; Generate variadic procedures that returns a value from a generator.
      ;; Useful for testing procedures that accept procedure arguments.
      (gmap (lambda (x)
              (lambda _
                x))
            gen))

    (define vector-generator-of
      (case-lambda
        ((gen)
         (gcons* (vector)
                 (gmap (lambda (len)
                         (generator->vector gen len))
                       (make-random-integer-generator 0 max-size))))
        ((gen max-length)
         (gcons* (vector)
                 (gmap (lambda (len)
                         (generator->vector gen len))
                       (make-random-integer-generator 0 max-length))))))

    ;; Runner

    (define (property-test-runner)
      ;; Implementation specific.
      ;; Some implementations do not support extended test runners.
      (let ((runner (test-runner-simple)))
        ;; (test-runner-on-test-end! runner property-test-runner-on-test-end)
        ;; (test-runner-on-group-end! runner property-test-runner-on-group-end)
        runner))

    ;; Test procedures

    (define (get-runner)
      (or (test-runner-current) (let ((runner (property-test-runner)))
                                  (test-runner-current runner)
                                  runner)))

    (define (prop-test property generators runner runs)
      (for-each
       (lambda (n)
         (test-assert
             (apply property
                    (let ((args (map (lambda (gen) (gen)) generators)))
                      (test-result-set! runner 'property-test-arguments args)
                      (test-result-set! runner 'property-test-iteration (+ n 1))
                      (test-result-set! runner 'property-test-iterations runs)
                      args))))
       (iota runs)))

    (define (prop-test-error type property generators runner runs)
      (for-each
       (lambda (n)
         (test-error
          type
          (apply property
                 (let ((args (map (lambda (gen) (gen)) generators)))
                   (test-result-set! runner 'property-test-arguments args)
                   (test-result-set! runner 'property-test-iteration (+ n 1))
                   (test-result-set! runner 'property-test-iterations runs)
                   args))))
       (iota runs)))

    (define test-property-error
      (case-lambda
        ((property generators)
         (prop-test-error #t property generators (get-runner) default-runs))
        ((property generators n)
         (prop-test-error #t property generators (get-runner) n))))

    (define test-property-error-type
      (case-lambda
        ((type property generators)
         (prop-test-error type property generators (get-runner) default-runs))
        ((type property generators n)
         (prop-test-error type property generators (get-runner) n))))

    (define test-property-skip
      (case-lambda
        ((property generators)
         (begin (test-skip default-runs)
                (prop-test property generators (get-runner) default-runs)))
        ((property generators n)
         (begin (test-skip n)
                (prop-test property generators (get-runner) n)))))

    (define test-property-expect-fail
      (case-lambda
        ((property generators)
         (begin (test-expect-fail default-runs)
                (prop-test property generators (get-runner) default-runs)))
        ((property generators n)
         (begin (test-expect-fail n)
                (prop-test property generators (get-runner) n)))))

    (define test-property
      (case-lambda
        ((property generators)
         (prop-test property generators (get-runner) default-runs))
        ((property generators n)
         (prop-test property generators (get-runner) n))))))
