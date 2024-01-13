;; Property-based testing extension for SRFI 64.
;; SPDX-License-Identifier: MIT
;; Copyright 2024 Antero Mejr <antero@mailbox.org>

(define-library (property-test)
  (import (scheme base)
          (scheme case-lambda)
          (scheme complex)
          (scheme write)
          (only (srfi 1) iota)
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
    ;; May be implementation-dependent, but should be reasonably high numbers.

    ;; Number of property tests to run by default.
    (define default-runs 100)
    ;; Maximum number size for random generators.
    (define max-int 1000000000000000001)
    ;; Maximum size for random bytevector/vector/lists generators.
    (define max-size 1001)
    ;; Change to 127 for implementations that only support ASCII.
    (define max-char 1114111) ;unicode max

    ;; Generator procedures

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
              (gmap integer->char (make-random-integer-generator 0 max-char))))

    (define (complex-generator)
      (gcons* (make-rectangular 0.0 0.0)
              (make-rectangular 0 0)
              (make-rectangular 1 1)
              (make-rectangular 1.1 1.1)
              (make-rectangular -1 -1)
              (make-rectangular -1.1 -1.1)
              (make-rectangular +inf.0 +inf.0)
              (make-rectangular -inf.0 -inf.0)
              (make-rectangular +nan.0 +nan.0)
              ;; omit (-nan.0 -nan.0) as there is no significance in Gauche.
              (make-random-rectangular-generator (- max-int) max-int
                                                 (- max-int) max-int)))

    (define (exact-generator)
      ;; Not including -0
      (gcons* 0 1 -1 (make-random-integer-generator (- max-int) max-int)))

    (define (exact-complex-generator)
      (gcons* (make-rectangular 0 0)
              (make-rectangular 1 1)
              (make-rectangular -1 -1)
              (gmap make-rectangular
                    (make-random-integer-generator (- max-int) max-int)
                    (make-random-integer-generator (- max-int) max-int))))

    (define (inexact-complex-generator)
      (gcons* (make-rectangular 0.0 0.0)
              (make-rectangular 1.1 1.1)
              (make-rectangular -1.1 -1.1)
              (make-rectangular +inf.0 +inf.0)
              (make-rectangular -inf.0 -inf.0)
              (make-rectangular +nan.0 +nan.0)
              ;; (make-rectangular -nan.0 -nan.0)
              (make-random-rectangular-generator (- max-int) max-int
                                                 (- max-int) max-int)))

    (define (inexact-generator)
      ;; Not including -0.0 or -nan.0
      (gcons* 0.0 1.1 -1.1 +inf.0 -inf.0 +nan.0
       (make-random-real-generator (- max-int) max-int)))

    (define (integer-generator) (exact-generator))

    (define (number-generator)
      (gcons* 0.0 0  1 -1 1.1 -1.1 +inf.0 -inf.0 +nan.0 -nan.0
              (inexact-generator)))

    (define (rational-generator)
      (gcons* 0 0.0 -1.1 1.1 -1 1
              (make-random-real-generator (- max-int) max-int)))

    (define (real-generator) (inexact-generator))

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
        ((gen) (gmap (lambda (len)
                       (generator->vector gen len))
                     (make-random-integer-generator 0 max-size)))
        ((gen max-length) (gmap (lambda (len)
                                  (generator->vector gen len))
                                (make-random-integer-generator 0 max-length)))))

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
      (for-each (lambda (n)
                  (let ((args (map (lambda (gen) (gen)) generators)))
                    (test-result-set! runner 'property-test-arguments args)
                    (test-result-set! runner 'property-test-iteration n)
                    (test-result-set! runner 'property-test-iterations runs)
                    (test-assert (apply property args))))
                (iota runs)))

    (define (prop-test-error type property generators runner runs)
      (for-each (lambda (n)
                  (let ((args (map (lambda (gen) (gen)) generators)))
                    (test-result-set! runner 'property-test-arguments args)
                    (test-result-set! runner 'property-test-iteration n)
                    (test-result-set! runner 'property-test-iterations runs)
                    (test-error type (apply property args))))
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
