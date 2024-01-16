;; Tests for property-test library.
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

(import (scheme base)
        (scheme complex)
        (scheme list)
        (scheme read)
        (srfi 18)
        (srfi 27)
        (srfi 36)
        (srfi 64)
        (srfi 158)
        (srfi 194)
        (property-test))

(test-begin "property-test")

(define (three x) 3)
(define (wrong-three x) x)
(define (three-property x) (= (three x) 3))
(define (wrong-three-property x) (= (wrong-three x) 3))
(define (error-three-property x) (string-append 1 2))
(define (make-read-error x) (read (open-input-string (string-append ")" x))))
(define (make-read-error-property x) (symbol? (make-read-error x)))
(define (slow-boolean-generator) (gmap (lambda (x)
                                         (thread-sleep! 100000)
                                         x)
                                       (boolean-generator)))

(test-group "test-property"
  (test-property three-property (list (integer-generator)))
  (test-property three-property (list (real-generator)))
  (test-property three-property (list (integer-generator)) 10))

(test-group "test-property-expect-fail"
  (test-property-expect-fail wrong-three-property (list (integer-generator)))
  (test-property-expect-fail wrong-three-property (list (integer-generator)) 10))

(test-group "test-property-skip" ; shouldn't run
  (test-property-skip error-three-property (list (slow-boolean-generator)))
  (test-property-skip error-three-property (list (slow-boolean-generator)) 10))

(test-group "test-property-error"
  (test-property-error error-three-property (list (integer-generator)))
  (test-property-error error-three-property (list (integer-generator)) 10))

(test-group "test-property-error-type"
  (test-property-error-type &read-error make-read-error-property
                            (list (string-generator))))

(test-group "test-property_with-2-arguments"
  (test-property (lambda (x y)
                   (and (boolean? x) (integer? y)))
                 (list (boolean-generator) (integer-generator))))

(test-group "boolean-generator"
  (test-property boolean? (list (boolean-generator))))

(test-group "bytevector-generator"
  (test-property bytevector? (list (bytevector-generator))))

(test-group "char-generator"
  (test-property char? (list (char-generator))))

(test-group "complex-generator"
  (test-property complex? (list (complex-generator))))

(test-group "exact-generator"
  (test-property exact? (list (exact-generator))))

(cond-expand (exact-complex
              (test-group "exact-complex-generator"
                (test-property (lambda (x)
                                 (and (complex? x)
                                      (exact? (real-part x))
                                      (exact? (imag-part x))))
                               (list (exact-complex-generator)))))
             (else))

(test-group "inexact-generator"
  (test-property inexact? (list (inexact-generator))))

(test-group "inexact-complex-generator"
  (test-property (lambda (x)
                   (and (complex? x)
                        (inexact? (real-part x))
                        (inexact? (imag-part x))))
                 (list (inexact-complex-generator))))

(test-group "integer-generator"
  (test-property integer? (list (integer-generator))))

(test-group "number-generator"
  (test-property number? (list (number-generator))))

(test-group "rational-generator"
  (test-property rational? (list (rational-generator))))

(test-group "real-generator"
  (test-property real? (list (real-generator))))

(test-group "string-generator"
  (test-property string? (list (string-generator))))

(test-group "symbol-generator"
  (test-property symbol? (list (symbol-generator))))

(test-group "list-generator-of"
  (test-property (lambda (x)
                   (and (list? x) (<= (length x) 1001)
                        (every integer? x)))
                 (list (list-generator-of (integer-generator)))))

(test-group "pair-generator-of"
  (test-property (lambda (x)
                   (pair? x) (integer? (car x)) (boolean? (cdr x)))
                 (list (pair-generator-of (integer-generator)
                                          (boolean-generator)))))

(test-group "procedure-generator-of"
  (test-property (lambda (x)
                   (procedure? x) (integer? (x)))
                 (list (procedure-generator-of (integer-generator)))))

(test-group "vector-generator-of"
  (test-property (lambda (x)
                   (vector? x)
                   (<= (vector-length x) 1001)
                   (every integer? (vector->list x)))
                 (list (vector-generator-of (integer-generator)))))

(test-group "determinism"
  (let ((rand (make-random-source)))
    (current-random-source rand)
    (let ((lst1 (generator->list (boolean-generator) 1001))
          (rand2 (make-random-source)))
      (current-random-source rand2)
      (let ((lst2 (generator->list (boolean-generator) 1001)))
        (test-assert (equal? lst1 lst2))))))

(test-end)
