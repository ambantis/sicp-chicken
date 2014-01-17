;; Structure & Interpretation of Computer Programs, Chapter 2
;; BUILDING ABSTRACTIONS WITH DATA

;;
;; 2.1 INTRODUCTION TO DATA ABSTRACTION
;; ====================================
;;
;; The previous chapter dealt with how to abstract out implementation details from a
;; procedure, a procedural abstraction. Here, we deal with the concept of data abstraction. The
;; basic idea is to structure programs to operate on data elements so that they deal with data
;; abstractions.
;;
;; 2.1.1 Example, Arithmetic Operations for Rational Numbers
;; ---------------------------------------------------------
;;
;; We want to do arithmetic with rational numbers, add, subtract, multiply, divide, and test whether
;; two rational numbers are equal. We want to have the following constructor and getters:
;;
;; (make-rat <n> <d>)
;; (numer <x>)
;; (denom <x>)
;;
;; With these definitions in place, we can do the four operations listed below. To enable
;;

(define (make-rat-v0 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define  (print-rat x)
  (printf "~S/~S~N"
          (numer x)
          (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Ecercise 2.1. Define a better version of `make-rat` that handles both positive and negative
;; arguments. `Make-rat` should normalize the sign so that if the rational number is positive, both
;; the numerator and denominator are positive, and if the rational number is negative, only the
;; numerator is negative.

(define (make-rat n d)
  (define sign
    (cond ((and (< 0 n) (> 0 d)) -1)
          ((and (> 0 n) (< 0 d)) -1)
          (else 1)))
  (let ((g (gcd n d)))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))
