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


;; 2.1.2 Abstraction Barriers
;; --------------------------
;;
;; Generally speaking, we should identify the common set of data set operations that can be applied
;; to all instances of a data set type and use only those operations. One question would be when to
;; implement a reduction algorithm, in the constructor or when an operation is called on the number.
;; One advantage of having an abstract interface is that this choice can be deferred.

;; Exercise 2.2. Consider the problem of representing line segments in a plane. Each segment is
;; represented as a pair of points: a starting point and an ending point. Define a constructor
;; `make-segment` and selectors `start-segment` and `end-segment` that define the representation of
;; segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the
;; x-coordinate and the y-coordinate. Accordingly, specify a constructor `make-point` and selectors
;; `x-point` and `y-point` that define this representation. Finally, using your selectors and
;; constructors, define a procedure `midpoint-segment` that takes a line segment as argument and
;; returns its midpoint (the point whose coordinates are the average of the coordinates of the
;; endpoints. To try your procedures, you'll need a way to print points:

(define (print-point p)
  (printf "(~S,~S)~N"
          (x-point p)
          (y-point p)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (avg a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let* ((p1 (start-segment s))
         (p2 (end-segment s))
         (x1 (x-point p1))
         (x2 (x-point p2))
         (y1 (y-point p1))
         (y2 (y-point p2)))
    (make-point (avg x1 x2) (avg y1 y2))))



