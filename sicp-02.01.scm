;; Structure & Interpretation of Computer Programs, Chapter 2
;; BUILDING ABSTRACTIONS WITH DATA

(use numbers)

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

;; Exercise 2.1. Define a better version of `make-rat` that handles both positive and negative
;; arguments. `Make-rat` should normalize the sign so that if the rational number is positive, both
;; the numerator and denominator are positive, and if the rational number is negative, only the
;; numerator is negative.

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


;; Exericse 2.3. Implement a representation for rectangles in a plane. In terms of your constructors
;; and selectors, create procedures that compute the perimeter and area of a given rectangle. Now
;; implement a different representation for rectangles. Can you design your system with suitable
;; abstraction barriers , so that the same perimeter and area procedures will work using either
;; representation?

;; This function takes a slope and returns a function to mutate a point such that
;; a new segment can be constructed from one end of the line segment to construct
;; two line segments that are perpendicular, intersect at one end, and together
;; form a rectangle


;; 2.1.3 What is Meant by Data
;; --------------------------
;;
;; In the previous section, we introduced a data structure through three methods: a constructor and
;; two selectors. Generally speaking, we can consider data as defined by some collection of
;; selectors and constructors, along with certain pre-conditions (e.g., that a rational number
;; cannot have zero in the denominator).
;;
;; For example, a `pair` as a data structure is defined by a set of language primitives: `cons`,
;; `car`, and `cdr` But we also could have defined them with our own methods:
;;
;;   (define (cons x y)
;;     (define (dispatch m)
;;       (cond ((= m 0) x)
;;       (cond ((= m 1) y)
;;       (else (error "argument not 0 or 1 -- CONS" m))))
;;     dispatch)
;;
;;   (define (car z) (z 0))
;;   (define (cdr z) (z 1))
;;
;; Note that this is a data representation of functions, all three return functions and thus is a
;; procedural representation of a data structure.

;; Exercise 2.6. In case representing pairs as procedures wasn't mind-boggling enough, consider
;; that, in a language that can manipulate procedures, we can get by without numbers (at least
;; insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as
;; listed below. Define `one` and `two` directly (not in terms of zero and add-1.

;; 2.1.4 Extended Exercise: Interval Arithmetic
;; --------------------------------------------
;;
;; Resistance values for electrical current can be calculated according to the following formula:
;;
;;    R = 1 / ( 1/ R_1) + (1 / R_2) )
;;
;; Alyssa B Hacker's idea is to implement interval arithmetic as a set of arithmetic operations
;; combining intervals. The result of adding to an interval should be a new interval. She postulates
;; the existance of an abstract object called `interval` that has two endpoints, a lower bound and an
;; upper bound. Further, given two endpoints, she can construct an interval with the data constructor
;; `make-interval`. She also postulates that you can add two intervals, with the lower bound being the
;; sum of the two lower bounds.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.7. Alyssa's program is incomplete because she has not specified the implementation of the
;; interval abstraction. Here is a definition of the interval constructor:

;; Define selectors `upper-bound` and `lower-bound` to complete the implementation

