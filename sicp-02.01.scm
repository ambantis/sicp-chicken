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

(define (len-segment s)
  (let* ((a (abs (- (x-point (start-segment s)) (x-point (end-segment s)))))
         (b (abs (- (y-point (start-segment s)) (y-point (end-segment s))))))
    (sqrt (+ (* a a) (* b b)))))

(define (midpoint-segment s)
  (define (avg a b) (/ (+ a b) 2))
  (let* ((p1 (start-segment s))
         (p2 (end-segment s))
         (x1 (x-point p1))
         (x2 (x-point p2))
         (y1 (y-point p1))
         (y2 (y-point p2)))
    (make-point (avg x1 x2) (avg y1 y2))))

;; Exericse 2.3. Implement a representation for rectangles in a plane. In terms of your constructors
;; and selectors, create procedures that compute the perimeter and area of a given rectangle. Now
;; implement a different representation for rectangles. Can you design your system with suitable
;; abstraction barriers , so that the same perimeter and area procedures will work using either
;; representation?

;; Given a line segment return boolean whether the slope is positive, negative, 0, or undefined.
(define (segment-slope s)
  (let* ((x1 (x-point (start-segment s)))
         (y1 (y-point (start-segment s)))
         (x2 (x-point (end-segment s)))
         (y2 (y-point (end-segment s)))
         (delta-x (- x1 x2))
         (delta-y (- y1 y2)))
    (if (zero? delta-x) +inf.0 (/ delta-y delta-x))))

;; Given a line, determines the inverse slope (such that the two line segments are perpendicular)
(define (segment-inv-slope s)
  (let ((slope (segment-slope s)))
    (cond ((infinite? slope) 0)
          ((zero? slope) +inf.0)
          (else (* -1 (/ 1 slope))))))

;; This function takes a slope and returns a function to mutate a point such that
;; a new segment can be constructed from one end of the line segment to construct
;; two line segments that are perpendicular, intersect at one end, and together
;; form a rectangle
(define (fn-point-mutator-alt slope)
  ;; Given hypotenuse of an isosoles right triangle, find the length of other side
  (define (find-a c) (sqrt (/ (* c c) 2)))
  ;; If slope == inf, then (x,y) => (x+h,y)
  (cond ((infinite? slope) (lambda (p len)
                             (make-point
                               (x-point p)
                               (+ len (y-point p)))))
        ;; If slope == 0, then (x,y) => (x,y+h)
        ((zero? slope) (lambda (p len)
                         (make-point
                           (+ len (x-point p))
                           (y-point p))))
        ;; if slope < 0, then (x,y) => (x + sqrt(h^2/2), y + sqrt(h^2/2)
        (else (lambda (p len)
                (let ((a (find-a len)))
                  (make-point
                    (+ (x-point p) a)
                    (+ (y-point p) a)))))))

(define (make-segment-alt midpoint slope len)
  (let* ((fn (fn-point-mutator-alt slope))
         (p1 (fn midpoint (/ len 2)))
         (p2 (fn midpoint (* -1 (/ len 2)))))
    (make-segment p1 p2)))

;; This function takes a slope and returns a function to mutate a point such that
;; a new segment can be constructed from one end of the line segment to construct
;; two line segments that are perpendicular, intersect at one end, and together
;; form a rectangle
(define (fn-point-mutator slope)
  ;; Given hypotenuse of an isosoles right triangle, find the length of other side
  (define (find-a c) (sqrt (/ (* c c) 2)))
  ;; If slope == inf, then (x,y) => (x+h,y)
  (cond ((infinite? slope) (lambda (p len)
                             (make-point
                               (+ len (x-point p))
                               (y-point p))))
        ;; If slope == 0, then (x,y) => (x,y+h)
        ((zero? slope) (lambda (p len)
                         (make-point
                           (x-point p)
                           (+ len (y-point p)))))
        ;; if slope < 0, then (x,y) => (x + sqrt(h^2/2), y - sqrt(h^2/2)
        (else (lambda (p len)
                (let ((a (find-a len)))
                  (make-point
                    (+ (x-point p) a)
                    (- (y-point p) a)))))))

;; Given a line segment and a length, construct a second line segment such that
;; the two line segments together form a rectangle consistent with len.
(define (make-rect s1 len)
  (let* ((slope (segment-slope s1))
         (fn (fn-point-mutator slope))
         (p1 (end-segment s1))
         (p2 (fn p1 len))
         (s2 (make-segment p1 p2)))
    (cons s1 s2)))

;; Given a line segment and a length, construct a second line segment that is perpendicular
;; to it, with the given length, and the two intersect at the midpoint.
(define (make-rect-alt s1 len)
  (let* ((slope (segment-inv-slope s1))
         (midpoint (midpoint-segment s1))
         (s2 (make-segment-alt midpoint slope len)))
    (cons s1 s2)))

(define (get-perimeter rect)
  (* 2 (+ (len-segment (car rect)) (len-segment (cdr rect)))))

(define (get-area rect)
  (* (len-segment (car rect)) (len-segment (cdr rect))))

;; Tests to verify that everything works
(define horizontal-segment
  (let ((p1 (make-point 0 0))
        (p2 (make-point 4 0)))
    (make-segment p1 p2)))
(define r1 (make-rect horizontal-segment 2))
(assert (equal? (get-perimeter r1) 12))
(assert (equal? (get-area r1 ) 8))

(define vertical-segment
  (let ((p1 (make-point 0 3))
        (p2 (make-point 0 0)))
    (make-segment p1 p2)))
(define r2 (make-rect vertical-segment 1))
(assert (equal? (get-perimeter r2) 8))
(assert (equal? (get-area r2) 3))

(define positive-slope-segment
  (let ((p1 (make-point 0 0))
        (p2 (make-point 4 3)))
    (make-segment p1 p2)))
(define r3 (make-rect positive-slope-segment 5))
(assert (equal? (get-area r3) 25.0))

(define negative-slope-segment
  (let ((p1 (make-point 0 0))
        (p2 (make-point 5 -12)))
    (make-segment p1 p2)))
(define r4 (make-rect negative-slope-segment 13))
(assert (equal? (get-area r4) 169.0))
