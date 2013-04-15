;; Structure & Interpretation of Computer Programs, Chapter 1
;; ELEMENTS OF PROGRAMMING

;; It turns out that very large numbers in scheme have a default behavior
;; of switching to flonums, thus, it is necessary to import the numbers
;; egg for very large numbers in scheme.

(use numbers)

;;
;; 1.3 FORMULATING ABSTRACTIONS WITH HIGHER-ORDER PROCEDURES
;; =========================================================
;;
;; We have seen the ability to make abstractions of higher-order procedures,
;; for example, this is an abstraction of the primitive multiplication.

(define (cube x) (* x x x))

;; On the other hand, _higher-order procedures_, can accept procedures as
;; arguments and return types.
;;
;; 1.3.1 Procedures as Arguments
;; -----------------------------
;;
;; Consider the following three procedures. The first computes the sum of
;; integers, from a to b, the second computes the sum of cubes of integers
;; and the third computes the sum of a series

(define (cube x) (* x x x))

(define (sum-integers-alt a b)
  (if (> a b)
      0
      (+ a (sum-integers-alt (+ a 1) b))))

(define (sum-cubes-alt a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes-alt (+ a 1) b))))

(define (pi-sum-alt a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum-alt (+ a 4) b))))

;; However, we can create a single function that takes functions as
;; arguments, as follows:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;; note that `identity is already defined within chicken scheme
;; (define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; To do an integal of a function f can be expressed as

(define (integal f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))



;; Exercise 1.29 Simpson's Rule is a more accurate of numerical integration
;; than the method illustrated above. Using Simpson's Rule, the integral of
;; a function `f` between `a` and `b` is approximated as:
;;
;; (h/3) * (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... 2y_n-2 + 4y_n-1 + y_n)
;;
;; where h = (b-a)/n , for some even integer n
;; y_k = f(a + kh)
;; a = 0
;; b = 1


;; y => (* f (+ a (* k h)))
;; h => (/ (- b a) n)
(define (simpson-rule f a b n)
  (define (h a b n)
    (/ (- b a) n))
  (define (y f a h k)
    (/ (- b a) n))
  )
