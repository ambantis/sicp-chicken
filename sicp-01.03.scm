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

(define (integral f a b dx)
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
(define (simpsons-rule f a b n)
  (define h
    (/ (- b a) n))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             (else 2))
       (y k)))
  (define (y k)
    (f (+ a (* k h))))
  (/ (* h (sum term 0 inc n)) 3))

;; Exercise 1.30 The sum procedure above generates a linear recursion.
;; The procedure can be rewritten so that the sum is performed iteratively.
;; Show how to do this by filling in the missing expressions in the
;; following definition:
;;
;;     (define (sum term a next b)
;;       (define (iter a result)
;;         (if <??>
;;             <??>
;;             (iter <??> <??>))))
;;       (iter <??> <??>))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (or (= a b) (> a b))
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral-with-iter f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.31 The sum procedure is only the simplest of a vast number of similar
;; abstractions that can be captured as higher-order procedures. Write an analogous
;; procedure called product that returns the product of the values of a function at
;; points over a given range. Show how to define factorial in terms of product.
;; Also, use product to compute approximations to \pi using the formula:
;;
;; \pi / 4 = (2*4*4*6*6*8...) / (3*3*5*5*7*7...)
;;
;; Do both a recursive and iterative version.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (or (= a b) (> a b))
        result
        (iter (next a) (+ result (term a)))))
  (iter a 1))

(define (fact-with-product-recursive n)
  (product identity 1 inc n))

(define (fact-with-product-iter n)
  (product identity 1 inc n))

(define (wallis-product n)
  (define (wallis-term i)
    (*
     (/
      (* 2.0 i)
      (- (* 2 i) 1))
     (/
      (* 2.0 i)
      (+ (* 2 i) 1))))
  (* 2 (product wallis-term 1 inc n)))
  
