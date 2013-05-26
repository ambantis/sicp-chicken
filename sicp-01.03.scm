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

;; Exercise 1.31 The sum procedure is only the simplest of a vast
;; number of similar abstractions that can be captured as higher-order
;; procedures. Write an analogous procedure called product that
;; returns the product of the values of a function at points over a
;; given range. Show how to define factorial in terms of product.
;; Also, use product to compute approximations to \pi using the
;; formula:
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
  
;; Exercise 1.32 Show that sum and product are both special cases of a still
;; more general notion called accumulate that combines a collection of terms,
;; using some general accumulation function:
;;
;;           (accumulate combiner null-value term a next b)
;;
;; Write accumulate and show how sum and product can both be defined as
;; simple calls to accumulate. Write both iterative and recursive versions.

(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate1 combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Exercise 1.33 You can obtain an even more general verison of accumulate
;; by introducing the notion of /filter/ on the terms to be combined. That
;; is, combine only those terms derived from values in the range that
;; satisfy a specified condition. The resulting `filtered-accumulate`
;; abstraction takes the same arguments as `accumulate`, together with
;; an additional predicate of one argument that specifies the filter. Write
;; `filtered-accumulate` as a procedure. Show how to express the following
;; using `filtered-accumulate`:
;;
;;     (a) the sum of the squares of all prime numbers from a to b
;;     (b) teh

(define (filter-accumulate1 filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filter-accumulate1
                               filter
                               combiner
                               null-value
                               term
                               (next a)
                               next
                               b)))
        (else (combiner null-value
                        (filter-accumulate1
                         filter
                         combiner
                         null-value
                         term
                         (next a)
                         next
                         b)))))

(define (filter-accumulate2 filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (square n) (* n n))

(define (prime? n)
  (if (< n 2)
      #f
      (= (smallest-divisor n) n)))

(define (sum-primes-a-to-b1 a b)
  (filter-accumulate1 prime? + 0 square a inc b))

(define (sum-primes-a-to-b2 a b)
  (filter-accumulate2 prime? + 0 square a inc b))

;; 1.3.2 Constructing Procedures Using Lambda
;; ------------------------------------------
;;
;; Using lambda, we can construct functions with no name, as an
;; alternative to defining one-off auxiliary functions. For example,

(define (pi-sum-heavy a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (pi-sum-light a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

;; Thus, we arrive at the same point, without having to construct so many
;; auxiliary helpers. In general, lambda has the following form:
;;
;;    (lambda (<formal-parameters>) <body>)
;;
;; for example, in the expression f(x,y) = x(1+xy)^2+y(1-y)+(1+xy)(1-y)
;;
;; could be expressed as
;;
;;     a = 1 + xy
;;     b = 1 - y
;;     f(x,y) = xa^2 + yb + ab
;;
;; It is possible to create a procedure that computes /f/, including as
;; local variables, not only `x` and `y` but also the names of intermediate
;; values like `a` and `b`.

(define (f-helper-way x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f-lambda-way x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 (* x y))))

(define (f-let-way x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; The general form of a let expression is
;;
;;     (let ((<var1> <exp1>)
;;           (<var2> <exp2>)
;;           ...
;;           (<varN> <expN>))
;;        <body>)
;;
;; Note that variables' values are computered outside the `let`. This
;; matters when the expressions that provide the values for the local
;; variables depend upon variables having the same names as the local
;; variables themselves. For example, if the value of x is 2, the
;; expression
;;
;;     (let ((x 3)
;;           (y (+ x 2)))
;;      (* x y))
;;
;; will have the value 12 because, inside the body of `let`, x will be 3
;; and y will be 4 (which is the outer x plus 2).
;;
;; The preference is to use internal define for internal procedures rather
;; than internal value assignments.

;; Exercise 1.34 Suppose we define the procedure
;;
;;     (define (f g) (g 2))
;;
;; Then we have (f square) -> 4 and (f (lambda (z) (* z (+ z 1)))) -> 6
;;
;; What hapens if we (perversely ask the interpreter to evaluate the
;; combination (f f)? Explain?
;;
;; Answer: We end up with a stack overflow, with the loop endlessly calling
;; itself without any termination condition.

;; 1.3.3 Procedures as General Methods
;; -----------------------------------
;; In this section, we go through general methods such as fixed point.
;;
;; The *half-interval method* is a simple but powerful technique for
;; finding the roots of an equation f(x) = 0, where f is a continuous
;; function. The idea is that, if we are given points *a* and *b*, such
;; that f(a) < 0 < f(b), then f must have at least one zero between a
;; and b. To locate a zero, let x be the average of a and b, and compute
;; f(x). If f(x)>0, then f must have a zero between a and x. If f(x)<0,
;; the f must have a zero between a and x. Since the interval of uncertainty
;; is reduced by half at each step of the process, the number of steps
;; required grows as θ(log(L/T)), where L is the length of the interval and
;; T is the error tolerance.
;;

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y) (/ (+ x y) 2))

;; search can be awkward to use directly, because we can accidentally give
;; points that are invalid. Instead, we use it through the following method:

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value ))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

;; Here, we can use the half-interval-method to search for a root of the
;; equation x^3 - 2x - 3 = 0, between 1 and 2
;;
;; (half-interval-method (lambda (x) (- (* x x x) (* x 2) 3))
;;                       1.0
;;                       2.0) --> 1.893.6640625
;;
;; A number x is called a *fixed point* of a function f if x satisfies
;; the equation f(x) = x. For some functions f, we can locate the fixed
;; point by beginning with an initial guess and applying f repeatedly,
;;
;;     f(x), f(f(x)), f(f(f(x))), ...
;;
;; until the value does not change very much. Thus:

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; unfortunately, sometimes guesses do not converge, for example:

(define (sqrt-bad x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;; guess y1
;; guess y2 = x/y1
;; guess y3 = x/y2 => x/(x/y1) => x * (y1/x) => y1
;;
;; Thus, we have an infinite loop that does not converge. One way to prevent
;; such large oscillations is by preventing the guesses from changing so
;; much. Since the answer is between the guess y and x/y, we can take the
;; average of y and x/y, thus (y + x/y)/2 instead of x/y. Thus,

(define (sqrt-better x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Exercise 1.35 Show that the golden ratio Φ is a fixed-point of the
;; transformation x ==> 1 + 1/x, and use this fact to compute Φ by means
;; of the fixed-point procedure.
