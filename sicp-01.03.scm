;; Structure & Interpretation of Computer Programs, Chapter 1
;; ELEMENTS OF PROGRAMMING

;; It turns out that very large numbers in scheme have a default behavior
;; of switching to flonums, thus, it is necessary to import the numbers
;; egg for very large numbers in scheme.

;;(use numbers)

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

(define (sqrt-v1 x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;; guess y1
;; guess y2 = x/y1
;; guess y3 = x/y2 => x/(x/y1) => x * (y1/x) => y1
;;
;; Thus, we have an infinite loop that does not converge. One way to prevent
;; such large oscillations is by preventing the guesses from changing so
;; much. Since the answer is between the guess y and x/y, we can take the
;; average of y and x/y, thus (y + x/y)/2 instead of x/y. Thus,

(define (sqrt-v2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Exercise 1.35 Show that the golden ratio Φ is a fixed-point of the
;; transformation x ==> 1 + 1/x, and use this fact to compute Φ by means
;; of the fixed-point procedure.

(define (golden-ratio x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

;; Exercise 1.36 Modify fixed-point so that it prints the sequence of
;; approximations it generates. Then find a solution to x^x = 1000 by
;; finding the fixed point of x ==> log(1000)/log(x). (Use Scheme's
;; primitive *log* procedure, which computes natural logarithms.) Compare
;; the number of steps this takes with and without average damping. (Note
;; that you cannot start *fixed-point* with a guess of 1, as this would
;; cause division by log(1) = 0.)

(define (fixed-point-with-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess i)
    (let ((next (f guess)))
      (print "i=" i ", guess=" guess)
      (if (close-enough? guess next)
          next
          (try next (+ i 1)))))
  (try first-guess 1))

(define (n-pow-n-fp1 x)
  (fixed-point-with-print (lambda (y) (/ (log 1000) (log y))) 2.0))

;; i=1, guess=2.0
;; i=2, guess=9.96578428466209
;; i=3, guess=3.00447220984121
;; i=4, guess=6.27919575750716
;; i=5, guess=3.75985070240154
;; i=6, guess=5.2158437849259
;; i=7, guess=4.1822071924014
;; i=8, guess=4.82776509834459
;; i=9, guess=4.38759338466268
;; i=10, guess=4.6712500857639
;; i=11, guess=4.48140361689505
;; i=12, guess=4.6053657460929
;; i=13, guess=4.52308496787189
;; i=14, guess=4.57711468204734
;; i=15, guess=4.54138248015145
;; i=16, guess=4.56490324523083
;; i=17, guess=4.54937267930334
;; i=18, guess=4.55960649191329
;; i=19, guess=4.55285387578827
;; i=20, guess=4.55730552974826
;; i=21, guess=4.55436906443618
;; i=22, guess=4.556305311533
;; i=23, guess=4.55502826357355
;; i=24, guess=4.55587039670285
;; i=25, guess=4.55531500119208
;; i=26, guess=4.55568126354333
;; i=27, guess=4.55543971573685
;; i=28, guess=4.55559900999829
;; i=29, guess=4.55549395753139
;; i=30, guess=4.55556323729288
;; i=31, guess=4.55551754841765
;; i=32, guess=4.5555476793064
;; i=33, guess=4.55552780851625
;; i=34, guess=4.55554091291796
;; 4.55553227080365

(define (n-pow-n-fp2 x)
  (fixed-point-with-print (lambda (y)
                            (let ((term (/ (log 1000) (log y))))
                              (average y term))) 2.0))
;; i=1, guess=2.0
;; i=2, guess=5.98289214233104
;; i=3, guess=4.92216872130834
;; i=4, guess=4.62822431819546
;; i=5, guess=4.56834651313624
;; i=6, guess=4.5577305909237
;; i=7, guess=4.55590980904513
;; i=8, guess=4.55559941161062
;; i=9, guess=4.55554655214737
;; 4.55553755199982

;; Conclusion: using average damping reduced the number of iterations from
;; 34 to 9.

;; Exercise 1.37 An infinite *continued fraction* is an expression of the
;; form:
;;
;;     f = N1/(D1+(N2/(D2+(N3/(D3+...
;;
;; As an example, one can show that the infinite continued expansion with
;; N_i and the D_i all equal to 1 produces 1/Φ where Φ is the golden ratio.
;; One way to approximate an infinite continued fraction is to truncate the
;; the expansion after a given number of terms. Such a truncation--a so
;; called *k-term finite continued fraction has the form:
;;
;;     f = N1(D1+N2/(D2+N3/...
;;
;; Suppose that n and d are procedures of one argument (the term index i)
;; that will return the N_i and D_i of the terms of the continued fraction.
;; Define a procedure *cont-frac* such that evaluating (cont-frac n d k)
;; computes the value of the k-term finite continued fraction. Check your
;; procedure by approximating 1/Φ using
;;
;;     (cont-frac (lambda (i) 1.0)
;;                (lambda (i) 1.0)
;;                k)
;;
;; for successive values of k. How large must you make k in order to get an
;; approximation that is accurate to 4 decimal places?

(define (cont-frac-recursive n d k)
  (define (frac i)
    (if (> i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (frac (+ i 1)))  )))
  (frac 1))

(define (cont-frac-iter n d k)
  (define (iter acc i)
    (if (< i 1)
        acc
        (iter
         (/ (n i)
            (+ (d i) acc))
         (- i 1))))
  (iter (/ (n k) (d k)) k))

(define (phi-reciprocal-r k)
  (cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) k))

(define (phi-reciprocal-i k)
  (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k))

;; 1/Φ approximately   => 0.61803398875
;; (phi-reciprocal  5) => 0.615384615384615
;; (phi-reciprocal 10) => 0.618055555555556
;; (phi-reciprocal 11) => 0.618025751072961

;; Exercise 1.38 In 1737, the Swiss mathematician Leonhard Euler published
;; a memoir *De Fractionibus Continuis*, which included a continued fraction
;; for *e-2* where `e` is the base of the natural logarithms. In this fraction
;; N_i are all 1, and the D_i are successively 1,2,1,1,4,1,1,6,1,1,8,...
;; Write a program that uses your cont-frac procedure from Exercise 1.37 to
;; approximate `e`, based on Euler's expansion.

(define (e-cont-frac-r k)
  (cont-frac-recursive
   (lambda (i) 1.0)
   (lambda (i)
     (cond ((= i 1) 1.0)
           ((= i 2) 2.0)
           ((= (remainder (- i 2) 3) 0) (* 2.0 (+ 1 (/ 3 (- i 2)))))
           (else 1.0)))
   k))

(define (e-cont-frac-i k)
  (cont-frac-iter
   (lambda (i) 1.0)
   (lambda (i)
     (cond ((= i 1) 1.0)
           ((= i 2) 2.0)
           ((= (remainder (- i 2) 3) 0) (* 2.0 (+ 1 (/ 3 (- i 2)))))
           (else 1.0)))
   k))

;; Exercise 1.39 A continued fraction representation of the tangent function
;; was published in 1770 by the German mathematician J.H. Lambert:
;;
;;      tan x = x / (1 - (x^2 / (3 - (x^2 / (5-...
;;
;; where x is in radians. Define a procedure (tan-cf x k) that computes an
;; approximation to the tangent function based on Lambert's formula. k
;; specifies the number of terms to compute, as in Exercise 1.37

(define (cont-frac-x-recursive n d k x)
  (define (frac i)
    (if (> i k)
        (/ (n x i) (d i))
        (/ (n x i) (+ (d i) (frac (+ i 1)))  )))
  (frac 1))

(define (cont-frac-x-iter n d k x)
  (define (iter acc i)
    (if (< i 1)
        acc
        (iter
         (/ (n x i)
            (+ (d i) acc))
         (- i 1))))
  (iter (/ (n x k) (d k)) k))

(define (tan-cont-frac-r x k)
  (cont-frac-x-recursive
   (lambda (x i)
     (if (= i 1) x (* -1 (* x x))))
   (lambda (i)
     (if (= i 1) 1.0 (- (* i 2.0) 1)))
   k
   x))

(define (tan-cont-frac-i x k)
  (cont-frac-x-iter
   (lambda (x i)
     (if (= i 1) x (* -1 (* x x))))
   (lambda (i)
     (if (= i 1) 1.0 (- (* i 2.0) 1)))
   k
   x))

;; 1.3.4 Procedures as Returned Values
;; -----------------------------------
;; In this section we explore procedures that return procedures themselves.
;; In the previous section, we formulated a new version of square-root with
;; average damping. We can create express average-damping as a procedure:
;;
;; (define (average x y) (/ (+ x y) 2))

(define (average-damp f) (lambda (x) (average x (f x))))

;; The procedure takes as an argument a procedure *f* and returns as a value
;; a procedure produced by the lambda that, when applied to a number x,
;; produces the average of x and (f x).
;;
;; Using this, we can reformulate the sqrt function as follows:
;;
;; (define tolerance 0.00001)
;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2))
;;        tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))

(define (sqrt-v3 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

;; As an example of reuse, observe that the cube root of x is a fixed point
;; function of y -> x/y^2, we can can generalize the squre-root procedure
;; to cube roots:

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; Newton's method is the use of the fixed-point method to approximate a
;; solution of the equation by finding a fixed point of the function *f*
;;
;; Basically, given a curved line (exponential function), finding the
;; square root is asking for the x-intercept of the line. Using the midpoint
;; algorithm, we take two points, one positive and the other negative,
;; and calculate the midpoint of the two. If the result is negative, then
;; the x-intercept will be between that new point and the positive guess,
;; iterating successive approximations.
;;
;; With Newton's method, the next guess will be the x-intercept of the
;; derivative at that point. The derivative is the slope of the function
;; at a given point. Note that in some cases, the derivative will not be
;; "well-behaved" and diverge from that root.
;;
;; The very general form says this:
;;
;; Given x1 and f(x1),
;;
;; y -  y1   = m     *(x - x1) # the slope formula
;; y - f(x1) = f'(x1)*(x - x1)
;; 0 - f(x1) = f'(x1)*(x - x1)
;; -f(x1)/f'(x1) = x - x1
;; thus:
;; g(x) = x - g(x)/Dg(x)
;;
;; The first step in newton's method is to express the idea of a derivative
;;
;;                      g(x + dx) - g(x)
;; generally: Dg(x) = -------------------
;;                           dx
;;
;; Thus, we can express the idea of a derivative as follows:

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;; along with the definition of dx:

(define dx 0.00001)

;; Thus, like average-damp, *deriv* is a procedure that takes a procedure
;; as an argument and returns a procedure as the result. For example, given
;;
;; (define (cube x) (* x x x))
;;
;; then
;;
;; ((deriv cube) 5) -> 75.0014999664018
;;
;; Notice the double parens, that is because *(deriv cube)* returns a
;; function that takes as its argument the value of 5. Thus, for the
;; function f(x) = x^3, at x=5, the slope of the line at that point will
;; be about 75.
;;
;; With the aid of *deriv*, we can express Newton's method as a fixed-point
;; process:

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Thus, the final and best method of expressing square root is as follows:

(define (sqrt-v4 x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

;; We've seen two ways to express the square-root computation, once as a
;; a fixed-point process, and once using Newton's method. The fixed-point
;; process is really a mid-point process. But really, both are fixed-point
;; processes. Each method begins with a function and finds a fixed point
;; of some transformation of the function. We can generalize this as:

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; This very general procedure takes as its arguments a procedure *g* that
;; computes some function, a procedure that transforms *g*, and an initial
;; guess. The returned result is a fixed point of the transformed function.
;;
;; Using this procedure, we can recast the original sqrt method as:

(define (sqrt-v5 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

;; In like manner, we can express the Newton transform as:

(define (sqrt-v6 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

;; In general, we should be altert to opportunities to identify the
;; underlying abstractions in our programs and to build upon them and
;; generalize them to create more powerful abstractions.
;;
;; Programming elements that have *first-class* status, have:
;;
;;   - They may be named as variables
;;   - They may be passed as arguments
;;   - They may be returned as results of procedures
;;   - They may be included in data structures

;; Exercise 1.40 Define a procedure *cubic* that can be used together with
;; the *newtons-method* procedure in expressions of the form:
;;
;; (newtons-method (cubic a b c) 1) to approximate zeros of the cubic
;;
;;     x^3 + ax^2 + bx + c

(define (sqrt x)
  (define dx 0.00001)
  (define tolerance 0.00001)
  (define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
  (define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (fixed-point f guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (fixed-point f next))))
  (define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

;; (define (newtons-method g guess)
;;  (fixed-point (newton-transform g) guess)

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

;; (newtons-method (cubic -3 -144 432) 1) -> 3.0

;; Exercise 1.41 Define a procedure *double* that takes a procedure of
;; one argument and returns a procedure that applies the original procedure
;; twice. For example, if *inc* is a procedure that adds 1 to its argument,
;; then (double inc) should be a procedure that adds 2. What value is
;; returned by
;;
;;         (((double (double double)) inc) 5)
;;

(define (double f)
  (lambda (x) (f (f x))))

;; (((double (double double)) inc) 5) -> 21

;; Exercise 1.42 Let *f* and *g* be two one argument functions. The
;; composition f after g is defined to be the function x -> f(g(x)).
;; Define a procedure *compose* that implements composition. For
;; example, if *inc* is a procedure that adds 1 to its arguments,
;;
;;     ((compose square inc) 6) -> 49

(define (compose f g)
  (lambda (x) (f (g x))))
