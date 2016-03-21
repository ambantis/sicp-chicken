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


;; Exercise 1.32 Show that sum and product are both special cases of a still
;; more general notion called accumulate that combines a collection of terms,
;; using some general accumulation function:
;;
;;           (accumulate combiner null-value term a next b)
;;
;; Write accumulate and show how sum and product can both be defined as
;; simple calls to accumulate. Write both iterative and recursive versions.


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

;; 1.3.2 Constructing Procedures Using Lambda
;; ------------------------------------------
;;
;; Using lambda, we can construct functions with no name, as an
;; alternative to defining one-off auxiliary functions. For example,


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

;; Exercise 1.35 Show that the golden ratio Φ is a fixed-point of the
;; transformation x ==> 1 + 1/x, and use this fact to compute Φ by means
;; of the fixed-point procedure.

;; Exercise 1.36 Modify fixed-point so that it prints the sequence of
;; approximations it generates. Then find a solution to x^x = 1000 by
;; finding the fixed point of x ==> log(1000)/log(x). (Use Scheme's
;; primitive *log* procedure, which computes natural logarithms.) Compare
;; the number of steps this takes with and without average damping. (Note
;; that you cannot start *fixed-point* with a guess of 1, as this would
;; cause division by log(1) = 0.)


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

;; Exercise 1.38 In 1737, the Swiss mathematician Leonhard Euler published
;; a memoir *De Fractionibus Continuis*, which included a continued fraction
;; for *e-2* where `e` is the base of the natural logarithms. In this fraction
;; N_i are all 1, and the D_i are successively 1,2,1,1,4,1,1,6,1,1,8,...
;; Write a program that uses your cont-frac procedure from Exercise 1.37 to
;; approximate `e`, based on Euler's expansion.

;; Exercise 1.39 A continued fraction representation of the tangent function
;; was published in 1770 by the German mathematician J.H. Lambert:
;;
;;      tan x = x / (1 - (x^2 / (3 - (x^2 / (5-...
;;
;; where x is in radians. Define a procedure (tan-cf x k) that computes an
;; approximation to the tangent function based on Lambert's formula. k
;; specifies the number of terms to compute, as in Exercise 1.37

;; 1.3.4 Procedures as Returned Values
;; -----------------------------------
;; In this section we explore procedures that return procedures themselves.
;; In the previous section, we formulated a new version of square-root with
;; average damping. We can create express average-damping as a procedure:
;;
;; (define (average x y) (/ (+ x y) 2))


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

;; Exercise 1.41 Define a procedure *double* that takes a procedure of
;; one argument and returns a procedure that applies the original procedure
;; twice. For example, if *inc* is a procedure that adds 1 to its argument,
;; then (double inc) should be a procedure that adds 2. What value is
;; returned by
;;
;;         (((double (double double)) inc) 5)
;;


;; Exercise 1.42 Let *f* and *g* be two one argument functions. The
;; composition f after g is defined to be the function x -> f(g(x)).
;; Define a procedure *compose* that implements composition. For
;; example, if *inc* is a procedure that adds 1 to its arguments,
