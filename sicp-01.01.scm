;; Structure & Interpretation of Computer Programs, Chapter 1
;; ELEMENTS OF PROGRAMMING
;;
;; 1.1 ELEMENTS OF PROGRAMMING
;; ===========================

;; 1.1.1 Expressions
;; ----------------- 
;;
;; expressions are normally prefix for the interpreter. Prefix notation has
;; several advantages, including that it can take arbitrary number of
;; arugments. For example, the function:

(define (sum-many a b c d e f g) (+ a b c d e f g))

;; can sum several items. Another advantage is that it allows combinations
;; to be nested, as in:

(define (nested x y)
  (+ (* x y) (- x y)))

;; 1.1.2 Naming And The Environment
;; -------------------------------- 
;;
;; We can name things using the `define` keyword. In the below example, we can
;; now refer to `2` with the name `size`

;; 1.1.3 Evaluating Combinations
;; -----------------------------

;; The general procedure the compiler uses is:
;;   1. Evaluate the subexpressions of the combination
;;   2. apply the procedure that is the value of the leftmost subexpression
;;      (the operator) to the arguments are are the values of other
;;      s-expressions (the operands)
;; 
;; The implication of this rule is that evaluation is recursive. Also, _special
;; forms_, such as `define`, do not follow this rule of evaluation.

(define size 2)

;; The concept of a procedure definition allows for a procedure to be called
;; by giving it a name. For example:

(define (square x) (* x x))

;; The general format is (define (<name> <formal parameters>) <body>)

;; Besides using the function `square` by calling it, we can also use it as
;; a building block for other procedures, for example:

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Now we can use sum-of-squares as a building block for futher functions,
;; as in:

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; 1.1.5 The Substitution Model
;; ----------------------------

;; However, two points are stressed:
;;   * The purpose of the substitution model is to understand procedure
;;     application, but this is not really how the interpreter works
;;   * This model is just one of many ways to understand how interpreters work,
;;     more complex models will be introduced in the course of the book.
;;
;; There are two methods of evaluation
;;   1. normal-order :: substitute operand expressions for parameters until
;;                      obtains an expression involving only primitive
;;                      predicates, and then evaluate that expression.
;;   2. applicative-order :: evaluate the arguments and then apply them to
;;                           the expressions
;;
;; Primitive predicates include operators such as * / + -. There are also
;; logical primitives. `and` and `or` are special forms while `not` is a
;; procedure.
;;   * (and <e1> ... <en>)
;;   * (or <e1> ... <en>)
;;   * (not <e>)
;;
;; Here's an example of normal-order expansion (call-by-reference)
;; (f 5)
;; (sum-of-squares (+ 5 1) (* 5 2))
;; (+   (square (+ 5 1))      (square (* 5 2))   )
;; (+   (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
;;
;; Here's an example of the default applicative-order expansion (call-by-value)
;; (f 5)
;; (sum-of-squares (+ 5 1) (* 5 2))
;; (+ (square 6) (square 10))
;; (+ (* 6 6) (* 10 10))
;; (+ 36 100)
;; 136
;;

;; 1.1.6 Conditional Expressions And Predicates
;; --------------------------------------------

;; We have the conditional and the if statement
;;
;; The cond syntax is shown in this function. Note that the result is
;; undefined if none of the expressions evalute as true.

(define (abs-alt0 x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;; Note that Chicken has an optional arrow notation, thus

(define (abs-alt1 x)
  (cond ((> x 0) => x)
        ((= x 0) => 0)
        ((< x 0) => (- x))))

;; there is also an optional else clause, thus

(define (abs-alt2 x)
  (cond ((> x 0) =>    x)
        ((= x 0) =>    0)
        (else       (- x))))

;; there is also an if statement

(define (abs-with-if x)
  (if (< x 0) (- x) x))

;; This uses the form (if <predicate> <consequent> <alternative>)

;; Exercise 1.2 Translate the following expression into prefix form:

(define ex1.2
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))


;; Exercise 1.3 Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers

(define (ex1.3 x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares x y))
        ((= x y)               (sum-of-squares x z))
        ((= x z)               (sum-of-squares x y))
        (else                  (sum-of-squares y z))))

;; Exercise 1.4 Observe that our model of evaluation allows for combinations
;; whose operators are compound expressions. Use this observation to
;; describe the behavior of the following procedure

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The value of (if (> b 0) + -) is evaluated before it is passed to (? a b)

;; Exercise 1.5 Ben Bitdiddle has inveted a test to determine whether the
;; interpreter he is faced with is using applicative-order (call-by-value)
;; evaluation or normal-order evaluation. He defines the following two
;; procedures

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; if the interpreter is using applicative-order evaluation, then there will
;; be infinite recursion because function `p` calls itself with infinite
;; recursion. If the interpreter is using normal-order evaluation, then there
;; will be infinite recursion only if (= x 0)

;; 1.1.7 Example: Square Roots By Newton's Method
;; ----------------------------------------------

;; There is an important distinction between mathematical and computer
;; functions because the latter must not only be valid by also effective.
;; For example, consider the following definition of a square-root function
;;     (sqrt x) = y such that y >= 0 and y * y == x

;; this is a mathematical definition, but note that it does _not_ define a
;; procedure (how to calcuate the square root). This reflects the general
;; distinction between *declarative* and *imperative* knowledge.

;; Newton's method of successive approximations says that whenever we have
;; a guess of `y` for the value of the square root of a number `x`, then
;; we can get a better guess by averaging `y` with `x/y`. Thus
;;
;; Guess    Quotient             Average
;; 1        (2/1) = 2            ((2 + 1)/2) = 1.5
;; 1.5      (2/1.5) = 1.3333     ((1.3333 + 1.5) = 1.4167
;; 1.4167   (2/1.4167) = 1.4118  ((1.4167 + 1.4118)/2) = 1.4142
;; 1.4142   ...

;; Thus, we can formalize the process in terms of procedures

 (define (sqrt-iter guess x)
  (if (improved-good-enough? guess x)
      guess
      (begin
        (print "guess = " guess)
        (sqrt-iter (improve guess x) x))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)  ) 0.001))

(define (sqrt-1 x)
  (sqrt-iter 1 x))

;; note that MIT Scheme defaults to use rational numbers as result when doing
;; integer division. Chicken Scheme allows for rational number by adding the
;; numbers egg.

;; Exercise 1.6 Alyssa P. Hacker doesn't see why `if` needs to be provided
;; as a special form. "Why can't I just define it as an ordinary procedure
;; in terms of `cond`?" she asks. Alyssa's friend Eva Lu Ator claims this
;; can indeed be done, and she defines a new version of `if`

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (alt-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (alt-sqrt-iter (improve guess x) x)))

;; What happens when Alyssa attemps to use this to compute square roots?
;; Explain. We have infinite recursion because of call-by-value, the
;; function is attempting to resolve the value of alt-sqrt-iter before
;; that step of recrsion is computed, thus, we have infinite recursion
;; because it can never be terminated.

;; Exercise 1.7 The `good-enough?` test used in computing square roots will
;; not be very effective for finding the square roots of very small numbers
;; Also, in real computers, arithmetic operations are almost always performed
;; with limited precision. this makes our test inadequate for very large
;; numbers. Explain these statements, with examples showing how the test fails
;; ffor small and large numbers. An alternative strategy for implementing
;; `good-enough?` is to watch how guess changes from one iteration to the
;; next and to stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end test. Does this
;; work better for small and large numbers?

;; The problem with very small numbers is that the actual answer might be
;; smaller than the tolerance level we're using (in this case, 0.001). For
;; very large floating point numbers, it is possible that the difference
;; might actually be larger than 0.001, leading to infinite recursion.
;;
;; The way to solve it is to take the guess and divide it by x

(define (improved-good-enough? guess x)
  (< (/
      (abs (- (square guess) x))
      x)
     0.001))

;; Exercise 1.8 Newton's method for cube roots is based on the fact that if
;; `y` is an approximation to the cube root of x, then a better approximation
;; is given by the value
;;
;;        x / y^2 + 2y
;;        ------------
;;             3
;;
;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure. In Section 1.3.4 we will see how to implement
;; Newton's method in general as an abstraction of these square-root and
;; cube-root procedures.

(define (cube-root x)
  (cube-root-iter 1 x))

(define (cube-root-iter guess x)
  (if (improved-cube-good-enough? guess x)
      guess
      (begin
        (print "guess = " guess)
        (cube-root-iter (improve-cube-guess guess x) x))))

(define (improved-cube-good-enough? guess x)
  (< (/
      (abs (- (cube guess) x))
      x)
     0.001))

(define (cube x)
  (* x x x))

(define (improve-cube-guess y x)
  (/
   (+
    (/ x (square y))
    (* 2 y))
   3))

;; 1.1.8 Procedures As Black-Box Abstractions
;; ------------------------------------------
;; To solve a problem, one must break a problem up into smaller problems,
;; the unit of breaking this down is the *procedural abstraction*, which
;; represents the abstraction of a given procedure. The point of a procedural
;; abstraction is to suppress detail.
;;
;; When a black-box is implemented, it should not matter what values local to
;; the procedure where chosen by the implementor. Thus, the improve procedure
;; for the cube-root procedure could have used `x y` or `a b`. Thus, the
;; meaning of a procedure should be independent of the parameter names used by
;; the author. This leads to the conclusion that the parameter names must be
;; local to the body of the procedure.
;;
;; Procedure parameters are called *bound variables* because the procedure
;; definition _binds_ the parameter value to a locally used variable. In
;; contrast, variables such as the procedure `square` are unbound (in other
;; words, they are varibles defined outside the procedure). The *scope* of a
;; variable is the set of procedures for which that particular binding applies.

;; Another way to control scope is to have internal definitions and block
;; structure, thus, a cleaner version of the sqrt procedure could be:

(define (sqrt-2 x)
  (define (average x y) (/ (+ x y) 2))
  (define (square x) (* x x))
  (define (good-enough? guess x)
    (< (/ (abs (- (square guess) x)) x) 0.001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (iter guess x)
    (if (good-enough? guess x)
        guess
        (iter (improve guess x) x)))
  (iter 1.0 x))

;; This version improves upon the privious one by reducing the amount of
;; variables the namespace uses, and thus sqrt-2 procedure is a true black box.
;; However, it can be improved even more because the variable `x` does not
;; need to be constantly bound, but instead can be a free variable, as is
;; shown below:

(define (sqrt-3 x)
  (define (average a b) (/ (+ a b) 2))
  (define (square a) (* a a))
  (define (good-enough? guess)
    (< (/ (abs (- (square guess) x)) x) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; Thus, the cube-root procedure would be as follows:

(define (cube-root-2 x)
  (define (cube a) (* a a a))
  (define (good-enough? guess)
    (< (/ (abs (- (cube guess) x)) x) 0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))
