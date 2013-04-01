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

;; 1.2 PROCEDURES AND THE PROCESSES THEY GENERATE
;; ==============================================
;;
;; The ability to visualize the consequences of the actions under
;; consideration is crucial to becoming an expert programmer. A procedure
;; is a pattern for the _local evolution_ of a computational process. In
;; this section, we look at some of those patterns.
;;
;; 1.2.1 Linear Recursion and Iteration
;; ------------------------------------
;;
;; Consider the procedure to calcuate a factorial

(define (fact-1 n)
  (if (= n 1) 1
      (* n (fact-1 (- n 1)))))

;; An alternative method using iteration might look like

(define (fact-2 n)
  (define (iter acc i)
    (if (> i n)
        acc
        (iter (* acc i) (+ i 1))))
  (iter 1 1))

;; Although these two versions arrive at the same answer, the shapes of the
;; processes they use are very different. In the recursive version, the
;; process builds up a chain of deferred operations. In contrast, the iterative
;; version does not grow or shrink because there are no deferred operations.
;; Instead, there are a fixed number of _state variables_ together with a rule
;; about how they should be updated as the process moves from state to state.
;;
;; Note that recursive process is distinquished from a recursive procedure that
;; calls itself. Instead, the recursive process is defined from the shape that
;; the process takes (growing number of deferred operations followed by a
;; shrinking as the deferred operations are collapsed together).
;;
;; (fact 6)
;; (* 6 (fact 5))
;; (* 6 (* 5 (fact 4)))
;; (* 6 (* 5 (* 4 (fact 3))))
;; (* 6 (* 5 (* 4 (* 3 (fact 2)))))
;; (* 6 (* 5 (* 4 (* 3 (* 2 (fact 1))
;; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
;; (* 6 (* 5 (* 4 (* 3 2))))
;; (* 6 (* 5 (* 4 6)))
;; (* 6 (* 5 24)
;; (* 6 120)
;; 720
;;
;; acc <== i * acc
;; i <== i + 1
;;
;; Another way of understanding the difference is that the iterative process
;; provides a complete description of the state of the process at any given
;; point in time. If the process needed to be stopped, it could be restarted
;; and all the information necessary to restart the process would be
;; available. In the recursive process, there is additional hidden information
;; on the stack, maintained by the interpreter and not contained in the
;; program variables. The longer the chain, the more information must be
;; maintained.
;;
;; Note that with *tail recursion* the iterative process is executed in
;; constant space, so there is no overhead to pay with recursion from the
;; compiler.

;; Exercise 1.9 Each of the following two procedures defines a method for
;; adding two positive integers in terms of the procedures `inc`, which
;; increments its argument by 1, and `dec`, which decrements its arguments.
;; Using the substitution model, illustrate the process generated by each
;; procedure in evaluating (+ 4 5). Are these processes iterative or
;; recursive?
;;
;; (define (+ a b)
;;   (if (= a 0) b (inc (+ (dec a) b))))
;;
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;;
;; (define (+ a b)
;;   (if (= a 0) b (+ (dec a) (inc b))))
;;
;; (+ 4 5)
;; (+ 5 4)
;; (+ 6 3)
;; (+ 7 2)
;; (+ 8 1)
;; (+ 9 0)
;; 9
;;
;; The first procedure is recursive and the second is iterative.

;; Exercise 1.10 The following procedure computes a mathematical function
;; called Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; What are the values of the following expressions?
;;
;; (A 1 10) ==> 1024
;; (A 2 4)  ==> 65536
;; (A 3 3)  ==> 65536
;;
;; Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))   ;; computes to 2n
(define (g n) (A 1 n))   ;; computes to n^n
(define (h n) (A 2 n))   ;; computes to 2^(h (- n 1))
(define (k n) (* 5 n n))

;; (h 1) ==> 2
;; (h 2) ==> 4
;; (h 3) ==> 16
;; (h 4) ==> 65536

(define (pow x y)
  (define (iter acc i)
    (if (= i 0)
        acc
        (iter (* acc x) (- i 1))))
  (iter 1 y))

;; Give concise mathematical definitions for the functions computed by the
;; procedures f, g, and h for positive integer values of `n`. For example,
;; (k n) computes to 5n^2.
;;
;; for solutions, see above

;; 1.2.2 Tree Recursion
;; --------------------
;;
;; Another pattern of computation is the tree recursion. Consider the
;; Fibnoacci sequence, in which each number is the sum of the previous two.
;; A programmatic definition of it might look like

(define (fib-1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-1 (- n 1))
                 (fib-1 (- n 2))))))

;; The evolved process of this procedure looks like a tree because for each
;; iteration, it calls itself twice. Note that the computation process is not
;; very productive because of redundant computation. In fact, the value of
;; Fib(n) grows exponentially with n. On the other hand, the space required
;; grows only linerally with the input, because we need to keep track only of
;; which nodes are above us in the tree at any point in the computation.
;;
;; An alterative would be this iterative process

(define (fib-2 n)
  (define (iter a b i)
    (if (= i 0) b
        (iter (+ a b) a (- i 1))))
  (iter 1 0 n))

;; The difference between the number of steps (N N) versus (N) can be huge
;; even for small inputs. The model of the problem as a recursive process
;; can be very helpful in framing the problem and better understanding it.
;;
;; Consider the problem of counting change. How many different ways can we
;; make change of $1.00, given half-dollars, quarters, dimes, nickels, and
;; pennies?
;;
;; The problem has a simple solution as a recursive procedure. If the coins
;; are sorted in order, then the following relation holds. The number of ways
;; to change amount `a` using `n` kinds of coins equals
;;   - number of ways to change amount a using all but first coin, plus
;;   - number of ways to change amount `a-d` using all n kinds of coins,
;;     where `d` is the denomination of the first kind of coin.
;;
;; Also, given the following degenerate cases
;;   - (= a 0) ==> 1 ways to make change
;;   - (< a 0) ==> 0 ways to make change
;;   - (= n 0) ==> 0 ways to make change
;;
;; Thus, we can translate these six statements into a recursive procedure

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (iter amount kinds-of-coins)
    (cond ((= amount 0)  1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (iter amount (- kinds-of-coins 1))
                   (iter (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (iter amount 5))

;; (count-change 100)
;; 292

;; It is interesting to note that this function uses a tree-recursive
;; algorithm, which is not very inefficient. However, it is not
;; obvious that there is a better alternative using an iterative
;; process. Thus, the trade-off is that tree-recursive process is easy
;; to understand but not efficient.

;; Exercise 1.11 A function `f` is defined by the rule that f(n) = n
;; if n < 3 and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n>= 3. Write a
;; procedure that computes f by means of a recursive process. Write a
;; procedure that computes f by means of an iterative process.

(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+
               (* (f-recursive (- n 1)) 1)
               (* (f-recursive (- n 2)) 2)
               (* (f-recursive (- n 3)) 3)))))

(define (f-iterative n)
  (define (iter i acc f1 f2 f3)
    (if (= i n)
        acc
        (iter (+ i 1)
              (+ (* acc 1) (* f1 2) (* f2 3))
              acc
              f1
              f2)))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (iter 3 4 2 1 0))))

;; exercise 1.12 The following pattern of numbers is called Pascal's
;; triangle
;;
;;             1
;;            1 1
;;           1 2 1
;;          1 3 3 1
;;         1 4 6 4 1
;;            ...
;;
;; The numbers at the edge of the triange are all 1, and each number
;; inside the triangle is the sum of the two numbers above it. Write a
;; procedure that computes elements of Pascal's triangle by means of a
;; recursive process.
;;

(define (pascal row col)
  (cond ((or (> col row) (< col 0) (< row 0)) 0)
        ((or (= col 0) (= col row)) 1)
        (else (+
               (pascal (- row 1) (- col 1))
               (pascal (- row 1) col)))))

;; Exercise 1.13 Prove that Fib(n) is the closest integer to
;; sigma^n/sqrt(5), where sigma = (1 + sqrt(5))/2. Hint: Let y = (1 -
;; sqrt(5))/2. Use induction and the definition of the Fibonacci
;; numbers to prove that Fib(n) = (theta^n = epsilon^n)/sqrt(5)

;; ???

;; 1.2.3 Orders of Growth
;; ----------------------
;;
;; We want to express the amount of computational resources that a
;; procedure will consume. The *order of growth* is a gross measure of
;; the resources required by a process as the inputs become larger.
;;
;; Let `n` be a parameter that measures the size of the problem. Then
;; R(n) is the amount of resources the process requires for problem
;; size of `n`.
;;
;; We say that R(n) has order of growth θ(f(n))", if there are
;; positive constants k_1 and k_2 independent of n such that k_1 of
;; f(n) <= R(n) <= k_2 of f(n)
;;
;; The `fact-recursive`, the number of steps grew proportional to the
;; input `n`, thus the steps required grew at θ(n). For the
;; `fact-iterative`, the number of steps was still θ(n) but the space
;; was θ(1), that is, constant. The tree-recursive Fibonacci
;; computation requires θ(φ^n) steps and space of θ(n), where φ is the
;; golden-ratio described in section 1.2.2. The concept of space
;; relates to how many values the procedure needs to remember.
;;
;; In general, the number of steps required by a tree-recursive
;; process will be proportional to the number of nodes in the tree,
;; while the space required will be proportional to the maximum depth
;; of the tree.
;;
;; The value of the *golden ratio* is (1+sqrt(5))/2 == 1.6180339887...
;; 
;; Orders of growth only provides a crude description of a process
;; behavior. Thus, a process requiring n^2 steps and a process requiring
;; 1000n^2 steps and another process requiring 3n^2 + 10n + 17 steps all
;; have θ(n^2) order of growth.
;;
;; However, it provides a useful indication of how we can expect the
;; behavior of a process to change as the size increases. For θ(n),
;; doubling the size of input will double the resources used.

;; Exercise 1.15 The sine of an angle (specified in radians) can be
;; computed by making use of the approximation sin =x if x is sufficently
;; small, and the trigonemetric identity
;;                                        x             x
;;                         sin x = 3 sin --- - 4 sin^3 --- 
;;                                        3             3
;;
;; to reduce the size of the argument of sin. (for purposes of this exercise
;; an angle is considered "sufficiently small" if its magnitude is not greater
;; than 0.1 radians.) these ideas are incorporated in the following procedures:

(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x)
    (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; How many times is the procedure p applied when (sine 12.15) is evaluated?
;; What is the order of growth in space and number of spets (as a function of
;; a)

;; 1.2.4 Exponentiation
;; --------------------
;;
;; Suppose we want to define a process for exponentiation via a recursive
;; definition

(define (exp-linear-recursive x y)
  (if (= y 0)
      1
      (* x (exp-linear-recursive x (- y 1)))))

(define (exp-linear-iterative x y)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (* acc x))))
  (iter y 1))

;; The recursive version requires θ(n) steps and θ(n) space. The iterative version
;; requires θ(n) steps and θ(1) space.
;;
;; It is possible to reduce the steps by using successive squaring. Thus for b^8,
;; instead of b * (b * (b * (b * (b * (b * (b * (b))))))), we can compute it using
;; three multiplications: b^2 = b * b, b^4 = b^2 * b^2, b^8 = b^4 * b^4. We can use
;; the following rule to address odd exponentials.
;;
;;                       b^n = (b^(n/2))^2, if n is even
;;                       b^n = b * b^(n-1)  if n is odd
;;
(define (fast-exp x y)
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= y 0) 1)
        ((even? y) (square (fast-exp x (/ y 2))))
        (else (* x (fast-exp x (- y 1))))))

;; The fast-exp version grows logarithmically with n in both space and number of
;; steps, using the fast version means we can double the size of the exponent (with
;; only one more multiplication) for each n. Thus, it is θ(log n).
;;
;; The difference with large numbers can be striking. For example, for an exponent
;; of 1000, it can be handled in 14 multiplications instead of 1000.

;; Exercise 1.16 Design a procedure that evolves an iterative exponentiation
;; process that uses successive squaring and uses a logarithmic number of steps, as
;; does fast-expt. (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep,
;; along with the exponent n and the base b, an additional state variable a, and
;; define the state transformation in such a way that the product ab^n is unchanged
;; from state to state. At the beginning of the process a is taken to be 1, and the
;; answer is given by the value of a at the end of the process. In general, the
;; technique of defining an _invariant quantity_ that remains unchanged from state
;; to state is a powerful way to think about the design of iterative algorithms.

(define (fast-exp2 base exp)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (square x) (* x x))
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 base exp))
