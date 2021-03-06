;; Structure & Interpretation of Computer Programs, Chapter 1
;; ELEMENTS OF PROGRAMMING

;; It turns out that very large numbers in scheme have a default behavior
;; of switching to flonums, thus, it is necessary to import the numbers
;; egg for very large numbers in scheme.

(use numbers)

;;
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

;; Exercise 1.10 The following procedure computes a mathematical function
;; called Ackermann's function


;; 1.2.2 Tree Recursion
;; --------------------
;;
;; Another pattern of computation is the tree recursion. Consider the
;; Fibnoacci sequence, in which each number is the sum of the previous two.
;; A programmatic definition of it might look like

(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib1 (- n 1))
                 (fib1 (- n 2))))))

;; The evolved process of this procedure looks like a tree because for each
;; iteration, it calls itself twice. Note that the computation process is not
;; very productive because of redundant computation. In fact, the value of
;; Fib(n) grows exponentially with n. On the other hand, the space required
;; grows only linerally with the input, because we need to keep track only of
;; which nodes are above us in the tree at any point in the computation.
;;
;; An alterative would be this iterative process

(define (fib2 n)
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
  (define (square n) (* n n))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= y 0) 1)
        ((even? y) (square (fast-exp x (/ y 2))))
        (else (* x (fast-exp x (- y 1))))))

;; The fast-exp version grows logarithmically with n in both space and
;; number of steps, using the fast version means we can double the
;; size of the exponent (with only one more multiplication) for each
;; n. Thus, it is θ(log n).
;;
;; The difference with large numbers can be striking. For example, for
;; an exponent of 1000, it can be handled in 14 multiplications
;; instead of 1000.

;; Exercise 1.16 Design a procedure that evolves an iterative
;; exponentiation process that uses successive squaring and uses a
;; logarithmic number of steps, as does fast-expt. (Hint: Using the
;; observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the
;; exponent n and the base b, an additional state variable a, and
;; define the state transformation in such a way that the product ab^n
;; is unchanged from state to state. At the beginning of the process a
;; is taken to be 1, and the answer is given by the value of a at the
;; end of the process. In general, the technique of defining an
;; _invariant quantity_ that remains unchanged from state to state is
;; a powerful way to think about the design of iterative algorithms.

;; Exercise 1.17 The exponentiation algorithms in this section are
;; based on performing exponentiation by means of repeated
;; multiplication. In a similar way, one can perform integer
;; multiplication by means of repeated addition. The following
;; multiplication procedure (in which it is assumed that our language
;; can only add, not multiply) is analogous to the expt procedure

;; This algorithm takes a number of steps that is linear in b. Now suppose
;; we include, together with addition, operations double, which doubles an
;; integer, and halve, which divides an (even) integer by 2. Using these,
;; design a multiplication procedure analogous to fast-expt that uses a
;; logarithmic number of steps.


;; Exercise 1.18 Using the results of Exercise 1.16 and 1.17, devise a
;; procedure that generates an iterative process for multiplying two
;; integers in terms of adding, doubling, and halving and uses a logarithmic
;; number of steps

;; Exercise 1.19 There is a clever algorithm for computing the Fibonacci
;; numbers in a logarithmic number of steps. Recall the transformation of
;; the state variables a and b in the fib-iter process of Section 1.2.2:
;; a <- a+b and b <- a. Call this transformation T, and observe that
;; applying T over and over again `n` times, starting with 1 and 0, produces
;; the pair Fib(n+1) and Fib(n). In other words, the Fibonacci numbers are
;; produced by applying T^n, the nth power of the transformation T, starting
;; with the pair (1,0). Now consider T to be the special case of p=0 and q=1
;; in the family of transformations T_pq, where T_pq transforms the pair
;; (a,b) according to a <- bq + aq + ap and b <- bp + aq. Show that if we
;; apply such a transformation T_pq twice, the effect is the same as using
;; a single transformation T_p'q' of the same form, and compute p' and q'
;; in terms of p and q. This gives us an explicit way to square these
;; transformations, and thus we can compute T^n using successive squaring,
;; as in the fast-expt procedure. Put this all together to complete the
;; following procedure, which runs in a logarithmic number of steps


;; a1 <- bq + aq + ap
;; b1 <- bp + aq
;;
;; a2 <- b1q + a1q + a1p
;; b2 <- b1q + a1q

;; a2 <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;    =   bpq + aqq + bqq + aqq + apq + bpq + apq + app
;;    =   bpq + bqq + bpq + aqq + aqq + apq + apq + app
;;    =   b(pq + qq + pq) + a(qq + qq + pq + pq + pp)
;;    =   b(2pq + qq) + a(pq + pq + qq) + a(pp + qq)
;;
;; b2 <- (bp + aq)p + (bq + aq + ap)q
;;    =   bpp + aqp + bqq + app + apq
;;    =   b(pp + qq) + a(qp + qp + pp)
;;
;; therefore, p = (pp + qq)
;;            q = (2pq + qq)


;; 1.2.5 Greatest Common Divisors
;; ------------------------------
;;
;; GCD is defined as the largest integer that divides both `a` and `b` with
;; no remainder. For example, the GCD of 16 and 28 is 4. One way to do this is
;; to factor them and search for common factors, but there is a famous
;; algorithm that is more famous.
;;
;; Given GCD(a,b) => GCD(b,r) where a % b = r. This is Euclid's Algorithm,
;; which can be expressed as
;;
(define (gcd-1 a b)
  (if (= b 0)
      a
      (gcd-1 b (remainder a b))))

;; This iterative process has a number of steps that grows as a logarithm of
;; the numbers involved. *Lame's Theorem* states that if Euclid's Algorithm
;; requires `k` steps to compute some GCD pair, then the smaller number in
;; the pair must be greater than or equal to the kth Fibonacci number, leading
;; to an estimate of n >= Fib(k) approximately equal to φ^k/sqrt(5). Thus,
;; the number of steps k grows as the logarithm (to the base φ) of n. Thus,
;; the order of growth is θ(log n).

;; Exercise 1.20 The process that a procedure generates is of course dependent
;; on the rules used by the interpreter. As an example, consider the iterative
;; gcd procedure given above. Suppose we were to interpret this procedure
;; using normal-order evaluation, as discussed in Section 1.1.5 (The normal-
;; order evaluation rule for `if` is described in Exercise 1.5). Using the
;; substitution method (for normal order), illustrate the process generated
;; in evaluating (gcd 206 40) and indicate the remainder operations that are
;; actually performed. How many remainder operations are actually performed
;; in the normal-order evaluation of (gcd 206 40)? In the applicative-order
;; evaluation?


;; 1.2.6 Example: Testing for Primality
;; ------------------------------------
;;
;; This section deals with two methods of checking the primality of an integer.
;; One has an order of growth of θ(sqrt(n)) and the probabilistic algorithm
;; with an order of growth θ(log n).
;;
;; The following procedure finds the smallest integral divosor (greater than 1)
;; of a given number n.

(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

;; Because it only tests divisors through sqrt(n), the order of growth is
;; limited to θ(sqrt(n)).
;;
;; The θ(log n) primality test is based on a result from number theory
;; known as Fermat's Little Theorem.
;;
;; Fermat's Little Theorem: If n is a prime number and `a` is any positive
;; integer less than `n`, then `a` raised to the nth power is congruent to
;; `a` modulo `n`.
;;
;; Two numbers are said to be _congruent modulo_ if they both have the same
;; remainder when divided by `n`.
;;
;; If `n` is not prime, then, in general, most of the numbers `a < n` will
;; not satisfy the above relation, which leads to the Fermat Test:
;;
;; Fermat Test: Given a number `n`, pick a random number a < n and compute
;; the remainder of a^n % n. If the result is not equal to `a`, then `n` is
;; certainly not prime. If it is `a`, then chances are good that `n` is
;; prime. Repeat the test with other numbers. By trying more and more values
;; we can increase our confidence in the result.
;;
;; To implement the Fermat test, we need a procedure that computes the
;; exponential of a number modulo another number

(define (expmod base exp m)
  (define (square n) (* n n))
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                      (square (expmod base (/ exp 2) m))
                      m))
        (else (remainder
               (* base
                  (expmod base (- exp 1) m))
               m))))

;; srfi-27 has (random-integer n) because the function random has an
;; upper limit in C. http://pine.cs.yale.edu/pinewiki/C/Randomization
;; indicates:
;;
;; "The rand function, declared in stdlib.h, returns a random integer
;; in the range 0 to RAND_MAX (inclusive) every time you call it. On
;; machines using the GNU C library RAND_MAX is equal to INT_MAX or
;; 231-1, but it may be as small as 32767. There are no particularly
;; strong guarantees about the quality of random numbers that rand
;; returns, but it should be good enough for casual use, and has the
;; advantage that as part of the C standard you can assume it is
;; present almost everywhere."
(use srfi-27)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; There are numbers that can fool Fermat's test, but such numbers are very
;; rare. Probabalistic algorithms are unique because the answer is only
;; probably correct. There is a class of numbers that can fool the Fermat
;; method, known as the _Carmichael numbers_. There are 255 such numbers below
;; 100,000,000. The first Carmichael numbers are 561, 1105, 1729, 2465, 2821,
;; and 6601. "In testing primality of very large numbers chosen at random, the
;; chance of stumbling upon a value that fools the Fermat test is less than
;; the chance that cosmic radiation will cause the computer to make an error
;; in carrying out the 'correct' algorithm.

;; Exercise 1.21 Use the `smallest-divisor` procedure to find the smallest
;; divisor for each of the following numbers: 199, 1999, 1999.


;; Exercise 1.22 Most Lisp implementations include a primitive called `runtime`
;; that returns an integer that specifies the amount of time the system has
;; been running (measured, for example, in microseconds). The following
;; `timed-prime-test` procedure, when called with an integer `n`, prints `n`
;; and checks to see if `n` is prime. If `n` is prime, the procedure prints
;; three asterisks followed by the amount of time used in performing the test.


;; Using this procedure, write a procedure `search-for-primes` that checks the
;; primality of consecutive odd integers in a specified range. Use your
;; procedure to find the three smallest primes larger than 1000; larger than
;; 10,000; larger than 100,000; larger than 1,000,000. Note the time needed
;; to test each prime. Since the testing algorithm has order of growth of
;; θ(sqrt(n)), you should expect that testing for primes around 10,000 should
;; take about sqrt(10) times as long as testing for primes around 1000. Do
;; your timing data bear this out? How well do the data for 100,000 and
;; 1,000,000 support the θ(sqrt(n)) prediction? Is your result compatible with
;; the notion that programs on your machine run in time proportional to the
;; number of steps required for the computation?


;; Exercise 1.23 The `smallest-divisor` procedure shown at the start
;; of this section does lots of needless testing: After it checks to
;; see if the number is divisible by any larger even numbers. This
;; suggests that the values used for `test-divisor` should not be
;; 2,3,4,5,6,..., but rather 2,3,5,7,9,... To implement this change,
;; define a procedure `next` that returns 3 if its input is equal to 2
;; and otherwise returns its input plus 2. Modify the
;; `smallest-divisor` procedure to use (next test-divisor) instead of
;; (+ test-divisor 1). With `timed-prime-test` incorporating this
;; modified version of `smallest-divisor`

;; Exericse 1.24 Modify the `timed-prime-test` procedure of Exercise 1.22
;; to use `fast-prime?` (the Fermat method), and test each of the 12 primes
;; you found in that exercise. Since the Fermat test has θ(log n) growth
;; how would you expect the time to test primes near 1,000,000 to compare
;; with the time needed to test primes near 1,000? Do your data bear this
;; out? Can you explain any discrepancy you find?

;; Exericse 1.25 Alyssa P Hacker complains that we went to a lot of extra
;; work in writing `expmod`. After all, she says, since we already know
;; how to compute exponentials, we could simply have written

(define (expmod-2 base exp m)
  (remainder (fast-exp base exp) m))

;; Is she correct? Would this procedure serve as well for our fast prime
;; tester? Explain.

;; Exercise 1.26 Louis Reasoner is having great difficulty doing
;; Exercise 1.24. His `fast-prime?` test seems to run more slowly
;; than his `prime?` test. Louis calls his friend Eva Lu Ator over
;; to help. When they examine Louis's code, they find that he has
;; rewritten the `expmod` procedure to use explicit multiplication,
;; rather than calling `square`:

;; "I don't see what difference that could make," says Louis. "I do."
;; says Eva. "By writing the procedure like that, you have transformed
;; the θ(log n) process into a θ(n) process." Explain.

;; The function doubles the number of calls to `expmod`, so rather than
;; generating each iteration's value, it is calculating each iteration's
;; value twice, which is more expensive than simply taking the value and
;; multiplying it twice.

;; Exericse 1.27 Demonstrate that the Carmichael numbers listed in Footnote
;; 1.47 really do fool the Fermat test. That is, write a procedure that
;; takes an integer `n` and tests whether `a^n` is congruent to `a` modulo
;; `n` for every `a < n`, and try your procedure on the given Carmichael
;; numbers (561, 1105, 1729, 2465, 2821, 6601)

;;> (test-carmichaels-fool-fermat first-carmichaels)
;; #t

;; Exercise 1.28 One variant of the Fermat test that cannot be fooled is
;; called the _Miller-Rabin test_ (Miller 1976; Rabin 1980). This starts
;; from an alternate form of Fermat's Little Theorem, which states that
;; if `n` is a prime number and `a` is any positive integer less than `n`,
;; then a^(n-1) is congruent to 1 % n. To test the primality of a number
;; `n` using the `expmod` procedure. However, whenever we perform the
;; squaring step in `expmod`, we check to see if we have discovered a
;; "nontrivial square root of 1 modulo n," that is, a number not equal
;; to 1 or n-1 whose square is equal to 1 modulo n. It is possible to
;; prove that if such a nontrivial square root of 1 exists, then `n` is
;; not prime. It is also possible to prove that if `n` is an odd number
;; that it is not prime, then, for at least half the numbers `a<n`
;; computing a^(n-1) in this way will reveal a nontrivial square root of
;; `1 mod n`. (This is why the Miller-Rabin test cannot be foolsed.)
;; Modify the `expmod` procedure to signal if it discovers a nontrival
;; square root of 1, and use this to implement the Miller-Rabin test with
;; a procedure analogous to `fermat-test`. Check your procedure by testing
;; various known primes and non-primes. Hint: One convenient way to make
;; `expmod` signal is to have it return 0.

;; given (= (prime? n) #t) AND (= (< a n) #t)
;; then (expt a (- n 1)) ~~ (remainder 1 n)
;; nontrivial square root => (not (or (= a 1) (= a (- n 1))))
;;                           (= (expt a 2) (remainder 1 n))
;;
