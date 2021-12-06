(load "unit-test.scm")
;#1
(display "#1\n")
(define memoized-factorial
  (let loop ((memo '()))
    (lambda (x)
      (if (<= x 1)
          1
          (let ((memo-val (assoc x memo)))
            (if memo-val
                  (cadr memo-val)
                (let ((res (* x (memoized-factorial (- x 1)))))
                  (set! memo (cons (list x res) memo))
                  res)))))))

(define fact-tests
  (list
   (test (memoized-factorial 4) 24)
   (test (memoized-factorial 0) 1)
   (test (memoized-factorial 3) 6)
   (test (memoized-factorial 5) 120)
   (test (memoized-factorial 1) 1)))
(run-tests fact-tests)
(newline)

;#2
(display "#2\n")

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (if (= k 0)
      (list)
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))

;test of natural numbers
(display "Naturals:\n")
(display (lazy-head (naturals 10) 12)) (newline)

(define (factorials)
  (let loop ((p 1) (n 1))
    (lazy-cons (* p n) (loop (* p n) (+ n 1)))))

(define (lazy-factorial n)
  (list-ref (lazy-head (factorials) n)
            (- n 1)))

;test of lazy-factorial
(display "\nFactorial:\n")
(begin
  (display (lazy-factorial 10)) (newline)
  (display (lazy-factorial 50)) (newline))




