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



