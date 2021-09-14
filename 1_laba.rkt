(define (day-of-week d m y)
  (if (< m 3)
      (remainder (+ d (quotient (* 31 (+ m 10)) 12) (- y 1) (quotient (- y 1) 4) (- (quotient (- y 1) 100)) (quotient (- y 1) 400)) 7)
      (remainder (+ d (quotient (* 31 (- m 2)) 12) y  (quotient y 4) (- (quotient y 100)) (quotient y 400)) 7)))

(define (equation a b c)
  (define D (- (* b b) (* 4 a c)))
  (if (< D 0)
      (list)
      (if (= D 0)
          (list (/ (- b) (* 2 a)))
          (list (/ (+ (- b) (sqrt D)) (* 2 a)) (/ (- (- b) (sqrt D)) (* 2 a)))
      )))

(define (my-gcd a b)
  (if (= b 0)
      (abs a)
      (my-gcd b (remainder a b))
      ))

(define (my-lcm a b)
  (quotient (* a b) (my-gcd a b))
  )

(define (fac n)
  (if (<= n 0)
      1
      (* n (fac (- n 1)))
      ))

(define (prime? n)
  (= (remainder (+ 1 (fac (- n 1))) n) 0))



;;;;;;;;;;;;;;;;; EXTRA TASK ;;;;;;;;;;;;;;;;;;;;;;

(define (prog-day x)
  (cond ((< x 2002) (display "Не праздновался"))
        ((< x 2009)
         (if (or (and (= (remainder x 4) 0) (not (= (remainder x 100) 0))) (= (remainder x 400) 0))
             (display "Праздновался неофициально 12 сентября")
             (display "Праздновался неофициально 13 сентября")
             ))
        ((< x 2022)
         (if (or (and (= (remainder x 4) 0) (not (= (remainder x 100) 0))) (= (remainder x 400) 0))
             (display "Праздновался официально 12 сентября")
             (display "Праздновался официально 13 сентября")
             ))
        (else
         (if (or (and (= (remainder x 4) 0) (not (= (remainder x 100) 0))) (= (remainder x 400) 0))
             (display "Будет праздноваться официально 12 сентября")
             (display "Будет праздноваться официально 13 сентября")
             )))
  )


