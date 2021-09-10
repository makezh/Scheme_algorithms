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

