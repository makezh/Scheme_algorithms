(define (day-of-week d m y)
  (if (< m 3)
      (remainder (+ d (quotient (* 31 (+ m 10)) 12) (- y 1) (quotient (- y 1) 4) (- (quotient (- y 1) 100)) (quotient (- y 1) 400)) 7)
      (remainder (+ d (quotient (* 31 (- m 2)) 12) y  (quotient y 4) (- (quotient y 100)) (quotient y 400)) 7)))