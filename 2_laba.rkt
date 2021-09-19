(define (count x xs)
  (if (null? xs)
    0
    (if (equal? x (car xs))
        (+ (count x (cdr xs)) 1)
        (count x (cdr xs)))))

(display (count 'a '(a b c d))) (newline)
(display (count 'a '(a a a a))) (newline)
(display (count 'a '())) (newline)