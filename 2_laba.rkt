;1
(define (count x xs)
  (if (null? xs)
    0
    (if (equal? x (car xs))
        (+ (count x (cdr xs)) 1)
        (count x (cdr xs)))))
(display "#1") (newline)
(display (count 'a '(a b c d))) (newline)
(display (count 'a '(a a a a))) (newline)
(display (count 'a '())) (newline)
(newline)

;2
(define (delete pred? xs)
  (if (null? xs)
      (list)
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))

(display "#2") (newline)
(display (delete even? '(1 2 3 4))) (newline)
(display (delete odd? '(1 2 3 4))) (newline)
(display (delete odd? '(1 3 5 7))) (newline)
(newline)

;3
(define (iterate f x n)
  (if (= n 0)
      '()
      (cons x (iterate f (f x) (- n 1)))))

(display "#3") (newline)
(display (iterate (lambda (x) (* 2 x)) 1 6)) (newline)
(display (iterate (lambda (x) (* 2 x)) 1 1)) (newline)
(display (iterate (lambda (x) (* 2 x)) 1 0)) (newline)
(newline)

;4
(define (intersperse x xs)
  (if (<= (length xs) 1)
      xs
      (cons (car xs) (cons x (intersperse x (cdr xs))))))
      
(display "#4") (newline)
(display (intersperse 'x '(1 2 3 4))) (newline)
(display (intersperse 'x '(1 2))) (newline)
(display (intersperse 'x '(1))) (newline)
(display (intersperse 'x '())) (newline)
(newline)

;5
(define (any? pred? xs)
  (if (null? xs)
      #f
      (or (pred? (car xs)) (all? pred? (cdr xs)))))
  ;(and (not (null? xs)) (or (pred? (car xs)) (all? pred? (cdr xs))) ))

(define (all? pred? xs)
  ;(if (null? xs)
     ;#t
     ;(and (pred? (car xs)) (all? pred? (cdr xs)))))
   (or (null? xs) (and (pred? (car xs)) (all? pred? (cdr xs))) ))

(display "#5") (newline)
(display (any? odd? '(1 3 5 7))) (newline)
(display (any? odd? '(0 1 2 3))) (newline)
(display (any? odd? '(0 2 4 6))) (newline)
(display (any? odd? '())) (newline)
(newline)

(display (all? odd? '(1 3 5 7))) (newline)
(display (all? odd? '(0 1 2 3))) (newline)
(display (all? odd? '(0 2 4 6))) (newline)
(display (all? odd? '())) (newline)
(newline)

;6
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)  ))))

(display "#6") (newline)
(display ((o f g h) 1)) (newline)
(display ((o f g) 1)) (newline)
(display ((o f) 1)) (newline)
(display ((o) 1)) (newline)
(newline)
