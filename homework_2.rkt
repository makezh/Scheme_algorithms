;#1

(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))

(my-range  0 11 3) (newline)

;---------------------------

(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (car xs) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))

(my-flatten '(1 2 (3 4) 5 (7 8 9 10) (12)) ) (newline)

;-----------------------------------------------------

(define (my-element? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (my-element? x (cdr xs)))))

(my-element? 122 '(1 2 3 122 x))
(my-element? 12 '(1 2 3 122 x))

;------------------------------



