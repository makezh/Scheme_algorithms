;#1

(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))


(display "#1") (newline)
(display "(my-range  0 11 3) -> ")
(my-range  0 11 3) 

;------------------------

(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (car xs) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))

(display "(my-flatten '(1 2 (3 4) 5 (7 8 9 10) (12))) -> ")
(my-flatten '(1 2 (3 4) 5 (7 8 9 10) (12)) )

;------------------------

(define (my-element? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (my-element? x (cdr xs)))))

(display "(my-element? 122 '(1 2 3 122 x)) -> ")
(my-element? 122 '(1 2 3 122 x))
(display "(my-element? 12 '(1 2 3 122 x)) -> ")
(my-element? 12 '(1 2 3 122 x))

;------------------------

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

(display "(my-filter odd? (my-range 0 10 1)) -> ")
(my-filter odd? (my-range 0 10 1))
(display "(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)) -> ")
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))

;------------------------

;(define (my-fold-left op xs))

;------------------------

;(define (my-fold-right op xs))

(newline)
;=========================

;#2

(define (list->set xs)
  (if (null? xs)
      '()
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))))

(display "#2") (newline)
(display "(list->set '(1 1 1 2 3)) -> ")
(list->set '(1 1 1 2 3))

;------------------------

(define (set? xs)
  (cond
    ((null? xs) #t)
    ((my-element? (car xs) (cdr xs)) #f)
    (else (set? (cdr xs)))))

(display "(set? '(1 2 3)) -> ")
(set? '(1 2 3))
(display "(set? '(1 2 3 3)) -> ")
(set? '(1 2 3 3))
(display "(set? '()) -> ")
(set? '()) 

;=========================

;#5

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)  ))))


