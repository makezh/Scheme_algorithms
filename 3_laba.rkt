;#1
(define-syntax trace-ex 
  (syntax-rules () 
    ((trace-ex x) 
     (let ((res x)) 
       (begin 
         (write 'x) 
         (write " => ") 
         (write res) 
         (newline) 
         res)))))


(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) ; Здесь...
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss)))))) ; ... и здесь

(display "#1 \n")
(zip '(1 2 3) '(one two three))
(newline)

;--------------------------
;#2

(load "unit-test.scm")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(define the-tests
  (list 
   (test (signum -2) -1)
   (test (signum 0) 0)
   (test (signum 2) 1)
   (test (signum 100) 1)))

(display "#2\n")
(run-tests the-tests)
(newline)

;--------------------------
;#3

(define (to-list x)
  (cond
    ((string? x) (string->list x))
    ((vector? x) (vector->list x))
    ((list? x) x)))

(define (ref x . xs) ; позиция вставки (car xs) -- элемент вставки (cadr xs)
  (define (loop i obj pos elem)
    (if (null? obj)
        (if (= pos (length (to-list x)))
            (list elem)
            (list))
        (if (= i pos)
            (append (cons elem (list (car (to-list obj)))) (loop (+ i 1) (cdr (to-list obj)) pos elem))
            (cons (car (to-list obj)) (loop (+ i 1) (cdr (to-list obj)) pos elem)))))
  (cond
    ((= (length xs) 1)
     (begin
       (if (or (> (car xs) (- (length (to-list x)) 1)) (< (car xs) 0))
           #f
           (list-ref (to-list x) (car xs))
           )))
    ((= (length xs) 2)
     (cond
       ((or (< (length (to-list x)) (car xs)) (< (car xs) 0)) #f)
       ((and (not (char? (cadr xs))) (string? x)) #f)
       ((string? x) (list->string (loop 0 (to-list x) (car xs) (cadr xs))))
       ((vector? x) (list->vector (loop 0 (to-list x) (car xs) (cadr xs))))
       (else (loop 0 x (car xs) (cadr xs)))))))

(define 3_tests
  (list
   (test (ref '(1 2 3) 1) 2)
   (test (ref #(1 2 3) 1) 2)
   (test (ref "123" 1) #\2)
   (test (ref "123" 3) #f)
   (test (ref "123" -1) #f)
   (test (ref '(1 2 3) 1 0) '(1 0 2 3))
   (test (ref #(1 2 3) 1 0) #(1 0 2 3))
   (test (ref #(1 2 3) 1 #\0) #(1 #\0 2 3))
   (test (ref "123" 1 #\0) "1023")
   (test (ref "123" 1 0) #f)
   (test (ref "123" 3 #\4) "1234")
   (test (ref "123" 5 #\4) #f)
   (test (ref "123" -1 #\4) #f)
   ))

(display "#3\n")
(run-tests 3_tests)
(newline)

;--------------------------
;#4


       


      
      

