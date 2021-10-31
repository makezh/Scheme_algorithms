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

(define (ref x . xs)
  (cond
    ((= (length xs) 1)
     (begin
       (if (> (car xs) (- (length (to-list x)) 1))
           #f
           (list-ref (to-list x) (car xs))
           )))))

(define 3_tests
  (list
   (test (ref '(1 2 3) 1) 2)
   (test (ref #(1 2 3) 1) 2)
   (test (ref "123" 1) #\2)
   (test (ref "123" 3) #f)))

(run-tests 3_tests)

       


      
      

