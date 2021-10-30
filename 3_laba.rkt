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

(define-syntax test
  (syntax-rules ()
    ((test fx x)
     (list `fx fx x) ; функция (car) -- факт ответ (cadr) -- ожид ответ (caddr)
     )))

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(define the-tests
  (list 
   (test (signum -2) -1)
   (test (signum 0) 0)
   (test (signum 2) 1)))

(define (run-test t)
  (write (car t))
  (if (= (cadr t) (caddr t))
      (begin (display " ok\n") #t)
      (begin
        (display " FAIL\n")
        (display "  Expected: ")
        (write (caddr t))
        (newline)
        (display "  Returned: ")
        (write (cadr t))
        (newline)
        #f)))

(define fl #t)
(define (run-tests t)
  (if (null? t)
      fl
      (if (run-test (car t))
          (run-tests (cdr t))
          (begin (set! fl #f) (run-tests (cdr t)))
          )))

(display "#2\n")
(run-tests the-tests)
(newline)

;--------------------------
      



      
      

  