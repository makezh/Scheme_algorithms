(define-syntax test
  (syntax-rules (list)
    ((_ fx ans)
     (begin
       (let
           ((x 'fx) (res ans))
         (list x res))))))

(define (run-test x)
  (write (car x))
  (let
      ((returned (eval (car x) (interaction-environment)))
       (expected (cadr x)))
    
    (define (check expects)
      (if (null? expects)
          #f
          (or (equal? (car expects) returned) (check (cdr expects)))))
   
    (if (or (equal? returned expected) (and (list? expected) (check expected)))
        (begin (display " ok\n") #t)
        (begin
          (display " FAIL\n")
          (display "  Expected:")
          (write expected)
          (newline)
          (display "  Returned:")
          (write returned)
          (newline)
          #f))))

(define (run-tests t)
  (define (loop fl t)
    (if (null? t)
        fl
        (if (run-test (car t))
            (loop fl (cdr t))
            (loop #f (cdr t)))))
  (loop #t t))





