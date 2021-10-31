(define-syntax test
  (syntax-rules ()
    ((test fx x)
     (list `fx fx x) ; функция (car) -- факт ответ (cadr) -- ожид ответ (caddr)
     )))

(define (run-test t)
  (write (car t))
  (if (equal? (cadr t) (caddr t))
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

(define (run-tests t)
  (define (loop fl t)
    (if (null? t)
        fl
        (if (run-test (car t))
            (loop fl (cdr t))
            (loop #f (cdr t)))))
    (loop #t t))
