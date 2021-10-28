;#1
(define-syntax trace-ex 
  (syntax-rules () 
    ((trace-ex object) 
     (let ((result object)) 
       (begin 
         (write 'object) 
         (display " => ") 
         (write result) 
         (newline) 
         result)))))


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

