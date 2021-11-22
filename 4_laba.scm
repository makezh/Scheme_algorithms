(load "unit-test.scm")
(define ie (interaction-environment))
(define call/cc call-with-current-continuation)

;#1
(define *env* #f)

(define-syntax use-assertions
  (syntax-rules ()
  ((_)
   (call/cc (lambda (cc)
              (set! *env* cc))))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (if (eval expr ie)
         #t
         (begin
           (display "FAILED: ")
           (*env* (write 'expr)))))))


(use-assertions) 

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5)) 
(map 1/x '(-2 -1 0 1 2))
(newline)