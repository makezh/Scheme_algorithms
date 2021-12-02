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

;#2

(define (load-data file)
  (call-with-input-file file
    (lambda (port) (read port))))

(define (save-data data file)
  (call-with-output-file file (lambda (port) (write data port)))
  )
    
(define data (load-data "input.txt"))
(display data)
(newline)
;(save-data data "output.txt")

(define (count-line file)
  (call-with-input-file file
    (lambda (port)
      (define s1 "")
      (define s2 "")
      (define (read-loop count)
        (set! s1 s2)
        (set! s2 (read-char port))
        (if (eof-object? s2)
            count
            (if (or (and (equal? s2 #\return) (not (equal? s1 #\newline))) (and (equal? s2 #\newline) (not (equal? s1 #\newline)) (not (equal? s1 #\return))))
                (read-loop (+ count 1))
                (read-loop count))))
      (read-loop 0))))

(count-line "4_laba.scm")

;#3

