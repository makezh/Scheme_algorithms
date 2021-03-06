(load "unit-test.scm")
(define ie (interaction-environment))
(define call/cc call-with-current-continuation)

;#1
(display "#1\n")

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
(display "\n#2\n")

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
(display "\n#3\n")

(define (trib-without-memo n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    (else (+ (trib-without-memo (- n 1)) (trib-without-memo (- n 2)) (trib-without-memo (- n 3))))))

(define (trib n)
  (let ((memo (make-vector (+ n 1))))
    (let loop ((num n))
      (cond
        ((< num 2) 0)
        ((= num 2) 1)
        (else (if (zero? (vector-ref memo num))
                  (vector-set! memo num
                               (+ (loop (- num 1))
                                  (loop (- num 2))
                                  (loop (- num 3)))))
              (vector-ref memo num))))))

(define trib-tests
 (list
  (test (trib 1) (trib-without-memo 1))
  (test (trib 2) (trib-without-memo 2))
  (test (trib 3) (trib-without-memo 3))
  (test (trib 4) (trib-without-memo 4))
  (test (trib 5) (trib-without-memo 5))
  (test (trib 6) (trib-without-memo 6))
  (test (trib 7) (trib-without-memo 7))
  ))

(run-tests trib-tests)

;#4
(display "\n#4\n")

(define-syntax my-if
  (syntax-rules ()
    ((my-if cond? t-expr f-expr)
     (let ((t-prom (delay t-expr)) (f-prom (delay f-expr)))
       (force (or (and cond? t-prom) f-prom))))))

(define if-tests
  (list
   (test (my-if #t 1 (/ 1 0)) 1)
   (test (my-if #f (/ 1 0) 1) 1)))

(run-tests if-tests)

;#5
(display "\n#5\n")

(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name1 value1) ...) expr1 ...)
     ((lambda (name1 ...) expr1 ...) value1 ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((name1 value1)) expr1 ...)
     (my-let ((name1 value1)) expr1 ...))
    ((my-let* ((name1 value1) (name2 value2) ...) expr1 ...)
     (my-let ((name1 value1))
             (my-let* ((name2 value2) ...) expr1 ...)))))

(define let-tests
  (list
   (test (my-let* () 10) 10)
   (test (my-let* ((x 100)) x) 100)
   (test (my-let* ((x 10) (y (* x 10))) (+ x y)) 110)))

(run-tests let-tests)

;#6
(display "\n#6\n")

(define-syntax when
  (syntax-rules ()
    ((when cond? expr1 ...)
     (if cond? (begin expr1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? expr1 ...)
     (if (not cond?) (begin expr1 ...)))))

(display "testing (when) and (unless)...") (newline)
(define x 100)
(when   (> x 0) (display "x > 0\n"))
(unless (< x 0) (display "x >= 0\n"))
(newline)


(define-syntax for
  (syntax-rules (in as)
    ((for i in xs expr1 ...)
     (let loop ((var xs))
       (if (not (null? var))
           (let ((i (car var)))
             expr1 ...
             (loop (cdr var))))))
    ((for xs as i expr1 ...)
     (for i in xs expr1 ...))))


(display "testing (for)...\n")
(for i in '(a b c)
  (for j in '(x y z)
    (display (list i j))
    (newline)))

(for '(a b c) as i
  (for '(x y z) as j
    (display (list i j))
    (newline)))
(newline)


(define-syntax while
  (syntax-rules ()
    ((while cond body ...)
      (let loop ()
        (when cond
          body ...
          (loop))))))

(display "Testing (while)...\n") 
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))
(newline)



(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr1 ...) until cond?)
     (let loop ()
       expr1 ...
       (if (not cond?) (loop))))))


(display "Testing (repeat ... until)...\n") 
(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))
(newline)


(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << endl . exprn)
     (begin (newline)
            (cout . exprn)))
    ((cout << expr1)
     (display expr1))
    ((cout << expr1 . exprn)
     (begin (display expr1)
            (cout . exprn)))))


(display "Testing (cout)...")
(cout << endl << "a = " << 1 << endl << "b = " << 2 << endl)
