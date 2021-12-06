(load "unit-test.scm")
(define ie (interaction-environment))

;#1
(display "#1\n")
(define memoized-factorial
  (let loop ((memo '()))
    (lambda (x)
      (if (<= x 1)
          1
          (let ((memo-val (assoc x memo)))
            (if memo-val
                  (cadr memo-val)
                (let ((res (* x (memoized-factorial (- x 1)))))
                  (set! memo (cons (list x res) memo))
                  res)))))))

(define fact-tests
  (list
   (test (memoized-factorial 4) 24)
   (test (memoized-factorial 0) 1)
   (test (memoized-factorial 3) 6)
   (test (memoized-factorial 5) 120)
   (test (memoized-factorial 1) 1)))
(run-tests fact-tests)

;#2
(display "\n#2\n")

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (if (= k 0)
      (list)
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))

(define (factorials)
  (let loop ((p 1) (n 1))
    (lazy-cons (* p n) (loop (* p n) (+ n 1)))))

(define (lazy-factorial n)
  (list-ref (lazy-head (factorials) n)
            (- n 1)))

;test of natural numbers
(display "Naturals:\n")
(display (lazy-head (naturals 10) 12)) (newline)

;test of lazy-factorial
(display "\nFactorial:\n")
(begin
  (display (lazy-factorial 10)) (newline)
  (display (lazy-factorial 50)) (newline))

;#3
(display "\n#3\n")
(display "load the file...\n")

(define (read-words)
  (let loop ((words '()) (word ""))
    (if (eof-object? (peek-char))
        (reverse (if (> (string-length word) 0)
                     (cons word words)
                     words))
        (let ((char (read-char)))
          (if (or (equal? char #\space)
                  (equal? char #\newline)
                  (equal? char #\tab))
              (if (> (string-length word) 0)
                  (loop (cons word words) "")
                  (loop words ""))
              (loop words (string-append word (string char))))))))


;#4
(display "\n#4\n")

(define (str<->symb s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define-syntax define-struct
  (syntax-rules ()
    ((define-struct name (field1 ...))
     (begin
       (eval (list 'define
                      (str<->symb (string-append "make-" (str<->symb 'name)))
                      (lambda (field1 ...)
                        (list (list 'type 'name) (list 'field1 field1) ...))) ie)
       (eval (list 'define
                      (str<->symb (string-append (str<->symb 'name) "?"))
                      (lambda (x)
                        (and (list? x) (not (null? x))
                             (let ((ares (assoc 'type x)))
                               (and ares (equal? (cadr ares) 'name)))))) ie)
       (eval (list 'define
                       (str<->symb (string-append (str<->symb 'name) "-" (str<->symb 'field1)))
                       (lambda (x)
                         (cadr (assoc 'field1 (cdr x))))) ie) ...
       (eval (list 'define
                       (str<->symb (string-append "set-" (str<->symb 'name) "-" (str<->symb 'field1) "!"))
                       (lambda (x val)
                         (set-car! (cdr (assoc 'field1 (cdr x))) val))) ie) ... ))))



(define-struct pos (row col)) ; Объявление типа pos
(define p (make-pos 1 2))     ; Создание значения типа pos

(define struct-tests
  (list
   (test (pos? p) #t)
   (test (pos-row p) 1)
   (test (pos-col p) 2)))
(run-tests struct-tests)

(set-pos-row! p 3) ; Изменение значения в поле row
(set-pos-col! p 4) ; Изменение значения в поле col

(define struct-tests2
  (list
   (test (pos-row p) 3)
   (test (pos-col p) 4)))

(run-tests struct-tests2)

