;#1

(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))


(display "#1") (newline)
(display "(my-range  0 11 3) -> ")
(my-range  0 11 3) 

;------------------------

(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))

(display "(my-flatten '(1 2 (3 (4)) 5 (7 (8 9) 10) (12))) -> ")
(my-flatten '(1 2 (3 (4)) 5 (7 (8 9) 10) (12)) )
(display "(my-flatten '((a b) c d)) -> ")
(my-flatten '((a b) c d))

;------------------------

(define (my-element? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (my-element? x (cdr xs)))))

(display "(my-element? 122 '(1 2 3 122 x)) -> ")
(my-element? 122 '(1 2 3 122 x))
(display "(my-element? 12 '(1 2 3 122 x)) -> ")
(my-element? 12 '(1 2 3 122 x))

;------------------------

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

(display "(my-filter odd? (my-range 0 10 1)) -> ")
(my-filter odd? (my-range 0 10 1))
(display "(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)) -> ")
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))

;------------------------

(define (my-fold-left op xs)
  (if (<= (length xs) 1) 
      xs
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))

(display "(my-fold-left quotient '(16 2 2 2 2)) -> ")
(my-fold-left quotient '(16 2 2 2 2))
(display "(my-fold-left quotient '(1)) -> ")
(my-fold-left quotient '(1))
(display "(my-fold-left expt '(2 2 3)) -> ")
(my-fold-left expt '(2 2 3))

;------------------------

(define (my-fold-right op xs)
  (if (<= (length xs) 1)
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(display "(my-fold-right expt '(2 3 4)) -> ")
(my-fold-right expt '(2 3 4))
(display "(my-fold-right expt '(2)) -> ")
(my-fold-right expt '(2))

(newline)
;=========================

;#2

(define (list->set xs)
  (if (null? xs)
      '()
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))))

(display "#2") (newline)
(display "(list->set '(1 1 1 2 2 2 2 3 3)) -> ")
(list->set '(1 1 1 2 2 2 2 3 3))

;------------------------

(define (set? xs)
  (cond
    ((null? xs) #t)
    ((my-element? (car xs) (cdr xs)) #f)
    (else (set? (cdr xs)))))

(display "(set? '(1 2 3)) -> ")
(set? '(1 2 3))
(display "(set? '(1 2 3 3)) -> ")
(set? '(1 2 3 3))
(display "(set? '()) -> ")
(set? '()) 

;------------------------

(define (union xs ys)
  (cond
    ((null? xs) ys)
    ((my-element? (car xs) ys) (union (cdr xs) ys))
    (else (cons (car xs) (union (cdr xs) ys)))))

(display "(union '(1 2 3 5 6) '(4 5 6 7)) -> ")
(union '(1 2 3 5 6) '(4 5 6 7))

;------------------------

(define (intersection xs ys)
  (cond
    ((or (null? xs) (null? ys)) '())
    ((my-element? (car xs) ys) (cons (car xs) (intersection (cdr xs) ys)))
    (else (intersection (cdr xs) ys))))

(display "(intersection '(1 2 3 4 7 8) '(2 3 4 5 6)) -> ")
(intersection '(1 2 3 4 7 8) '(2 3 4 5 6))

;------------------------

(define (difference xs ys)
  (cond
    ((null? xs) '())
    ((my-element? (car xs) ys) (difference (cdr xs) ys))
    (else (cons (car xs) (difference (cdr xs) ys)))))

(display "(difference '(1 2 3 4 5) '(2 3)) -> ")
(difference '(1 2 3 4 5) '(2 3))

;------------------------

(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))

(display "(symmetric-difference '(1 2 3 4) '(3 4 5 6)) -> ")
(symmetric-difference '(1 2 3 4) '(3 4 5 6))

;------------------------

(define (set-eq? xs ys)
  (and (set? xs) (set? ys) (equal? (symmetric-difference xs ys) '())))

(display "(set-eq? '(1 2 3) '(2 3 4 1)) -> ")
(set-eq? '(1 2 3) '(2 3 4 1))
(display "(set-eq? '(1 2 3) '(2 3 1)) -> ")
(set-eq? '(1 2 3) '(2 3 1))
(display "(set-eq? '(a b c) '(a b c d)) -> ")
(set-eq? '(a b c) '(a b c d))
(display "(set-eq? '(a d b c) '(a b c d)) -> ")
(set-eq? '(a d b c) '(a b c d))

(newline)
;=========================

;#3

(define (string-trim-left s)
  (if (char-whitespace? (string-ref s 0)) 
      (string-trim-left (substring s 1))
      s))

(display "#3\n")
(display "(string-trim-left  \"\\t\\tabc def\") -> ")
(string-trim-left  "\t\tabc def")
(display "(string-trim-left  \"   \\t\\n\\tabc def\") -> ")
(string-trim-left  "\t\n\tabc def")

;------------------------

(define (string-trim-right s)
  (if (char-whitespace? (string-ref s (- (string-length s) 1)))
      (string-trim-right (substring s 0 (- (string-length s) 1)))
      s))

(display "(string-trim-right \"abc def\\t\\t\\t\\t\\t\\t\\t\\t   \") -> ")
(string-trim-right "abc def\t\t\t\t\t\t\t\t   ") 

;------------------------

(define (string-trim s)
  (string-trim-left (string-trim-right s)))

(display "(string-trim \"    \\t       abc def \\n       \") -> ")
(string-trim "    \t       abc def \n       ")

;------------------------

(define (string-prefix? a b)
  (cond
    ((> (string-length a) (string-length b)) #f)
    ((equal? (substring b 0 (string-length a)) a) #t)
    (else #f)))

(display "(string-prefix? \"abc\" \"abcdef\") -> ")
(string-prefix? "abc" "abcdef")
(display "(string-prefix? \"abcdef\" \"abc\") -> ")
(string-prefix? "abcdef" "abc")
(display "(string-prefix? \"bcde\" \"abcdef\") -> ")
(string-prefix? "bcde" "abcdef")

;------------------------

(define (string-suffix? a b)
  (cond
    ((> (string-length a) (string-length b)) #f)
    ((equal? (substring b (- (string-length b) (string-length a))) a) #t)
    (else #f)))

(display "(string-suffix? \"def\" \"abcdef\") -> ")
(string-suffix? "def" "abcdef")
(display "(string-suffix? \"bcd\" \"abcdef\") -> ")
(string-suffix? "bcd" "abcdef")

;------------------------

(define (string-infix? a b)
  (cond
    ((> (string-length a) (string-length b)) #f)
    ((string-prefix? a b) #t)
    (else (string-infix? a (substring b 1)))))

(display "(string-infix? \"def\" \"abcdefgh\") -> ")
(string-infix? "def" "abcdefgh")
(display "(string-infix? \"abc\" \"abcdefgh\") -> ")
(string-infix? "abc" "abcdefgh")
(display "(string-infix? \"fgh\" \"abcdefgh\") -> ")
(string-infix? "fgh" "abcdefgh")
(display "(string-infix? \"ijk\" \"abcdefgh\") -> ")
(string-infix? "ijk" "abcdefgh")
(display "(string-infix? \"bcd\" \"abc\") -> ")
(string-infix? "bcd" "abc")

;------------------------

(define (add-elem s sep)
  (cond
    ((= (string-length s) 0) (string))
    ((string-prefix? sep s) (string))
    (else (string-append (make-string 1 (string-ref s 0)) (add-elem (substring s 1) sep)))))

(define (string-split s sep)
  (cond
    ((= (string-length s) 0) (list))
    ((string-prefix? sep s) (string-split (substring s (string-length sep)) sep))
    (else (cons (add-elem s sep) (string-split (substring s (string-length (add-elem s sep))) sep)))))

(display "(string-split \"x;y;z\" \";\") -> ")
(string-split "x;y;z" ";")
(display "(string-split \"x-->y-->z\" \"-->\") -> ")
(string-split "x-->y-->z" "-->")
(display "(string-split \"abc;def;ghi;\" \";\") -> ")
(string-split "abc;def;ghi;" ";")

(newline)
;=========================

;#4



(newline)
;=========================
;#5

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)))))


(display "#5") (newline)
(display "((o f g h) 1) -> ")
((o f g h) 1)
(display "((o f g) 1) -> ")
((o f g) 1)
(display "((o h) 1) -> ")
((o h) 1)
(display "((o) 1) -> ")
((o) 1)       