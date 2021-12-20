(load "unit-test.scm")

;#1

;<expr> := <content> FINISH
;<content> := <frac> | <content> <frac>
;<frac> := <sign> <number> SLASH <number>
;<number> := <digit> | <number> <digit>
;<digit> := 0|1|2|3|4|5|6|7|8|9
;<sign> := + | -

;FINISH
(define force-return 0)
(define (FINISH) (force-return #f))

(define ie (interaction-environment))
(define call/cc call-with-current-continuation)

(define (dict-ref dict key) ;элемент по ключу в словаре
  (cadr (assoc key dict)))

;<sign> := + | -
(define (get-sign str) ;получение знака дроби/выражения
  (if (equal? (car str) #\-)
      '-
      '+))

(define (skip-sign str) ;пропуск знака при заполнении
  (if (or (equal? (car str) #\+) (equal? (car str) #\-))
      (cdr str)
      str))

;<digit> := 0|1|2|3|4|5|6|7|8|9
(define (symbol->number x) ;хвост с элементом в число
  (string->number (list->string x)))

(define (last xs) ;последний элемент в списке
  (list-ref xs (- (length xs) 1)))

;<frac> := <sign> <number> SLASH <number>
(define (fill-frac init-str) ;преобразование строки в словарь
  (let ((expr (list (list 'sign (get-sign init-str)) (list 'num '()) (list 'denom '())))) ;{ (знак) -- (числитель) -- (знаменатель) } 
    (let loop ((init-str (skip-sign init-str)) (stage 'num))
      (if (not (null? init-str))
          (let ((char (car init-str)) (other (cdr init-str)))
            (if (equal? char #\/) ;SLASH
                (if (equal? stage 'num)
                    (loop other 'denom)
                    (set-car! (cdr (assoc stage expr)) '())) ;делаем пустым словарь, если что-то не то
                (if (char-numeric? char)
                    (begin (set-car! (cdr (assoc stage expr)) (append (cadr (assoc stage expr)) (list char)))
                           (loop other stage))
                    (set-car! (cdr (assoc stage expr)) '()))))))
    expr))


(define (check-frac str)
  (let ((expr (fill-frac (string->list str))))
    (and (not (null? (dict-ref expr 'num)))
         (not (null? (dict-ref expr 'denom))))))

(define (scan-frac str)
  (and (check-frac str)
       (let ((expr (fill-frac (string->list str))))
         (eval (list '/
                     (list (dict-ref expr 'sign)
                           (symbol->number (dict-ref expr 'num)))
                     (symbol->number (dict-ref expr 'denom))) ie))))

(define (split-fracs str)
  (let loop ((fracs '()) (it-frac "") (listr (string->list str)))
    (if (null? listr)
        (if (> (string-length it-frac) 0)
            (append fracs (list it-frac))
            fracs)
        (if (or (equal? (car listr) #\tab)
                (equal? (car listr) #\newline)
                (equal? (car listr) #\space))
            (if (> (string-length it-frac) 0)
                (loop (append fracs (list it-frac)) "" (cdr listr))
                (loop fracs it-frac (cdr listr)))
            (loop fracs (string-append it-frac (string (car listr))) (cdr listr))))))

;<content> := <frac> | <content> <frac>
(define (scan-many-fracs str)
  (call/cc
   (lambda (stack)
     (set! force-return stack)
     (let loop ((str-fracs (split-fracs str)))
       (if (null? str-fracs) '()
           (let ((scanned-frac (scan-frac (car str-fracs))))
             (if (not scanned-frac)
                 (FINISH)
                 (cons scanned-frac (loop (cdr str-fracs))))))))))

; TESTS

(check-frac "110/111")
(check-frac "-4/3") 
(check-frac "+5/10")
(check-frac "5.0/10")
(check-frac "FF/10")

(scan-frac "110/111")  
(scan-frac "-4/3")     
(scan-frac "+5/10")    
(scan-frac "5.0/10")   
(scan-frac "FF/10")    

(scan-many-fracs
 "\t1/2 1/3\n\n10/8")  
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")  
