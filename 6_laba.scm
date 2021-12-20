(load "unit-test.scm")

;#1

;<expr> := <content> FINISH
;<content> := <frac> | <content> <frac>
;<frac> := <sign> <number> SLASH <number>
;<number> := <digit> | <number> <digit>
;<digit> := 0|1|2|3|4|5|6|7|8|9
;<sign> := + | -

;FINISH
(define break 0)
(define (FINISH) (break #f))

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
                    (begin (set-car! (cdr (assoc stage expr)) (append (cadr (assoc stage expr)) (list char))) ;добавляем символ в список числителя/знаменателя
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
         (eval (list '/ ;склеиваем в выражение вида: (/ +110/111)
                     (list (dict-ref expr 'sign)
                           (symbol->number (dict-ref expr 'num)))
                     (symbol->number (dict-ref expr 'denom))) ie))))

(define (split-fracs str)
  (let loop ((fracs '()) (temp-frac "") (listr (string->list str))) ;(цельные выражения -- формирующееся выражение -- строка, которая уже список
    (if (null? listr)
        (if (> (string-length temp-frac) 0)
            (append fracs (list temp-frac))
            fracs)
        (if (or (equal? (car listr) #\tab)
                (equal? (car listr) #\newline)
                (equal? (car listr) #\space))
            (if (> (string-length temp-frac) 0)
                (loop (append fracs (list temp-frac)) "" (cdr listr)) ;сохраняем сформированное выражение в общий список выражений
                (loop fracs temp-frac (cdr listr)))
            (loop fracs (string-append temp-frac (string (car listr))) (cdr listr)))))) ;добавляем в формирующееся выражение

;<content> := <frac> | <content> <frac>
(define (scan-many-fracs str)
  (call/cc
   (lambda (stack)
     (set! break stack) 
     (let loop ((str-fracs (split-fracs str))) 
       (if (null? str-fracs)
           (list)
           (let ((scanned-frac (scan-frac (car str-fracs)))) ;поочередно проверяем выражения
             (if scanned-frac ;если отсканировалось, то сохраняем, иначе - завершаем
                 (cons scanned-frac (loop (cdr str-fracs)))
                 (FINISH))))))))

; TESTS


(define tests
  (list
   (test (check-frac "110/111") #t)
   (test (check-frac "-4/3") #t)    
   (test (check-frac "+5/10") #t)   
   (test (check-frac "5.0/10") #f)
   (test (check-frac "FF/10") #f)
   (test (check-frac "/313") #f)
   (test (check-frac "-2a3/23") #f)

   (test (scan-frac "110/111") 110/111)
   (test (scan-frac "-4/3") -4/3)
   (test (scan-frac "+5/10") 1/2)
   (test (scan-frac "5.0/10") #f)
   (test (scan-frac "FF/10") #f)

   (test (scan-many-fracs "111/1233 11/222 -2/23") '(111/1233 11/222 -2/23))
   (test (scan-many-fracs "\t1/2 1/3\n\n10/8") '(1/2 1/3 5/4))
   (test (scan-many-fracs "\t1/2 1/3\n\n2/-5") #f)))
;(run-tests tests)

;(scan-many-fracs "\t1/2 1/3\n\n10/8")  
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")  
