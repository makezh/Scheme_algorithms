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

(run-tests tests)



;#2

(define force-return 0)
(define (exit reason) (force-return #f))

(define (find-tail xs x) ;список после элемента
  (if (equal? (car xs) x)
      (cdr xs)
      (find-tail (cdr xs) x)))

(define (push xs x) ;добавление в конец списка
  (append xs (list x)))

(define (find-head xs x) ;список до элемента
  (let loop ((xs xs) (tmp '()))
    (if (null? xs)
        #f
        (if (equal? (car xs) x)
            tmp
            (loop (cdr xs) (push tmp (car xs)))))))


(define (tail-endif program)
  (let loop ((program program) (depth -1)) ;программа -- глубина IFов
    (if (null? program)
        #f
        (let ((word (car program)))
          (cond
            ((and (equal? word 'endif) (zero? depth)) (cdr program))
            ((equal? word 'endif) (loop (cdr program) (- depth 1)))
            ((equal? word 'if) (loop (cdr program) (+ depth 1)))
            (else (loop (cdr program) depth)))))))

(define (head xs n) ;элементы до n-го включительно
  (if (or (< n 0) (null? xs))
      '()
      (cons (car xs) (head (cdr xs) (- n 1)))))

(define (parse-body program)
  (let loop ((program program) (parsed '()) (stack '())) ; программа -- обработанные выражения -- в процессе обработки
    (if (not (null? program))
        (let ((word (car program)))
          (cond
            ((equal? word 'if)
             (let ((tail (tail-endif program)))
               (if tail
                   (loop tail (push parsed (list 'if (loop (cdr program) '() (cons 'if stack)))) stack) ;если хвоста нет, то заключаем выражения в IF в скобки
                   (exit "1 BODY")))) 
            ((equal? word 'endif)
             (if (and (not (null? stack)) (equal? (car stack) 'if)) 
                 parsed 
                 (exit "2 BODY")))
            ((member word '(define end)) (exit "3 BODY"))
            (else (loop (cdr program) (push parsed word) stack)))) ;добавляем в конец списка элементы программы
        parsed)))

(define (parse-articles program)
  (let loop ((program program))
    (if (not (null? program))
        (let ((word (car program)) (other (cdr program)))
          (if (equal? word 'define)
              (if (null? other) (exit "1 ART")
                  (if (member (car other) '(if endif)) (exit "2 ART")
                      (let ((infunc (find-head (cdr other) 'end))) ;все, что внутри функции
                        (if (not infunc) (exit "3 ART")
                            (cons (cons (car other) (list (parse-body infunc))) ; парсинг внутри функции и соединение с хвостом
                                  (loop (find-tail (cdr other) 'end)))))))
              (list program)))
        (list program))))

(define (parse program)
  (call/cc
   (lambda (stack)
     (set! force-return stack)
     (let ((program (vector->list program)))
       (if (equal? (car program) 'define)
           (let ((articles (parse-articles program)))
             (cons (head articles (- (length articles) 2)) ;соединяем запарсенное в функции с оставшимся
                   (list (parse-body (last articles)))))
           (cons '() (list (parse-body program))))))))

(newline)

(define tests#2 (list
                 (test (parse #(1 2 +)) '(() (1 2 +)))
                 (test (parse #(x dup 0 swap if drop -1 endif)) '(() (x dup 0 swap (if (drop -1)))))
                 (test (parse #( define -- 1 - end
                                  define =0? dup 0 = end
                                  define =1? dup 1 = end
                                  define factorial
                                  =0? if drop 1 exit endif
                                  =1? if drop 1 exit endif
                                  dup --
                                  factorial
                                  *
                                  end
                                  0 factorial
                                  1 factorial
                                  2 factorial
                                  3 factorial
                                  4 factorial )) '(((-- (1 -))
                                                    (=0? (dup 0 =))
                                                    (=1? (dup 1 =))
                                                    (factorial
                                                     (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
                                                   (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial)))
                 (test (parse #(if 1 2 endif if 3 4)) #f)
                 (test (parse #(define word w1 w2 w3)) #f)
                 (test (parse #(define =0? dup 0 = end 
                                 define <0? dup 0 < end 
                                 define signum 
                                 =0? if exit endif 
                                 <0? if drop -1 exit endif 
                                 drop 
                                 1 
                                 end 
                                 0 signum 
                                 -2 signum )) '(((=0? (dup 0 =)) (<0? (dup 0 <)) (signum (=0? (if (exit)) <0? (if (drop -1 exit)) drop 1))) (0 signum -2 signum)))
                 (test (parse #(1 2 if + if dup - endif endif dup)) '(() (1 2 (if (+ (if (dup -)))) dup)))
                 (test (parse #(define abs 
                                 dup 0 < 
                                 if neg endif 
                                 end 
                                 9 abs 
                                 -9 abs)) '(((abs (dup 0 < (if (neg))))) (9 abs -9 abs)))
                 (test (parse #(display if end endif)) #f)
                 (test (parse #(if define if endif)) #f)
                 ))

(run-tests tests#2)