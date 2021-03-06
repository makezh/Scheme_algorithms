;Работает ТОЛЬКО с реализованным макросом тестирования!!!
(define (derivative xs)

  (define (elementary x) ; вычисление элементарной производной ;
    (if (list? x)
        (cond
          ((number? (car x)) 0)
          ((equal? (car x) 'x) 1)
          ((equal? (car x) '-x) -1)
          ((list? (car xs)) (cons  (elementary (car xs) (elementary (cadr xs)))))
          (else (action (car x) (cdr x))))
        (cond ((number? (car (list x))) 0)
              ((equal? (car (list x)) 'x) 1)
              ((equal? (car (list x)) '-x) -1)
              (else (action (car (list x)) (cdr (list x)))))))

  (define (loop xs)
    (if (null? xs)
        (list)
        (cons (elementary (car xs)) (loop (cdr xs)))))

  (define (action sign xs)
    (cond
      ((and (equal? sign '*) (number? (car xs))) ; (const * x)' = const * x' ;(const * fx)' == const * fx' ;
       (if (<= (length xs) 2)
           (list '* (car xs) (elementary (cadr xs)))
           (list '* (car xs) (elementary (cons '* (cdr xs))))))
      
      ((equal? sign '*) ; (fx * gx)' == fx' * gx + gx' * fx ;
       (list '+ (list '* (elementary (car xs)) (cadr xs)) (list '* (car xs) (elementary (cadr xs)))))

      ((equal? sign '/) 
       (list '/
             (list '- (list '* (elementary (car xs)) (cadr xs)) (list '* (car xs) (elementary (cadr xs))))
             (list 'expt (cadr xs) 2)))

      ((equal? sign 'expt) ; x^c == c*x^(c-1) ; c^x == c^x * ln(a) ; c - const
       (if (equal? (car xs) 'x)
           (list '* (cadr xs) (list 'expt 'x (- (cadr xs) 1)))
           (list '* (cons 'expt xs) (list 'log (car xs)))))
       
      ((equal? sign 'cos) ; cos(x)' == -sin(x) ; 
       (list '* -1 (list 'sin (car xs)) (elementary (car xs))))
  
      ((equal? sign 'sin) ; sin(x)' == cos(x) ;
       (list '* (list 'cos (car xs)) (elementary (car xs))))
          
      ((equal? sign 'exp) ; (e^x)' == e^x ;
       (list '* (list 'exp (car xs)) (elementary (car xs))))
        
      ((equal? sign 'log) ; ln(fx)' == fx'/fx ;
       (list '/ (elementary (car xs)) (car xs)))))

  (if (or (equal? (car xs) '+) (equal? (car xs) '-)) ; (fx +- gx)' = fx' +- gx' 
      (cons (car xs) (loop (cdr xs)))
      (elementary xs)))  

(load "unit-test.scm") ; Загружаем МАКРОС ТЕСТИРОВАНИЯ

(define derivative-tests
  (list (test
         (begin (eval (derivative '(5)) (interaction-environment)))
         '0) ;№1
        (test
         (begin (define x 5) (eval (derivative '(x)) (interaction-environment)))
         '1) ;№2
        (test
         (begin (define x 5) (eval (derivative '(-x)) (interaction-environment)))
         '-1) ;№3
        (test
         (begin (define x 5) (eval (derivative '(* 1 x)) (interaction-environment)))
         '1) ;№4
        (test
         (begin (define x 5) (eval (derivative '(* -1 x)) (interaction-environment)))
         '-1) ;№5
        (test
         (begin (define x 5) (eval (derivative '(* -4 x)) (interaction-environment)))
         '-4) ;№6
        (test
         (begin (define x 5) (eval (derivative '(* 10 x)) (interaction-environment)))
         '10) ;№7
        (test
         (begin (define x 5) (eval (derivative '(- (* 2 x) 3)) (interaction-environment)))
         '2) ;№8
        (test
         (begin (define x 5) (eval (derivative '(expt x 10)) (interaction-environment)))
         '19531250) ;№9
        (test
         (begin (define x 5) (eval (derivative '(* 2 (expt x 5))) (interaction-environment)))
         '6250) ;№10
        (test
         (begin (define x 5) (eval (derivative '(expt x -2)) (interaction-environment)))
         '-2/125) ;№11
        (test
         (begin (define x 5) (eval (derivative '(expt 5 x)) (interaction-environment)))
         '5029.493476356563) ;№12
        (test
         (begin (define x 5) (eval (derivative '(cos x)) (interaction-environment)))
         '0.9589242746631385) ;№13
        (test
         (begin (define x 5) (eval (derivative '(sin x)) (interaction-environment)))
         '0.2836621854632263) ;№14
        (test
         (begin (define x 5) (eval (derivative '(exp x)) (interaction-environment)))
         '148.4131591025766) ;№15
        (test
         (begin (define x 5) (eval (derivative '(* 2 (exp x))) (interaction-environment)))
         '296.8263182051532) ;№16
        (test
         (begin (define x 5) (eval (derivative '(* 2 (exp (* 2 x)))) (interaction-environment)))
         '88105.86317922687) ;№17
        (test
         (begin (define x 5) (eval (derivative '(log x)) (interaction-environment)))
         '1/5) ;№18
        (test
         (begin (define x 5) (eval (derivative '(* 3 (log x))) (interaction-environment)))
         '3/5) ;№19
        (test
         (begin (define x 5) (eval (derivative '(+ (expt x 3) (expt x 2))) (interaction-environment)))
         '85) ;№20
        (test
         (begin (define x 5) (eval (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2)))) (interaction-environment)))
         '130) ;№21
        (test
         (begin (define x 5) (eval (derivative '(/ 3 x)) (interaction-environment)))
         '-3/25) ;№22
        (test
         (begin (define x 5) (eval (derivative '(* 3/2 (expt x -2))) (interaction-environment)))
         '-3/125) ;№23
        (test
         (begin (define x 5) (eval (derivative '(* 2 (sin x) (cos x))) (interaction-environment)))
         '-1.6781430581529047) ;№24
        (test
         (begin (define x 5) (eval (derivative '(* 2 (exp x) (sin x) (cos x))) (interaction-environment)))
         '-200.43555976072966) ;№25
        (test
         (begin (define x 5) (eval (derivative '(sin (* 2 x))) (interaction-environment)))
         '-1.6781430581529049) ;№26
        (test
         (begin (define x 5) (eval (derivative '(cos (* 2 (expt x 2)))) (interaction-environment)))
         '5.247497074078575) ;№27
        (test
         (begin (define x 5) (eval (derivative '(sin (log (expt x 2)))) (interaction-environment)))
         '-0.39880605671921887) ;№28
        (test
         (begin (define x 5) (eval (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2))))) (interaction-environment)))
         '3.5693540159256703) ;№29
        (test
         (begin (define x 5) (eval (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2))))) (interaction-environment)))
         '-4.474100229696363) ;№30
        (test
         (begin (define x 5) (eval (derivative '(* (exp x) (sin x))) (interaction-environment)))
         '-100.21777988036483) ; рекомендация ;
        (test
         (begin (define x 5) (eval (derivative '(* (sin x) (exp x))) (interaction-environment)))
         '-100.21777988036483) ; рекомендация ;
        (test
         (begin (define x 5) (eval (derivative '(/ 3 x)) (interaction-environment)))
         '-3/25) ; рекомендация ;
        (test
         (begin (define x 5) (eval (derivative '(* 3 (/ 1 x))) (interaction-environment)))
         '-3/25))) ; рекомендация ;

(run-tests derivative-tests)
          



