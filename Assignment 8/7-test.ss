(define (test-all)
 (and (test-boolean-string) (test-quote) (test-if) (test-primitives) (test-begin) (test-lambda)))

(define (test-boolean-string)
 (and
  (equal? (eval-one-exp (quote (vector 1 1 1))) '#(1 1 1))
  (equal? (eval-one-exp (quote #t)) '#t)
  (equal? (eval-one-exp (quote #f)) '#f)
  (equal? (eval-one-exp (quote (quote ()))) '())
  (equal? (eval-one-exp (quote "hello")) '"hello")))
  
(define (test-quote)
 (and
  (equal? (eval-one-exp (quote (quote 3))) 3)
  (equal? (eval-one-exp (quote (quote a))) 'a)
  (equal? (eval-one-exp (quote (quote (a b c)))) '(a b c))))
  
(define (test-if)
 (and
  (equal? (eval-one-exp (quote (if 5 4 3)))	4)
  (equal? (eval-one-exp (quote (if (< 5 7) 4 3))) '4)
  (equal? (eval-one-exp (quote (if (< 5 7) 4)))	'4)
  (equal? (eval-one-exp (quote (if (< 9 7) 4 3))) '3)
  (equal? (eval-one-exp (quote (if (if (= 4 4) #t #f) (+ 4 3) (+ 42 1)))) '7)))
  
(define (test-primitives)
 (and
  (equal? (eval-one-exp (quote 5)) '5)
  (equal? (eval-one-exp (quote (/ 4 7))) '4/7)
  (equal? (eval-one-exp (quote (- (* 2 3) (* 6 3)))) '-12)
  (equal? (eval-one-exp (quote (< 5 7))) '#t)
  (equal? (eval-one-exp (quote (car (quote (4 5 6))))) '4)
  (equal? (eval-one-exp (quote (cons (quote a) (quote (b c d))))) '(a b c d))
  (equal? (eval-one-exp (quote (if (vector? #(1)) (vector 1 1 1) duh))) '#(1 1 1))
  (equal? (eval-one-exp (quote (if (not (null? (quote ()))) (vector 1 1 1) 'duh))) 'duh)
  (equal? (eval-one-exp (quote (procedure? car))) '#t)
  (equal? (eval-one-exp (quote (procedure? procedure?))) '#t)
  (equal? (eval-one-exp (quote (procedure? 5))) '#f)
  (equal? (eval-one-exp (quote (eq? (cons 2 3) (cons 2 3)))) '#f)
  (equal? (eval-one-exp (quote ((car (list car)) (quote (2 5))))) '2)))
  
(define (test-begin)
 (and
  (equal? (eval-one-exp (quote (begin 3 4 5))) '5)
  (equal? (eval-one-exp (quote (if #t (begin (+ 3 4)) 5))) '7)
  (equal? (eval-one-exp (quote (begin (begin (begin 3 4 5))))) '5)
  (equal? (eval-one-exp (quote (begin (begin (begin 3) 4) 7))) '7)))
  
(define (test-lambda)
 (and
  (equal? (eval-one-exp (quote ((lambda (x) (+ 3 x)) 6))) '9)
  (equal? (eval-one-exp (quote (((lambda (x) (lambda (y) (+ x y))) 8) 9))) '17)
  (equal? (eval-one-exp (quote ((lambda (x) (cadr x)) (quote (1 #t 3))))) '#t)
  (equal? (eval-one-exp (quote ((lambda (a) ((lambda (p a) (* a (p 2))) (lambda (x) (+ x a)) 5)) 3))) '25)
  (equal? (eval-one-exp (quote ((lambda (x) x) (quote (1 () 3))))) '(1 () 3))
  (equal? (eval-one-exp (quote ((lambda (x) (list (cadr x) (car x))) '(1 3)))) '(3 1))
  (equal? (eval-one-exp (quote (((lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))) (lambda (g) (lambda (n) (if (= n 0) 1 (* n (g (- n 1))))))) 5))) '120)
  (equal? (eval-one-exp (quote ((lambda x (cadr x)) 77 42 33))) '42)
  (equal? (eval-one-exp (quote ((lambda (x . y) (list x (cadr y))) 77 42 33))) '(77 33))
  (equal? (eval-one-exp (quote ((lambda (x y) (list x y) (list y y)) 12 14))) '(14 14))))