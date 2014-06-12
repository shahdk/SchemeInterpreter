(define (test-all-8)
 (and (test-let) (test-let*) (test-and-or) (test-cond-case) (test-while) (test-map)))

(define (test-let)
 (and
  (equal? (eval-one-exp (quote (let () 3))) '3)
  (equal? (eval-one-exp (quote (let ([a 4]) a))) '4)
  (equal? (eval-one-exp (quote (let ([a (let ([b 42]) b)][c 36][d 77]) (+ a c d)))) '155)
  (equal? (eval-one-exp (quote ((lambda (x) (let ([a 3][b 4]) (+ a b x))) 5))) '12)
  (equal? (eval-one-exp (quote ((lambda (x) x) (let ([a 3][b 4]) (+ a b))))) '7)))
  
(define (test-let*)
 (and
  (equal? (eval-one-exp (quote (let* () 3))) '3)
  (equal? (eval-one-exp (quote (let* ([a 42]) a))) '42)
  (equal? (eval-one-exp (quote (let* ([a 42][b 72]) (list a b)))) '(42 72))
  (equal? (eval-one-exp (quote (let* ([a 32] [b (let* ([c 12][d (+ c a)]) (+ a c d))][c (+ b 1)]) (+ a b c)))) '209)))
  
(define (test-and-or)
 (and
  (equal? (eval-one-exp (quote (and 3 4 5))) '5)
  (equal? (eval-one-exp (quote (and))) '#t)
  (equal? (eval-one-exp (quote (let ([a (list 1)]) (and (begin (set-car! a (+ (car a) 1)) #f) (begin (set-car! a (+ (car a) 1)) #f)) a))) '(2))
  (equal? (eval-one-exp (quote (or))) '#f)
  (equal? (eval-one-exp (quote (or 3 4 5))) '3)
  (equal? (eval-one-exp (quote (let ([a (list 1)]) (or (begin (set-car! a (+ (car a) 1)) #t) (begin (set-car! a (+ (car a) 1)) #t)) a))) '(2))))
  
(define (test-cond-case)
 (and
  (equal? (eval-one-exp (quote (cond ((< 3 2) 5) ((< 4 3) 11) (else 4)))) '4)
  (equal? (eval-one-exp (quote (let ((a (cond ((< 3 2) 5) ((< 3 4) 11) (else 4)))) (+ a 8)))) '19)
  (equal? (eval-one-exp (quote (case 6 ((4) #f) ((5) #f) ((6) 17) (else #f)))) '17)
  (equal? (eval-one-exp (quote (case 6 ((4 5 7) #f) ((8 2 6) 19) (else #f)))) '19)
  (equal? (eval-one-exp (quote (case 6 ((1 2 3 4) #f) ((5 7 8 9) #t) (else 4)))) '4)))
  
(define (test-while)
 (and
  (equal? (eval-one-exp (quote (let ([a 3]) (while #f (set! a 42)) a))) '3)
  (equal? (eval-one-exp (quote (let ([a (list 1)][c (list 0)]) (while (< (car a) 20) (set-car! c (+ (car a) (car c))) (set-car! a (+ (car a) 1))) (car c)))) '190)
  (equal? (eval-one-exp (quote (let ((a (list 10))) (while (> (car a) 0) (set-car! a (- (car a) 3))) a))) '(-2))))
  
(define (test-map)
 (and
  (equal? (eval-one-exp (quote (map car '((a b) (c d) (e f))))) '(a c e))
  (equal? (eval-one-exp (quote (apply * '(3 4 5 6)))) '360)
  (equal? (eval-one-exp (quote (assq 'b '((a . 1)(b . 2) (c . 3))))) '(b . 2))
  (equal? (eval-one-exp (quote (assq 1/2 '((1 . 5)(1/2 . 6) (2 . 7))))) '#f)
  (equal? (eval-one-exp (quote (cdr (assv 1/2 '((1 . 5)(1/2 . 6) (2 . 7)))))) '6)))