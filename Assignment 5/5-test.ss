(define (test-connected?)
  (and
    (equal? (connected? (quote ((a ())))) #t)
    (equal? (connected? (quote ((a (b)) (b (a))))) #t)
    (equal? (connected? (quote ((a (b c)) (b (c a)) (c (b a))))) #t)
    (equal? (connected? (quote ((a (b)) (b (a)) (c (d)) (d (c))))) #f)
    (equal? (connected? (quote ((a (b c)) (b (a c)) (c (b a)) (d (e f)) (e (d f)) (f (d e))))) #f)
    (equal? (connected? (quote ((a ()) (b ())))) #f)
    (equal? (connected? (quote ((a (b)) (c ()) (b (a))))) #f)
    (equal? (connected? (quote ((a (b c)) (b (c a)) (c (b a)) (d (e f g)) (e (d f g)) (f (d e g)) (g (d e f))))) #f)
    (equal? (connected? (quote ((a (b)) (k (j)) (j (k i)) (c (b d)) (i (h j)) (h (i g)) (g (f h)) (b (c a)) (f (e g)) (e (d f)) (d (c e))))) #t)))
	
(define (test-iterator)
  (and
    (equal? (let ((iter (make-slist-leaf-iterator (quote ((a b ())))))) (begin (iter) (iter)) (iter)) #f)
    (equal? (let ((iter (make-slist-leaf-iterator (quote ((())))))) (iter)) #f)
    (equal? (let ((iter (make-slist-leaf-iterator (quote ((() a (b c d ()) () e f g)))))) (iter)) 'a)
    (equal? (let ((iter (make-slist-leaf-iterator (quote ((() a (b () c (d ())) () e f g)))))) (begin (iter) (iter) (iter) (iter)) (iter)) 'e)
    (equal? (let ((iter (make-slist-leaf-iterator (quote ((() (() ()) (a) (z (x) d ()) () e f g)))))) (begin (iter)) (iter)) 'z)
    (equal? (let ((iter1 (make-slist-leaf-iterator (quote (a (b (c) (d)) (((e))))))) (iter2 (make-slist-leaf-iterator (quote (z (x n (v) ((m)))))))) (let loop ((count 2) (accum (quote ()))) (if (>= count 0) (loop (- count 1) (cons (iter1) (cons (iter2) accum))) accum))) '(c n b x a z))
    (equal? (let ((iter (make-slist-leaf-iterator (quote ((() (z) (a (x) d ()) () e f g)))))) (begin (iter) (iter) (iter) (iter) (iter) (iter) (iter) (iter) (iter)) (iter)) #f)))
	
(define  (test-subst-leftmost)
	(and
	(equal? (subst-leftmost (quote d) (quote e) (quote (a b c)) eq?) '(a b c))
	(equal? (subst-leftmost (quote d) (quote c) (quote (a b c)) eq?) '(a b d))
	(equal? (subst-leftmost (quote a) (quote b) (quote ()) eq?) '())
	(equal? (subst-leftmost (quote h) (quote i) (quote (a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((g))) d f g)) eq?) '(a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((g))) d f g))
	(equal? (subst-leftmost (quote h) (quote d) (quote (a b (((c b) c) a e (d (q (r))) (((s) d) s) () ((t))) u v w)) eq?) '(a b (((c b) c) a e (h (q (r))) (((s) d) s) () ((t))) u v w))
	(equal? (subst-leftmost (quote h) (quote f) (quote (a b (((c b) c) a e (d (d (d))) (((f) q) q) () ((r))) t v v)) eq?) '(a b (((c b) c) a e (d (d (d))) (((h) q) q) () ((r))) t v v))
	(equal? (subst-leftmost (quote h) (quote g) (quote (a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((g))) z g z)) eq?) '(a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((h))) z g z))
	(equal? (subst-leftmost (quote h) (quote g) (quote (((() () (a) () (a) b ((() c) ()) c b a)))) eq?) '(((() () (a) () (a) b ((() c) ()) c b a))))
	(equal? (subst-leftmost (quote h) (quote a) (quote (((() () (a) () (q) r ((() s) ()) t a v)))) eq?) '(((() () (h) () (q) r ((() s) ()) t a v))))
	(equal? (subst-leftmost (quote 1) (quote -10) (quote (3 (2 5) (2 (3 (4))) (-10 (6 7) (8) 9 10))) =) '(3 (2 5) (2 (3 (4))) (1 (6 7) (8) 9 10)))
	(equal? ((lambda() (define equal? test-equal?) (subst-leftmost (quote d) (quote e) (quote (a b c)) (make-fail-pred-eq? eq?)))) '(a b c))
	(equal? ((lambda ()(define equal? test-equal?) (subst-leftmost (quote d) (quote c) (quote (a b c)) (make-fail-pred-eq? eq?)))) '(a b d))
	(equal? ((lambda ()(define equal? test-equal?) (subst-leftmost (quote h) (quote i) (quote (a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((g))) d f g)) (make-fail-pred-eq? eq?)))) '(a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((g))) d f g))
	(equal? ((lambda () (define equal? test-equal?) (subst-leftmost (quote h) (quote d) (quote (a b (((c b) c) a e (d (q (r))) (((s) s) s) () ((t))) u v w)) (make-fail-pred-eq? eq?)))) '(a b (((c b) c) a e (h (q (r))) (((s) s) s) () ((t))) u v w))
	(equal? ((lambda () (define equal? test-equal?) (subst-leftmost (quote h) (quote f) (quote (a b (((c b) c) a e (d (d (d))) (((f) q) q) () ((r))) t v v)) (make-fail-pred-eq? eq?)))) '(a b (((c b) c) a e (d (d (d))) (((h) q) q) () ((r))) t v v))
	(equal? ((lambda () (define equal? test-equal?) (subst-leftmost (quote h) (quote g) (quote (a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((g))) z z z)) (make-fail-pred-eq? eq?)))) '(a b (((c b) c) a e (d (d (d))) (((f) f) f) () ((h))) z z z))
	(equal? ((lambda () (define equal? test-equal?) (subst-leftmost (quote h) (quote g) (quote (((() () (a) () (a) b ((() c) ()) c b a)))) (make-fail-pred-eq? eq?)))) '(((() () (a) () (a) b ((() c) ()) c b a))))
	(equal? ((lambda () (define equal? test-equal?) (subst-leftmost (quote h) (quote a) (quote (((() () (a) () (q) r ((() s) ()) t u v)))) (make-fail-pred-eq? eq?)))) '(((() () (h) () (q) r ((() s) ()) t u v))))
	(equal? ((lambda () (define equal? test-equal?) (subst-leftmost (quote 1) (quote -10) (quote (3 (2 5) (2 (3 (4))) (-10 (6 7) (8) 9 10))) (make-fail-pred-eq? =)))) '(3 (2 5) (2 (3 (4))) (1 (6 7) (8) 9 10)))))
	