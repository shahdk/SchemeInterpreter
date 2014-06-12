
(define (test-leftmost)
  (and
   (equal? (left-most '(() a)) '())
   (equal? (left-most '((a b) (c (d e)))) 'a)
   (equal? (left-most '((((c ((e f) g) h))))) 'c)))

(define (test-rightmost)
  (and
   (equal? (right-most '(a ())) '())
   (equal? (right-most '((a b) (d (c d (f (g h) i) m n) u v))) 'v)
   (equal? (right-most '((((((b (c))))))))  'c)))

(define (test-removeleftmost)
  (and
   (equal?	(remove-left-most 'e '((((a ((e e) e) e)))))	'((((a ((e) e) e)))))
   (equal?	(remove-left-most 'b '((a b) (c (b e))))	'((a) (c (b e))))
   (equal?	(remove-left-most 'a '(() a))	'(()))
   (equal?	(remove-left-most '(c d) '((a b) (c d (b e (c d)) (c d)))) '((a b) (c d (b e) (c d))))))

(define (test-removerightmost)
  (and
   (equal?	(remove-right-most 'e '((((a ((e e) e) a)))))	'((((a ((e e)) a)))))
   (equal?	(remove-right-most 'b '((a b) (c (b e))))	'((a b) (c (e))))
   (equal?	(remove-right-most 'a '(a ()))	'(()))
   (equal?	(remove-right-most '(c d) '((a b) ((c d) (b e (c d)) c d)))	'((a b) ((c d) (b e) c d)))))

(define (test-reverse)
  (and
   (equal?	(reverse '())	'())
   (equal?	(reverse '(1 2 3))	'(3 2 1))
   (equal?	(reverse '((a) (b c) (d (e))))	'(((e) d) (c b) (a)))
   (equal?	(reverse '((a b) (d (c d (f (g h) i) m n) u v)))	'((v u (n m (i (h g) f) d c) d) (b a)))))

(define (test-largest)
  (and
   (equal?	(largest '())	#f)
   (equal?	(largest '(5))	5)
   (equal?	(largest '((((7)))))	7)
   (equal?	(largest '((1) 2 (3 (4 (8)) 6) 7))	8)))

(define (test-intervalunion)
  (and
   (equal?	(interval-union '(1 5) '(2 6))	'((1 6)))
   (equal?	(interval-union '(1 5) '(2 4))	'((1 5)))
   (equal?	(interval-union '(1 5) '(5 5))	'((1 5)))
   (equal?	(interval-union '(1 5) '(15 25))	'((1 5)(15 25)))
   (equal?	(interval-union '(5 5) '(25 25))	'((5 5) (25 25)))))

(define (test-intersection)
  (and
   (equal?	(intersection '(a b d e f h i j) '(h q r i z))	'(h i))
   (equal?	(intersection '(g h i) '(j k l))	'())
   (equal?	(intersection '(a p t) '())	'())
   (equal?	(intersection '() '(g e t))	'())))
  
(define (test-insertremoveinc)
  (and
   (equal?	(empty-tree)	'())
   (equal?	(insert-inc 4 (empty-tree))	'(4 () ()))
   (equal?	(insert-inc 6 (insert-inc 4 (empty-tree)))	'(4 () (6 () ())))
   (equal?	(insert-inc 5 (insert-inc 8 (insert-inc 2 (insert-inc 6 (insert-inc 4 (empty-tree))))))	'(4 (2 () ()) (6 (5 () ()) (8 () ()))))
   (equal?	(remove-inc 7 (insert-inc 5 (insert-inc 8 (insert-inc 2 (insert-inc 6 (insert-inc 4 (empty-tree)))))))	'(4 (2 () ()) (6 (5 () ()) (8 () ()))))
   (equal?	(remove-inc 2 (insert-inc 5 (insert-inc 8 (insert-inc 2 (insert-inc 6 (insert-inc 4 (empty-tree)))))))	'(4 () (6 (5 () ()) (8 () ()))))
   (equal?	(remove-inc 4 (insert-inc 5 (insert-inc 8 (insert-inc 2 (insert-inc 6 (insert-inc 4 (empty-tree)))))))	'(2 () (6 (5 () ()) (8 () ()))))
   (equal?	(remove-inc 6 (insert-inc 5 (insert-inc 8 (insert-inc 2 (insert-inc 6 (insert-inc 4 (empty-tree)))))))	'(4 (2 () ()) (5 () (8 () ()))))))

(define (test-insertremovedec)
  (and
   (equal?	(insert-dec 4 (empty-tree))	'(4 () ()))
   (equal?	(insert-dec 6 (insert-dec 4 (empty-tree)))	'(4 (6 () ()) ()))
   (equal?	(insert-dec 5 (insert-dec 8 (insert-dec 2 (insert-dec 6 (insert-dec 4 (empty-tree))))))	'(4 (6 (8 () ()) (5 () ())) (2 () ())))
   (equal?	(remove-dec 2 (insert-dec 5 (insert-dec 8 (insert-dec 2 (insert-dec 6 (insert-dec 4 (empty-tree)))))))	'(4 (6 (8 () ()) (5 () ())) ()))
   (equal?	(remove-dec 4 (insert-dec 5 (insert-dec 8 (insert-dec 2 (insert-dec 6 (insert-dec 4 (empty-tree)))))))	'(5 (6 (8 () ()) ()) (2 () ())))
   (equal?	(remove-dec 6 (insert-dec 5 (insert-dec 8 (insert-dec 2 (insert-dec 6 (insert-dec 4 (empty-tree)))))))	'(4 (8 () (5 () ())) (2 () ())))))