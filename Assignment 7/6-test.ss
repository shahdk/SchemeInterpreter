(define (test-parse-uparse)
(and
 (equal?
  (unparse-expression (parse-expression
		       (quote x)))
  'x)

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda (x) (+ x 5)))))
  '(lambda (x) (+ x 5)))

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda x y z))))
  '(lambda x y z))

 (equal?
  (unparse-expression (parse-expression
		       (quote (let ((x y)) x x x y))))
  '(let ((x y)) 
    x x x y))

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda (x) 1 z))))
  '(lambda (x) 1 z))

 (equal?
  (unparse-expression (parse-expression
		       (quote (let* ((a b) 
				     (c d) 
				     (e f)) 
				g h))))
  '(let* ((a b) 
	 (c d) 
	 (e f)) 
    g h))

 (equal?
  (unparse-expression (parse-expression
		       (quote (let* ((a b)) b))))
  '(let* ((a b)) 
    b))

 (equal?
  (unparse-expression (parse-expression 
		       (quote (lambda x x y))))
  '(lambda x 
    x 
    y))

 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(let ((x 1) 
			      (y (let () 
				   (let ((z t)) z)))) 
			  (+ z y)))))
  '(let ((x 1) 
	(y (let () 
	     (let ((z t)) 
	       z)))) 
    (+ z y)))

 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(lambda () 
			  (letrec 
			      ((foo 
				(lambda (L) 
				  (if (null? L) 
				      3 
				      (if (symbol? (car L)) 
					  (cons (car L) 
						(foo (cdr L))) 
					  (foo (cdr L))))))) 
			    foo)))))
  '(lambda () 
    (letrec 
	((foo 
	  (lambda (L) 
	    (if (null? L) 
		3 
		(if (symbol? (car L)) 
		    (cons (car L) 
			  (foo (cdr L))) 
		    (foo (cdr L))))))) 
      foo)))

 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(lambda (x) 
			  (if (boolean? x) 
			      #(1 2 3 4) 
			      1234)))))
  '(lambda (x) 
    (if (boolean? x) 
	#(1 2 3 4) 
	1234)))
  
 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda x (car x)))))
  '(lambda x (car x)))

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda (c) 
				(if (char? c) 
				    string 
				    12345)))))
  '(lambda (c) 
     (if (char? c) 
	 string 
	 12345)))

 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(lambda (datum) 
			  (or (number? datum) 
			      (boolean? datum) 
			      (null? datum) 
			      (string? datum) 
			      (symbol? datum) 
			      (pair? datum) 
			      (vector? datum))))))
  '(lambda (datum) 
    (or (number? datum) 
	(boolean? datum) 
	(null? datum) 
	(string? datum) 
	(symbol? datum) 
	(pair? datum) 
	(vector? datum))))

 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(lambda (t) 
			  (let ((l (build-list t)) 
				(sum< (lambda (a b) 
					(< (cdr a) (cdr b)))) 
				(intsum? (lambda (x) 
					   (symbol? (car x))))) 
			    (car (genmax sum< (filter intsum? l))))))))
  '(lambda (t) 
     (let ((l (build-list t)) 
	   (sum< (lambda (a b) 
		   (< (cdr a) (cdr b)))) 
	   (intsum? (lambda (x) 
		      (symbol? (car x))))) 
       (car (genmax sum< (filter intsum? l))))))
  
 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(letrec ((a (lambda () (b 2))) 
				 (b (lambda (x) (- x 4)))) 
			  (lambda () (a))))))
  '(letrec ((a (lambda () (b 2))) 
	   (b (lambda (x) (- x 4)))) 
    (lambda () (a))))

 (equal?
  (unparse-expression (parse-expression
		       (quote (let* ((a (lambda () (c 4))) 
				     (b a)) 
				(lambda (b) (a))))))
  '(let* ((a (lambda () (c 4))) 
	 (b a)) 
    (lambda (b) (a))))

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda x (cons a x)))))
  '(lambda x (cons a x)))

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda x 
				(let* ((a x) 
				       (b (cons x a))) 
				  b)))))
  '(lambda x 
    (let* ((a x) 
	   (b (cons x a))) b)))

 (equal?
  (unparse-expression (parse-expression
		       (quote (lambda (a b c) 
				(let* ((dbl (lambda x (append x x))) 
				       (lst (dbl a b c))) 
				  lst)))))
  '(lambda (a b c) 
    (let* ((dbl (lambda x (append x x))) 
	   (lst (dbl a b c))) 
      lst)))

 (equal?
  (unparse-expression (parse-expression
		       (quote 
			(lambda a 
			  (letrec 
			      ((stuff 
				(lambda (b) 
				  (if (null? b) 
				      (list) 
				      (cons (car b) (stuff (cdr b))))))) 
			    (stuff a))))))
  '(lambda a 
    (letrec 
	((stuff 
	  (lambda (b) 
	    (if (null? b) 
		(list)
		(cons (car b) (stuff (cdr b))))))) 
      (stuff a))))

 (equal?
  (unparse-expression (parse-expression 
		       (quote (lambda (a b c) 
				(let* ((a b) 
				       (d (append a c))) d)))))
  '(lambda (a b c) 
    (let* ((a b) 
	   (d (append a c))) d)))))
  