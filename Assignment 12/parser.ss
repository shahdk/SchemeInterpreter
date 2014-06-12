(define scheme-value? 
  (lambda (v) #t))
  
(define list-of
  (lambda (pred . l)
    (let ((all-preds (cons pred l)))
      (lambda (obj)
	   #t))))
        ;(let loop ((obj obj) (preds '()))
          ;(or 
            ;(and (null? obj) (null? preds))
            ;(if (null? preds)
                ;(loop obj all-preds)
                ;(and (pair? obj)
                     ;((car preds) (car obj))
                     ;(loop (cdr obj) (cdr preds))))))))))

(define expression?
 (lambda (exp)
	(if (or (null? exp) (null? (car exp)))
		#t
		(cond 
			((equal? (car exp) 'lit-exp) (scheme-value? (cadr exp)))
			((equal? (car exp)'var-exp) ((lambda (v) (or (symbol? v) (eqv? 'sys-var (car v)))) (cadr exp)))
			((equal? (car exp)'lambda-exp) (and ((lambda (var) (or ((list-of symbol?) var) (pair? var) (symbol? var))) (cadr exp))
							(expression? (caddr exp))))
			((equal? (car exp)'app-exp) (and (expression? (cadr exp))
						((list-of expression?) (caddr exp))))
			((equal? (car exp)'if-exp) (and
						(expression? (cadr exp))
						(expression? (caddr exp))
						(expression? (car (cdddr exp)))))
			((equal? (car exp)'let-exp) (and
					((list-of symbol?) (cadr exp))
					((list-of expression?) (caddr exp))
					(expression? (car (cdddr exp)))))
			((equal? (car exp)'named-let-exp)
					(name symbol?)
					(symbols (list-of symbol?))
					(values (list-of expression?))
					(body expression?))
			((equal? (car exp)'letrec-exp)
					(symbols (list-of symbol?))
					(values (list-of expression?))
					(body expression?))
			((equal? (car exp)'let*-exp)
					(symbols (list-of symbol?))
					(values (list-of expression?))
					(body expression?))
			((equal? (car exp)'set!-exp)
					(var symbol?)
					(value expression?))
			((equal? (car exp)'begin-exp)
					((list-of expression?) (cadr exp)))
			((equal? (car exp)'while-exp) (and
					(expression? (cadr exp))
					(expression? (caddr exp))))
			((equal? (car exp)'and-exp)
					((list-of expression?) (cadr exp)))
			((equal? (car exp)'or-exp)
					((list-of expression?) (cadr exp)))
			((equal? (car exp)'cond-exp)
					((list-of (lambda (cd) 
							   (and (or (expression? (car cd)) (scheme-value? (car cd)))
									(expression? (cadr cd))))) (cadr exp)))
			((equal? (car exp)'case-exp) (and
					(expression? (cadr exp))
					((list-of (lambda (cd)
							(and ((list-of expression?) (car cd))
								 (expression? (cadr cd))))) (caddr exp))))
			((equal? (car exp)'define-exp) (and
					(symbol? (cadr exp))
					(expression? (caddr exp))))
			(else #f)))))

(define param-list
      (lambda (args)
        (cond
          [(null? args) '()]
          [(atom? args) (cons 'variable (list args))]
          [(list? args)
           (if (atom? (car args)) 
               (append (list (car args)) (param-list (cdr args)))
               (if (list? (car args))
                   (append (list (car args)) (param-list (cdr args)))
                   (append (param-list (car args)) (param-list (cdr args)))))]
          [else (cons 'improper (list (improper->proper args)))])))

(define valid?
  (lambda (args)
    (cond
      [(null? args) #t]
      [else (and (isvalid (car args)) (valid? (cdr args)))])))

(define isvalid
  (lambda (arg)
    (cond
      [(null? arg) #t]
      [(not (list? arg)) #f]
      [(number? (car arg)) #f]
      [(null? (cdr arg)) #f]
      [(not (null? (cddr arg))) #f]
      [else #t])))

(define parse-expression
  (lambda (datum)
    (cond 
      [(null? datum) (list 'lit-exp datum)]
      [(symbol? datum) (list 'var-exp datum)]
      [(number? datum) (list 'lit-exp datum)]
      [(string? datum) (list 'lit-exp datum)]
      [(boolean? datum) (list 'lit-exp datum)]
      [(vector? datum) (list 'lit-exp datum)]
      [(pair? datum)
       (cond 
         [(eq? (car datum) 'quote)  (list 'lit-exp (cadr datum))]
         [(equal? (car datum) 'lambda)
          (list 'lambda-exp (param-list (cadr datum))
                   (if (= (length (cddr datum)) 1)
                       (parse-expression (caddr datum))
                       (list 'begin-exp (map parse-expression (cddr datum)))))]
         [(equal? (car datum) 'set!)
          (list 'set!-exp (cadr datum)
                    (parse-expression (caddr datum)))]
         [(equal? (car datum) 'letrec)
          (if (valid? (cadr datum))
                      (list 'letrec-exp (map car (cadr datum))
                        (map parse-expression (map cadr (cadr datum)))
                        (list 'begin-exp (map parse-expression (cddr datum))))
					  '())]
         [(equal? (car datum) 'let*)
          (if (valid? (cadr datum))
                      (list 'let*-exp (map car (cadr datum))
                        (map parse-expression (map cadr (cadr datum)))
                        (list 'begin-exp (map parse-expression (cddr datum))))
                      '())]
         [(equal? (car datum) 'let)
          (if (list? (cadr datum))
              (if (or (valid? (cadr datum)) #t)
                          (list 'let-exp (map car (cadr datum))
                            (map parse-expression (map cadr (cadr datum)))
                            (list 'begin-exp (map parse-expression (cddr datum))))
                          '())
              (list 'named-let-exp (cadr datum)
                            (map car (caddr datum))
                            (map parse-expression (map cadr (caddr datum)))
                            (list 'begin-exp (map parse-expression (cdddr datum)))))]
         [(equal? (car datum) 'if)
			(list 'if-exp (parse-expression (cadr datum)) (parse-expression (caddr datum))
                    (if (null? (cdddr datum))
                        '()
                        (parse-expression (car (cdddr datum)))))]
         [(equal? (car datum) 'begin)  (list 'begin-exp (map parse-expression (cdr datum)))]
         [(equal? (car datum) 'while)  (list 'while-exp (parse-expression (cadr datum)) (list 'begin-exp (map parse-expression (cddr datum))))]
         [(equal? (car datum) 'and)
          (list 'and-exp (map parse-expression (cdr datum)))]
         [(equal? (car datum) 'or)
          (list 'or-exp (map parse-expression (cdr datum)))]
         [(equal? (car datum) 'cond)
          (list 'cond-exp (map cd-lambda (cdr datum)))]
         [(equal? (car datum) 'case)
          (list 'case-exp (parse-expression (cadr datum)) 
                    (map (lambda (ls)
                           (list (if (equal? 'else (car ls))
                                     (list (parse-expression (cadr datum)))
									 (if (atom? (car ls)) (list 'lit-exp (car ls))
                                     (map parse-expression (car ls))))
                             (parse-expression (cadr ls))))
                      (cddr datum)))]
         [(equal? (car datum) 'define)
          (list 'define-exp (cadr datum) 
                    (parse-expression (caddr datum)))]
         [else (if (not (list? (cdr datum)))
                   '(())
                   (list 'app-exp (parse-expression (car datum))
				   (if (null? (cdr datum)) '(())
                     (map parse-expression (cdr datum)))))])])))

(define valid-clause?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(not (eq? (length (car ls)) 2)) #f]
      [(and (equal? 'else (caar ls)) (not (null? (cdr ls)))) #f]
      [else (valid-clause? (cdr ls))])))
       
(define cd-lambda
  (lambda (ls)
    (list (if (equal? 'else (car ls)) (list 'lit-exp '#t)
              (parse-expression (car ls)))
      (parse-expression (cadr ls)))))
