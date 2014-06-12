(define-datatype expression expression?
  (lit-exp
   (value scheme-value?))
  (var-exp
   (id symbol?))
  (lambda-exp
   (id symbol?)
   (body expression?))
  (let-exp
   (syms (list-of symbol?))
   (val (list-of expression?))
   (body expression?))
  (app-exp
   (rator expression?)
   (rand expression?)))

(define scheme-value? (lambda (v) #t))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond [(eqv? (car datum) 'lambda)
		  (lambda-exp (caadr datum)
			      (parse-expression (caddr datum)))]
		 [(eq? (car datum) 'let)
			(let-exp (map car (cadr datum))
					 (map parse-expression (map cadr (cadr datum)))
					 (parse-expression (cddr datum)))]
		 [else (app-exp
			(parse-expression (car datum))
			(parse-expression (cadr datum)))])]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntax ~s" datum)])))

[let-exp (syms vals body)
		 (eval-expression body (extend-env syms (map (eval-expression-env env) vals) env))]
		
(define eval-expression-env
 (lambda (env)
  (lambda (exp)
   (eval-expression exp env))))