(load "chez-init.ss")

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (sym symbol?)   
   (val scheme-value?)
   (env environment?)])

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (sym val env)
    (extended-env-record sym val env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
	   [empty-env-record ()
			     (eopl:error 'apply-env "No binding for ~s" sym)]
	   [extended-env-record (symbol value env)
				  (if (eq? symbol sym)
				      value
				      (apply-env env sym))])))





(define init-env
  (lambda ()
    (extend-env 'list
		list
		(extend-env 'add1
			    (lambda (x) (+ x 1))
			    (extend-env '-
					-
					(empty-env))))))
