(define rep
 (lambda ()
    (display ">>> ")
    (let ([foo (read)])
      (if (not (equal? foo '(exit)))
      (begin (write (eval-one-exp foo)) (newline) (rep))
	  (display "Bye...")))))
	  
(define eval-one-exp
  (lambda (exp)
	;(display (expand-syntax (parse-expression exp)))
	;(newline)
    (let* ([parse-tree (parse-expression exp)]
           [expanded-tree (expand-syntax parse-tree)]
           [result (top-level-eval expanded-tree)])
      result)))
	  
(define top-level-eval
  (lambda (form)
	(cond 
	   [(equal? (car form) 'define-exp)
	     (extend-global-env (cadr form) (eval-expression (caddr form) (empty-env)))]
	   [else (eval-expression form (empty-env))])))

(define expand-syntax
  (lambda (exp)
	(if (null? exp)
	'()
    (cond
      [(equal? (car exp) 'lambda-exp)
        (list 'lambda-exp (cadr exp) (expand-syntax (caddr exp)))]
      [(equal? (car exp) 'app-exp) (list 'app-exp (expand-syntax (cadr exp)) (map expand-syntax (caddr exp)))]
      [(equal? (car exp) 'if-exp)  (list 'if-exp (expand-syntax (cadr exp)) (expand-syntax (caddr exp)) (expand-syntax (car (cdddr exp))))]
      [(equal? (car exp) 'set!-exp) (list 'set!-exp (cadr exp) (expand-syntax (caddr exp)))]
      [(equal? (car exp) 'letrec-exp) (list 'letrec-exp (cadr exp) (map expand-syntax (caddr exp)) (expand-syntax (car (cdddr exp))))]
      [(equal? (car exp) 'let-exp) (list 'app-exp (list 'lambda-exp (cadr exp) (expand-syntax (car (cdddr exp)))) (map expand-syntax (caddr exp)))]
      [(equal? (car exp) 'named-let-exp) (expand-syntax (list 'app-exp (list 'letrec-exp (list (cadr exp)) (list (list 'lambda-exp (caddr exp) (cadr (cdddr exp)))) (list 'var-exp (cadr exp))) (car (cdddr exp))))]
      [(equal? (car exp) 'let*-exp) 
        (cond
          [(and (null? (cadr exp)) (null? (caddr exp))) (expand-syntax (car (cdddr exp)))]
          [else (list 'app-exp (list 'lambda-exp (list (car (cadr exp)))
                           (expand-syntax (list 'let*-exp (cdr (cadr exp)) (cdr (caddr exp)) (car (cdddr exp)))))
                  (list (expand-syntax (car (caddr exp)))))])]
      [(equal? (car exp) 'begin-exp) (list 'begin-exp (map expand-syntax (cadr exp)))]
      [(equal? (car exp) 'define-exp) (list 'define-exp (cadr exp) (expand-syntax (caddr exp)))]
      [(equal? (car exp) 'and-exp) 
        (if (null? (cadr exp))
            (list 'lit-exp '#t)
            (if (null? (cdr (cadr exp)))
                (expand-syntax (car (cadr exp)))
                (expand-syntax (list 'if-exp (car (cadr exp)) (list 'and-exp (cdr (cadr exp))) (list 'lit-exp '#f)))))]
      [(equal? (car exp) 'or-exp) 
        (if (null? (cadr exp))
            (list 'lit-exp '#f)
            (if (null? (cdr (cadr exp)))
                (expand-syntax (car (cadr exp)))
                (expand-syntax (list 'let-exp (cons 'sys-var (list 'tempVal)) (list (car (cadr exp)))
                                 (list 'if-exp (list 'var-exp (cons 'sys-var (list 'tempVal))) 
                                   (list 'var-exp (cons 'sys-var (list 'tempVal)))
                                   (list 'or-exp (cdr (cadr exp))))))))]
      [(equal? (car exp) 'cond-exp) 
        (cond 
          [(null? (cadr exp)) '()]
          [else (expand-syntax (list 'if-exp (caar (cadr exp)) (cadar (cadr exp))
                                 (list 'cond-exp (cdr (cadr exp)))))])]
      [(equal? (car exp) 'case-exp)
        (let ([case-key (expand-syntax (cadr exp))])
          (cond 
            [(null? (caddr exp)) '()]
            [else (expand-syntax (list 'if-exp (list 'or-exp (map (lambda (ls)
                                                        (list 'app-exp (list 'var-exp 'equal?) (list ls case-key))) (caar (caddr exp))))
                                   (cadar (caddr exp))
                                   (list 'case-exp (cadr exp) (cdr (caddr exp)))))]))]
      [else exp]))))


(define eval-expression
  (lambda (exp env)
  (if (null? exp)
	'()
    (cond 
      [(equal? (car exp) 'var-exp) 	(apply-env env (cadr exp))]
      [(equal? (car exp) 'lit-exp) (cadr exp)]
      [(equal? (car exp) 'lambda-exp) (make-closure (cadr exp) (caddr exp) env)]
      [(equal? (car exp) 'app-exp)
        (let* ([procedure (eval-expression (cadr exp) env)]
               [arg (map (lambda (operand) (eval-expression operand env)) (caddr exp))])
              (apply-proc procedure arg env))]
      [(equal? (car exp) 'if-exp)
        (if (eval-expression (cadr exp) env)
            (eval-expression (caddr exp) env)
            (if (not (null? (cdddr exp)))
                (eval-expression (car (cdddr exp)) env)
                '()))]
      [(equal? (car exp) 'set!-exp)
        (change-env env (cadr exp) (eval-expression (caddr exp) env))]
	  
      [(equal? (car exp) 'letrec-exp)
        (cond
          [else (let* ([vals (map (lambda (v) (eval-expression v env)) (caddr exp))]
                       [letrec-env (extend-env-recur (cadr exp) vals env)])
                 	(eval-expression (car (cdddr exp)) letrec-env))])]

      [(equal? (car exp) 'begin-exp)
        (letrec ([eval-begin (lambda (exprs)
                               (cond
                                 [(null? exprs) '()]
                                 [(null? (cdr exprs)) (eval-expression (car exprs) env)]
                                 [else (begin (eval-expression (car exprs) env) (eval-begin (cdr exprs)))]))])
          (eval-begin (cadr exp)))]
      [(equal? (car exp) 'while-exp)
        (letrec ([loop (lambda ()
                         (if (eval-expression (cadr exp) env)
                             (begin (eval-expression (caddr exp) env) (loop))))])
		    (loop))]
      [(equal? (car exp) 'define-exp)
        (if (null? env)
            (extend-global-env (cadr exp) (eval-expression (caddr exp) (empty-env)))
            (add-to-env (cadr exp) (eval-expression (caddr exp) env) env))]          
      ))))


(define null-list
  (lambda (len accum)
    (cond 
      [(= len 0) accum]
      [else (null-list (- len 1) (append accum (list '#f)))]))) 

(define procedure?
	(lambda (proc)
		(if (atom? proc)
			#f
		(cond 
			[(equal? (car proc) 'closure-record) (and ((lambda (id) (or ((list-of symbol?) id) (symbol? id) (pair? id))) (cadr proc))
								 (expression? (caddr proc)) (environment? (car (cdddr proc))))]
			[(equal? (car proc) 'primitive) (symbol? (cadr proc))]
			[else #f]))))
      
(define make-closure
  (lambda (id body env)
    (list 'closure-record id body env)))

(define get-proc
	(lambda (proc)
	   (if (atom? (car proc))
	       proc
		   (get-proc (car proc)))))
	
(define apply-proc
  (lambda (proc arg env)
    (let ([proc (get-proc proc)])
    (cond
      [(equal? (car proc) 'closure-record)
        (eval-expression (caddr proc) (extend-env (cadr proc) arg (car (cdddr proc))))]
      [(equal? (car proc) 'primitive)
        (apply-primitive-proc (cadr proc) arg env)]))))

(define list-of-numbers?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(null? (car ls)) #f]
      [else (and (list-of-numbers? (cdr ls)) (number? (car ls)))])))

(define apply-primitive-proc
  (lambda (id arg env)
        (cond
          [(equal? id '+) (apply + arg)]
          [(equal? id '-) (apply - arg)]
          [(equal? id '*) (apply * arg)]
          [(equal? id '/) (apply / arg)]
		  [(equal? id 'string?) (apply string? arg)]
		  [(equal? id 'boolean?) (apply boolean? arg)]
          [(equal? id 'add1) (apply add1 arg)]
          [(equal? id 'sub1) (apply sub1 arg)]
          [(equal? id 'zero?) (apply zero? arg)]
          [(equal? id 'not) (apply not arg)]
          [(equal? id '=) (apply = arg)]
          [(equal? id '<) (apply < arg)]
          [(equal? id '>) (apply > arg)]
          [(equal? id '<=) (apply <= arg)]
          [(equal? id '>=) (apply >= arg)]
          [(equal? id 'cons) (apply cons arg)]
          [(equal? id 'list)  (apply list arg)]
          [(equal? id 'null?) (apply null? arg)]
          [(equal? id 'eq?) (apply eq? arg)]
          [(equal? id 'equal?) (apply equal? arg)]
          [(equal? id 'atom?) (apply atom? arg)]
          [(equal? id 'length) (apply length arg)]
          [(equal? id 'list->vector) (apply list->vector arg)]
          [(equal? id 'list?) (apply list? arg)]
          [(equal? id 'pair?) (apply pair? arg)]
          [(equal? id 'procedure?) (apply procedure? arg)]
          [(equal? id 'vector->list) (apply vector->list arg)]
          [(equal? id 'vector) (apply vector arg)]
          [(equal? id 'make-vector) (apply make-vector arg)]
          [(equal? id 'vector-ref) (apply vector-ref arg)]
		  [(equal? id 'list-ref) (apply list-ref arg)]
          [(equal? id 'vector?) (apply vector? arg)]
          [(equal? id 'number?) (apply number? arg)]
          [(equal? id 'symbol?) (apply symbol? arg)]
          [(equal? id 'set-car!) (apply set-car! arg)]
          [(equal? id 'set-cdr!) (apply set-cdr! arg)]
          [(equal? id 'vector-set!) (apply vector-set! arg)]
          [(equal? id 'car) (apply car arg)]
          [(equal? id 'cdr) (apply cdr arg)]
          [(equal? id 'caar) (apply caar arg)]
          [(equal? id 'cadr) (apply cadr arg)]
          [(equal? id 'cdar) (apply cdar arg)]
          [(equal? id 'cddr) (apply cddr arg)]
          [(equal? id 'caaar) (apply caaar arg)]
          [(equal? id 'caadr) (apply caadr arg)]
          [(equal? id 'cadar) (apply cadar arg)]
          [(equal? id 'cdaar) (apply cdaar arg)]
          [(equal? id 'cdddr) (apply cdddr arg)]
          [(equal? id 'cddar) (apply cddar arg)]
          [(equal? id 'cdadr) (apply cdadr arg)]
          [(equal? id 'caddr) (apply caddr arg)]
          [(equal? id 'map) (map (lambda (args)
                                (apply-proc (car arg) args  env)) (index-recur (cdr arg)))]
          [(equal? id 'apply) (apply-proc (car arg) (combine-arg (cdr arg)) env)]
          [(equal? id 'assq) (apply assq arg)]
          [(equal? id 'assv) (apply assv arg)]
          [(equal? id 'append) (let f ([ls '()] [args arg])
                              (if (null? args)
                                  ls
                                  (let g ([ls ls])
                                    (if (null? ls)
                                        (f (car args) (cdr args))
                                        (cons (car ls) (g (cdr ls)))))))]
          [(equal? id 'max) (apply max arg)]
		  [(equal? id 'open-input-file) (apply open-input-file arg)]
		  [(equal? id 'eof-object?) (apply eof-object? arg)]
		  [(equal? id 'close-port) (apply close-port arg)]
		  [(equal? id 'write) (apply write arg)]
		  [(equal? id 'display) (apply display arg)]
		  [(equal? id 'read) (let ([temp (if (null? (car arg)) (read) (apply read arg))])
						;(display (car temp))
						;(newline)
						temp)]
		  [(equal? id 'newline) (newline)]
          [(equal? id 'load) (let ([p (open-input-file (car arg))])
                    (let f ([x (read p)])
                      (if (eof-object? x)
                        		(begin (close-port p) '())
                          (begin (eval-one-exp x) (f (read p))))))]
          )))

(define combine-arg
  (lambda (ls)
    (cond 
      [(null? ls) '()]
      [(list? (car ls)) (append  (car ls) (combine-arg (cdr ls)))]
      [else (append (list (car ls)) (combine-arg (cdr ls)))])))

(define index-recur
  (lambda (ls)
    (if (null? (cdr ls)) 
        (letrec ([loop (lambda (ls accum)
                         (if (null? ls) accum
                             (loop (cdr ls) (append accum (list (list (car ls)))))))])
          (loop (car ls) '()))
        (let ([size (- (length (car ls)) 1)])
          (letrec ([index-recur-helper (lambda (count)
                                        	(cond [(eq? count size) (list (index-value count ls))]
                                        			[else (append (list (index-value count ls)) (index-recur-helper (+ count 1)))]))])
            (index-recur-helper 0))))))
                                                                           
                                
(define index-value
  (lambda (index ls)
    (letrec ([index-helper (lambda (index ls accum count)
                             (cond 
                               [(null? ls) accum]
                               [(list? (car ls)) (index-helper index (cdr ls) (append accum (list (index-helper index (car ls) '() 0))) count)]
                               [else (if (eq? index count)
                                         (car ls)
                                         (index-helper index (cdr ls) '() (+ count 1)))]))])
      (index-helper index ls '() 0))))




(define reset-global-env
  (lambda ()
    (set! global-env (map (lambda (name) (cons name (list (list 'primitive name)))) primitive-proc))))
	
(define extend-env-recur
  (lambda (syms vals env)
    (let* ([vec (list->vector vals)]
	   [new-env (cons (cons syms vec) env)]
	   [pos-list (make-indices (- (length vals) 1) '())])
	   (letrec ([loop (lambda (item pos)
                     (if (and (not (null? item)) (procedure? (car item)))
                        	(begin (vector-set! vec
                             	    (car pos)
                                  (cond
                                 			[(equal? (caar item) 'closure-record)
                                   			(list 'closure-record (car (cdar item)) (car (cddar item)) new-env)]
                                    [(equal? (caar item) 'primitive)
                                   			(car item)])) (loop (cdr item) (cdr pos)))))])
		(loop vals pos-list))
      new-env)))
	  
(define primitive-proc
  (list '+ '- '* '/ 'add1 'sub1 'max 'display 'read 'list-ref
    'zero? 'not '= '< '> '<= '>= 'close-port 'newline
    'cons 'list 'null? 'eq? 'equal? 'eof-object? 'write
    'atom? 'length 'list->vector 'list? 'open-input-file
    'pair? 'procedure? 'vector->list 'vector 'boolean?
    'make-vector 'vector-ref 'vector? 'number? 'string?
    'symbol? 'set-car! 'set-cdr! 'vector-set!
    'car 'cdr 'caar 'cadr 'cdar 'cddr 'caaar 'load
    'caadr 'cadar 'cdaar 'cdddr 'cddar 'cdadr 'caddr 'map 'apply 'assq 'assv 'append))

(define global-env
  (map (lambda (name) (cons name (list (list 'primitive name)))) primitive-proc))
