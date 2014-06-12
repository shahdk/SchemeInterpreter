;;Dharmin Shah Assignment 7

(define (rl) (load "interpreter.ss"))

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
       	   [initial-environment global-env]
	   [result (eval-expression parse-tree initial-environment)])
      result)))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      [var-exp (id) (apply-env env id)]
      [lit-exp (val) val]
      [lambda-exp (id body) (make-closure id body env)]
      [app-exp (operator operand)
        (let* ([procedure (eval-expression operator env)]
               [arg (map (lambda (operand) (eval-expression operand env)) operand)])
              (apply-proc procedure arg env))]
      [if-exp (condition if-true if-false)
        (if (eval-expression condition env)
            (eval-expression if-true env)
            (if (not (null? (cdddr exp)))
                (eval-expression if-false env)
                '()))]
      [set!-exp (variable value) '()]
      [letrec-exp (symbols values body) '()]
      [let-exp (symbols values body) '()]
      [named-let-exp (name symbols values body) '()]
      [let*-exp (symbols values body) '()]
      [begin-exp (expressions)
        (letrec ([eval-begin (lambda (exprs)
                               (cond
                                 [(null? exprs) (void)]
                                 [(null? (cdr exprs)) (eval-expression (car exprs) env)]
                                 [else (begin (eval-expression (car exprs) env) (eval-begin (cdr exprs)))]))])
          (eval-begin expressions))]
      [null-exp () '()])))

(define make-closure
  (lambda (id body env)
    (closure-record id body env)))

(define-datatype procedure procedure?
  [closure-record
    (id (lambda (id) (or ((list-of symbol?) id) (symbol? id) (pair? id))))
    (body expression?)
    (env environment?)]
  [primitive
    (id symbol?)])

(define apply-proc
  (lambda (proc arg env)
    (cases procedure proc
      [closure-record (id body env)
        (eval-expression body (extend-env id arg env))]
      [primitive (id)
        (apply-primitive-proc id arg)])))

(define list-of-numbers?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(null? (car ls)) #f]
      [else (and (list-of-numbers? (cdr ls)) (number? (car ls)))])))

(define apply-primitive-proc
  (lambda (id arg)
    (if (null? arg) (eopl:error 'apply-primitive-proc "Missing procedure body")
        (case id
          [(+) (cond
                 [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure +>" arg)]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure +>" arg)]
                 [else (apply + arg)])]
          [(-) (cond
                 [(null? (cdr arg)) (- 0 (car arg))]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure ->" arg)]
                 [else (apply - arg)])]
          [(*) (cond
                 [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure *>" arg)]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure *>" arg)]
                 [else (apply * arg)])]
          [(/) (cond
                 [(null? (cdr arg)) (/ 1 (car arg))]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure />" arg)]
                 [(zero? (cadr arg)) (eopl:error 'apply-primitive-proc "You created a black hole by dividing with 0")]
                 [else (apply / arg)])]
          [(add1) (cond
                    [(or (not (number? (car arg))) (null? (car arg))) (eopl:error 'apply-primitive-proc "~s Not a number in #<primitive procedure add1>" (car arg))]
                    [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure add1>" arg)]
                    [else (add1 (car arg))])]
          [(sub1) (cond
                    [(or (not (number? (car arg))) (null? (car arg))) (eopl:error 'apply-primitive-proc "() Not a number in #<primitive procedure sub1>")]
                    [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure sub1>" arg)]
                    [else (sub1 (car arg))])]
          [(zero?) (cond
                     [(or (not (number? (car arg))) (null? (car arg))) (eopl:error 'apply-primitive-proc "() Not a number in #<primitive procedure zero?>")]
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure zero?>" arg)]
                     [else (zero? (car arg))])]
          [(not) (cond
                   [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure not>" arg)]
                   [else (not (car arg))])]
          [(=) (cond
                 [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure =>" arg)]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure =>" arg)]
                 [else (apply = arg)])]
          [(<) (cond
                 [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure <>" arg)]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure <>" arg)]
                 [else (apply < arg)])]
          [(>) (cond
                 [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure >>" arg)]
                 [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure >>" arg)]
                 [else (apply > arg)])]
          [(<=) (cond
                  [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure <=>" arg)]
                  [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure <=>" arg)]
                  [else (apply <= arg)])]
          [(>=) (cond
                  [(null? (cdr arg)) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure >=>" arg)]
                  [(not (list-of-numbers? arg)) (eopl:error 'apply-primitive-proc "Invalid arguments ~s in #<primitive procedure >=>" arg)]
                  [else (apply >= arg)])]
          [(cons) (cond 
                    [(or (not (null? (cddr arg))) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure cons>" arg)]
                    [else (cons (car arg) (cadr arg))])]
          [(list) (car (list arg))]
          [(null?) (cond
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure null?>" arg)]
                     [else (null? (car arg))])]
          [(eq?) (cond
                   [(or (not (null? (cddr arg))) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure eq?>" arg)]
                   [else (eq? (car arg) (cadr arg))])]
          [(equal?) (cond
                      [(or (not (null? (cddr arg))) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure eq?>" arg)]
                      [else (equal? (car arg) (cadr arg))])]
          [(atom?) (cond
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure atom?>" arg)]
                     [else (atom? (car arg))])]
          [(length) (cond
                      [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure length>" arg)]
                      [(not (list? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a proper list in #<primitive procedure length>" arg)]
                      [else (length (car arg))])]
          [(list->vector) (cond
                            [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure list-vector>" arg)]
                            [(not (list? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a proper list in #<primitive procedure list->vector>" arg)]
                            [else (list->vector (car arg))])]
          [(list?) (cond
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure list?>" arg)]
                     [else (list? (car arg))])]
          [(pair?) (cond
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure pair?>" arg)]
                     [else (pair? (car arg))])]
          [(procedure?) (cond
                          [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure procedure?>" arg)]
                          [else (procedure? (car arg))])]
          [(vector->list) (cond
                            [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure vector->list>" arg)]
                            [(not (vector? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a vector in #<primitive procedure vector->list>" arg)]
                            [else (vector->list (car arg))])]
          [(vector) (apply vector arg)]
          [(make-vector) (cond
                           [(not (number? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a number in #<primitive procedure make-vector>" (car arg))]
                           [(null? (cdr arg)) (make-vector (car arg))]
                           [(not (null? (cddr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure make-vector>" arg)]
                           [else (apply make-vector arg)])]
          [(vector-ref) (cond
                          [(or (not (null? (cddr arg))) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure vector-ref>" arg)]
                          [(not (vector? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a vector in #<primitive procedure vector-ref>" (car arg))]
                          [(not (number? (cadr arg))) (eopl:error 'apply-primitive-proc "~s is not a number in #<primitive procedure vector-ref>" (cadr arg))]
                          [else (vector-ref (car arg) (cadr arg))])]
          [(vector?) (cond
                       [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure vector?>" arg)]
                       [else (vector? (car arg))])]
          [(number?) (cond
                       [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure number?>" arg)]
                       [else (number? (car arg))])]
          [(symbol?) (cond
                       [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure symbol?>" arg)]
                       [else (symbol? (car arg))])]
          [(set-car!) (cond
                        [(or (not (null? (cddr arg))) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure set-car!>" arg)]
                        [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a pair in #<primitive procedure set-car!>" (car arg))]
                        [else (set-car! (car arg) (cadr arg))])]
          [(set-cdr!) (cond
                        [(or (not (null? (cddr arg))) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure set-cdr!>" arg)]
                        [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s is not a pair in #<primitive procedure set-cdr!>" (car arg))]
                        [else (set-cdr! (car arg) (cadr arg))])]
          [(vector-set!) (cond
                           [(or (not (null? (cdddr arg))) (null? (cddr arg)) (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure set-car!>" arg)]
                           [(not (vector? (car arg))) (eopl:error 'apply-primitive-proc "~s is not vector in #<primitive procedure vector-set!>" (car arg))]
                           [(not (number? (cadr arg))) (eopl:error 'apply-primitive-proc "~s is not a number in #<primitive procedure vector-set!>" (cadr arg))]
                           [else (vector-set! (car arg) (cadr arg) (caddr arg))])]
          [(car) (cond 
                   [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure car>" arg)]
                   [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure car>" arg)]
                   [else (car (car arg))])]
          [(cdr) (cond 
                   [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure cdr>" arg)]
                   [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure cdr>" arg)]
                   [else (cdr (car arg))])]
          [(caar) (cond 
                    [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                    [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                    [(atom? (caar arg)) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure caar>" (car arg))]
                    [else (caar (car arg))])]
          [(cadr) (cond 
                    [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                    [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                    [(atom? (cdar arg)) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cadr>" (car arg))]
                    [else (cadr (car arg))])]
          [(cdar) (cond 
                    [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                    [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                    [(atom? (caar arg)) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cdar>" (car arg))]
                    [else (cdar (car arg))])]
          [(cddr) (cond 
                    [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                    [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                    [(atom? (cdar arg)) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cddr>" (car arg))]
                    [else (cddr (car arg))])]
          [(caaar) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (caar arg)) (atom? (caaar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure caaar>" (car arg))]
                     [else (caaar (car arg))])]
          [(caadr) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (cdar arg)) (atom? (cadar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure caadr>" (car arg))]
                     [else (caadr (car arg))])]
          [(cadar) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (caar arg)) (atom? (cdaar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cadar>" (car arg))]
                     [else (cadar (car arg))])]
          [(cdaar) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (caar arg)) (atom? (caaar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cdaar>" (car arg))]
                     [else (cdaar (car arg))])]
          [(cdddr) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (cdar arg)) (atom? (cddar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cdddr>" (car arg))]
                     [else (cdddr (car arg))])]
          [(cddar) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (caar arg)) (atom? (cdaar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cddar>" (car arg))]
                     [else (cddar (car arg))])]
          [(cdadr) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (cdar arg)) (atom? (cadar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure cdadr>" (car arg))]
                     [else (cdadr (car arg))])]
          [(caddr) (cond 
                     [(not (null? (cdr arg))) (eopl:error 'apply-primitive-proc "Incorrect argument count ~s in #<primitive procedure caar>" arg)]
                     [(not (pair? (car arg))) (eopl:error 'apply-primitive-proc "~s not a pair in #<primitive procedure caar>" arg)]
                     [(or (atom? (cdar arg)) (atom? (cddar arg))) (eopl:error 'apply-primitive-proc "Incorrect list structure ~s in #<primitive procedure caddr>" (car arg))]
                     [else (caddr (car arg))])]
          [else (eopl:error 'apply-primitive-proc "Incorrect primitive procedure ~s" id)]))))

(define primitive-proc
  (list '+ '- '* '/ 'add1 'sub1 
    'zero? 'not '= '< '> '<= '>= 
    'cons 'list 'null? 'eq? 'equal? 
    'atom? 'length 'list->vector 'list? 
    'pair? 'procedure? 'vector->list 'vector 
    'make-vector 'vector-ref 'vector? 'number? 
    'symbol? 'set-car! 'set-cdr! 'vector-set!
    'car 'cdr 'caar 'cadr 'cdar 'cddr 'caaar 
    'caadr 'cadar 'cdaar 'cdddr 'cddar 'cdadr 'caddr))

(define global-env
  (extend-env primitive-proc (map primitive primitive-proc) (empty-env)))