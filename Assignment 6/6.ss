(load "chez-init.ss")

;<expression>::= <identifier>
;            ::= <number>
;            ::= (lambda (<identifier>) <expression>)
;            ::= (if <expression> <expression> <expression>)
;            ::= (<expression> <expression>)
;(define (rl) (load "6.ss"))
;(load "6-test.ss")

(define scheme-value? 
  (lambda (v) #t))

(define-datatype expression expression?
  (lit-exp
    (v scheme-value?))
  (var-exp
    (id symbol?))
  (lambda-exp
    (variable atom?)
    (vars (lambda (v) (or ((list-of symbol?) v) (pair? v) (symbol? v))))
    (body expression?))
  (multi-body-exp
    (expressions (list-of expression?)))
  (app-exp
    (operator expression?)
    (operand (list-of expression?)))
  (if-exp
    (condition expression?)
    (if-true expression?)
    (if-false expression?))
  (let-exp
    (symbols (list-of symbol?))
    (values (list-of expression?))
    (body expression?))
  (named-let-exp
    (name symbol?)
    (symbols (list-of symbol?))
    (values (list-of expression?))
    (body expression?))
  (letrec-exp
    (symbols (list-of symbol?))
    (values (list-of expression?))
    (body expression?))
  (let*-exp
    (symbols (list-of symbol?))
    (values (list-of expression?))
    (body expression?))
  (set!-exp
    (var symbol?)
    (value expression?))
  (null-exp ))

(define param-list
      (lambda (args)
        (cond
          [(null? args) '()]
          [(pair? args)
           (if (atom? (car args)) 
               (append (list (car args)) (param-list (cdr args)))
               (append (param-list (car args)) (param-list (cdr args))))]
          [else (list args)])))

(define valid?
  (lambda (args)
    (cond
      [(null? args) #t]
      [else (and (isvalid (car args)) (valid? (cdr args)))])))

(define isvalid
  (lambda (arg)
    (cond
      [(not (list? arg)) #f]
      [(number? (car arg)) #f]
      [(null? (cdr arg)) #f]
      [(not (null? (cddr arg))) #f]
      [else #t])))

(define parse-expression
  (lambda (datum)
    (cond 
      [(null? datum) (null-exp)]
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(boolean? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(pair? datum)
       (case (car datum)
         ['quote (if (null? (cdr datum)) (eopl:error 'parse-expression "quote expr: nothing quoted")
                     (lit-exp (cadr datum)))]
         ['lambda
          (cond
            [(null? (cddr datum)) (eopl:error 'parse-expression "parse-exp: lambda expression mising body: ~s" datum)]
            [(and (not ((list-of symbol?) (cadr datum))) (not (symbol? (cadr datum))) (not (pair? (cadr datum))))
             (eopl:error 'parse-expression "parse-exp: lambda expression: incorrect formals must be symbols: ~s" datum)]
            [else (lambda-exp (if (and (atom? (cadr datum)) (not (null? (cadr datum))))
                                  'variable
                                  'list)
                                  (cadr datum)
                    (if (equal? (length (cddr datum)) 1)
                        (parse-expression (caddr datum))
                        (multi-body-exp (map parse-expression (cddr datum)))))])]
         ['set!
          (cond [(or (null? (cdr datum)) (null? (cddr datum)) (not (null? (cdddr datum))))
                 (eopl:error 'parse-expression "set! expr: incorrect number of arguments in ~s" datum)]
            [(not (symbol? (cadr datum)))
             (eopl: error 'parse-expression "set! expr: invalid argument: ~s" (cadr datum))]
            [else (set!-exp (cadr datum)
                    (parse-expression (caddr datum)))])]
         ['letrec
          (cond [(not (list? (cadr datum)))
                 (eopl:error 'parse-expression "letrec expr: bindings not a list: ~s" datum)]
            [(null? (cddr datum))
             (eopl:error 'parse-expression "letrec expr: no body: ~s" datum)]
            [else (if (valid? (cadr datum))
                      (letrec-exp (map car (cadr datum))
                        (map parse-expression (map cadr (cadr datum)))
                        (multi-body-exp (map parse-expression (cddr datum))))
                      (eopl:error 'parse-expression "letrec expr: invalid argument ~s" (cadr datum)))])]
         ['let*
          (cond [(not (list? (cadr datum)))
                 (eopl:error 'parse-expression "let* expr: bindings are not a list: ~s" datum)]
            [(null? (cddr datum))
             (eopl:error 'parse-expression "let* expr: missing body: ~s" datum)]
            [else (if (valid? (cadr datum))
                      (let*-exp (map car (cadr datum))
                        (map parse-expression (map cadr (cadr datum)))
                        (multi-body-exp (map parse-expression (cddr datum))))
                      (eopl:error 'parse-expression "let* expr: invalid arugments ~s" (cadr datum)))])]
         ['let
          (if (list? (cadr datum))
              (cond [(null? (cddr datum))
                     (eopl:error 'parse-expression "let expr: no body: ~s" datum)]
                [else (if (valid? (cadr datum))
                          (let-exp (map car (cadr datum))
                            (map parse-expression (map cadr (cadr datum)))
                            (multi-body-exp (map parse-expression (cddr datum))))
                          (eopl:error 'parse-expression "let expr: invalid arguments in ~s" datum))])
              (cond [(not (list? (caddr datum)))
                     (eopl:error 'parse-expression "named-let expr: bindings not a list ~s" datum)]
                [(null? (cdddr datum))
                 (eopl:error 'parse-expression "named-let expr: no body ~s" datum)]
                [else (if (valid? (cadr datum))
                          (named-let-exp (cadr datum)
                            (map car (caddr datum))
                            (map parse-expression (map cadr (caddr datum)))
                            (multi-body-exp (map parse-expression (cadddr datum))))
                          (eopl:error 'parse-expression "named-let expr: invalid arguments ~s" datum))]))]
         ['if
          (cond [(null? (cddr datum)) (eopl:error 'parse-expression "if-expr: empty if/else clase in ~s" datum)]
            [(not (null? (cddddr datum))) (eopl:error 'parse-expression "if-expr: too many clauses in ~s" datum)]
            [else (if-exp (parse-expression (cadr datum)) (parse-expression (caddr datum))
                    (if (null? (cdddr datum))
                        (null-exp)
                        (parse-expression (cadddr datum))))])]
         [else (if (not (list? (cdr datum)))
                   (eopl:error 'parse-expression "~s is not a proper list" datum)
                   (app-exp (parse-expression (car datum))
                     (map parse-expression (cdr datum))))])]
      [else (eopl:error 'parse-expression
              "Invalid concrete syntax ~s" datum)])))


(define unparse-arguments
  (lambda (symbols values)
    (cond
      [(null? symbols) '()]
      [else (cons (list (car symbols) (unparse-expression (car values))) (unparse-arguments (cdr symbols) (cdr values)))])))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (val) val]
      [multi-body-exp (expressions)
        (map unparse-expression expressions)]
      [lambda-exp (variable id body)           
        (cons 'lambda (append (if (equal? variable 'variable)
                                  id
                                  (list id))
                        (if (equal? (car body) 'multi-body-exp)
                            (unparse-expression body)
                            (list (unparse-expression body)))))]
      [app-exp (operator operand)
        (cons (unparse-expression operator)
          (map unparse-expression operand))]
      [if-exp (condition if-true if-false)
        (append (list 'if
               	  (unparse-expression condition)
                  (unparse-expression if-true))
          (let ([temp (unparse-expression if-false)])
            (if (null? temp) '()  (list temp))))]
      [set!-exp (variable value)
        (list 'set! variable (unparse-expression value))]
      [letrec-exp (symbols values body)
        (append (list 'letrec (unparse-arguments symbols values)) (unparse-expression body))]
      [let-exp (symbols values body)
        (append (list 'let (unparse-arguments symbols values)) (unparse-expression body))]
      [named-let-exp (name symbols values body)
        (append (list 'let name (unparse-arguments symbols values)) (unparse-expression body))]
      [let*-exp (symbols values body)
        (append (list 'let* (unparse-arguments symbols values)) (unparse-expression body))]
      [null-exp () '()])))

