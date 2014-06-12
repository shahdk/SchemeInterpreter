(define-datatype continuation continuation?
  (halt-cont)
  (rep-cont)
  (error-cont)
  (cons-cont
    (v scheme-value?)
    (cont continuation?))
  (proc-cont
    (cont continuation?))
  (eval-exps-cont
    (exps (lambda (var) (or ((list-of expression?) var) (pair? var) (expression? var))))
    (env scheme-value?)
    (cont continuation?))
  (if-cont
    (true-exp expression?)
    (false-exp expression?)
    (cont continuation?)
    (env list?))
  (eval-begin-cont
    (exprs (lambda (v) (or ((list-of expression?) v) (expression? v))))
    (cont continuation?)
    (env list?))
  (while-cont
    (condition expression?)
    (body (lambda (v) (or ((list-of expression?) v) (expression? v))))
    (cont continuation?)
    (env list?))
  (while-recur-cont
    (condition expression?)
    (body (lambda (v) (or ((list-of expression?) v) (expression? v))))
    (cont continuation?)
    (env list?))
  (set!-cont
    (env list?)
    (variable (lambda (v) (or (symbol? v) ((list-of symbol?) v))))
    (cont continuation?))
  (extend-global-env-cont
    (var symbol?)
    (cont continuation?))
  (add-to-env-cont
    (name symbol?)
    (env list?)
    (cont continuation?))
  (letrec-map-cont
    (ls list?)
    (pred scheme-value?)
    (accum list?)
    (k continuation?))
  (letrec-cont 
    (symbols (lambda (v) (or (symbol? v) ((list-of symbol?) v))))
    (body (lambda (v) (or (expression? v) ((list-of expression?) v))))
    (cont continuation?)
    (env list?))
  (letrec-body-cont
    (body (lambda (v) (or (expression? v) ((list-of expression?) v))))
    (cont continuation?))
  (call/cc-cont 
    (cont continuation?))
  (map-cont 
    (proc scheme-value?)
    (ls list?)
    (cont continuation?)))

(define scheme-value? (lambda (x) #t))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont ()
        val]
      [rep-cont ()
        (pretty-print val)
        (rep)]
      [error-cont ()
        (display val)
        (newline)
        (rep)]
      [eval-exps-cont (exps env cont)
        (eval-exps exps (cons-cont val cont) env)]
      [cons-cont (v cont)
        (apply-cont cont (cons v val))]
      [proc-cont (cont)
        (apply-proc (car val) (cdr val) cont)]
      [if-cont (if-true-exp if-false-exp next-cont env)
        (if val
            (eval-expression if-true-exp next-cont env)
            (eval-expression if-false-exp next-cont env))]
      [eval-begin-cont (exprs cont env)
        (eval-begin-cps exprs cont env)]
      [while-recur-cont (condition body cont env)
        (while-cps condition body cont env)]
      [while-cont (condition body cont env)
        (if val
            (eval-expression body (while-recur-cont condition body cont env) env)
            (apply-cont cont '()))]
      [set!-cont (env variable cont)
        (change-env env variable val cont)]
      [extend-global-env-cont (name cont)
        (extend-global-env name val cont)]
      [add-to-env-cont (name env cont)
        (add-to-env name val env cont)]
      [letrec-map-cont (ls pred accum k)
        (letrec-map-cps pred ls (append accum (list val)) k)]
      [letrec-cont (syms body cont env)
        (extend-env-recur syms val env (letrec-body-cont body cont))]
      [letrec-body-cont (body cont)
        (eval-expression body cont val)]
      [map-cont (proc ls cont)
        (map-cps proc ls (cons-cont val cont))]
      [call/cc-cont (cont)
        (cases procedure val
          [closure-record (ids body env)
            (eval-expression body cont (extend-env ids
                                         (list (acontinuation cont)) env))]
          [else (apply-cont (error-cont) (format "error"))])])))
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        