;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
    (lambda ()
          '()))

(define extend-env
  (lambda (syms vals env)
    (improper->proper (cons (improper->proper (cons syms (list->vector vals))) env))))

(define apply-env
  (lambda (env sym)
    (if (null? env)
	(eopl:error 'apply-env "No binding for ~s" sym)
	(let* ([syms (if (= (length (caar env)) 2)
                        (cond [(eq? (caaar env) 'variable) (cdaar env)]
                          [(eq? (caaar env) 'improper) (cadaar env)]
                          [else (car (car env))])
                 (car (car env)))]
              [vals (if (= (length (caar env)) 2)
                        (cond [(eq? (caaar env) 'variable) (vector (cadar env))]
                          [(eq? (caaar env) 'improper) (list->vector (apply-env-improper (length syms) (vector->list (cadar env))))]
                          [else (cadr (car env))])
                        (cadr (car env)))]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
                (let ([temp (vector-ref vals pos)])
                		(if (vector? temp)
                      (vector->list temp)
                      temp))
       		(apply-env env sym)))))))

(define apply-env-improper
  (lambda (len vals)
    (cond
      [(= len 1) (list vals)]
      [else (append (list (car vals)) (apply-env-improper (- len 1) (cdr vals)))])))

(define find-position
  (lambda (sym ls)
    (cond [(null? ls) #f]
	  [(eq? sym (car ls)) 0]
	  [else (let ([index (find-position sym (cdr ls))])
		  (if (number? index)
		      (+ index 1)
		      #f))])))

(define environment?
  (lambda (env)
    (cond
      [(null? (car env)) #t]
      [else (and ((list-of symbol?) (caar env)) #t(vector? (cadar env)) (environment? (cdr env)))])))

(define improper->proper
  (lambda (p)
    (if (not (pair? p))
      (cons p '())
      (cons (car p) (improper->proper (cdr p))))))