;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
    (lambda ()
          '()))

(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))


(define apply-env
  (lambda (env sym)
    (if (or (null? env) (null? (car env)))
        (apply-global-env sym)
	(let* ([syms (caar env)]
               [vals (cdar env)]
        	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
                (let ([temp (vector-ref vals pos)])
                		(if (vector? temp)
                      (vector->list temp)
                      temp))
       		(apply-env env sym)))))))

(define add-to-env
  (lambda (symbols values env)
    (set-cdr! (car env) (list->vector (cons values (vector->list (cdar env)))))
    (set-car! (car env) (cons symbols (caar env)))))


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
      [(or (null? env) (null? (car env))) #t]
      [else (and ((list-of symbol?) (caar env)) (vector? (cdar env)) (environment? (cdr env)))])))

(define improper->proper
  (lambda (p)
    (if (not (pair? p))
      (cons p '())
      (cons (car p) (improper->proper (cdr p))))))

(define change-env
  (lambda (env sym val)
    (if (or (null? env) (null? (car env)))
	(extend-global-env sym val)
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [env (cdr env)])
	  (let ((pos (find-position sym syms)))
	    (if (number? pos)
		(vector-set! vals pos val)
		(change-env env sym val)))))))

(define extend-env-recur
  (lambda (syms vals env)
    (let* ([vec (list->vector vals)]
	   [new-env (cons (cons syms vec) env)])
      (for-each (lambda (item pos)
		  (if (procedure? item)
		      (vector-set! vec
				   pos
				   (cases procedure item
					  [closure-record (ids bodies toss-env)
						   (closure-record ids bodies new-env)]
					  [primitive (id)
						     item]))))
		vals
		(make-indices (- (length vals) 1) '()))
      new-env)))

(define make-indices
  (lambda (n accu)
    (if (= n 0)
	(cons 0 accu)
	(make-indices (- n 1) (cons n accu)))))

(define extend-global-env
  (lambda (sym val)
    (if (assq sym global-env)
        (set-cdr! (assq sym global-env) (list val))
        (set! global-env (cons (cons sym (list val)) global-env)))))

(define apply-global-env
  (lambda (sym)
    (let ([ele (assq sym global-env)])
      (if ele
          (cadr ele)
          (eopl:error 'apply-global-env "No binding for ~s" sym)))))
