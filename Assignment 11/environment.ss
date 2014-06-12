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
	(let* ([syms (if (= (length (caar env)) 2)
                        (cond [(eq? (caaar env) 'variable) (cdaar env)]
                          [(eq? (caaar env) 'improper) (cadaar env)]
                          [(eq? (caaar env) 'sys-var) (list (caar env))]
                          [else (car (car env))])
                  (car (car env)))]
              [vals (if (= (length (caar env)) 2)
                        (cond [(eq? (caaar env) 'variable) (vector (cdar env))]
                          [(eq? (caaar env) 'improper) (list->vector (apply-env-improper (length syms) (vector->list (cdar env))))]
                          [else (cdr (car env))])
                        (cdr (car env)))]
        	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
                (let ([temp (vector-ref vals pos)])
                		(if (vector? temp)
                      (vector->list temp)
                      temp))
       		(apply-env env sym)))))))

			
(define add-to-env
  (lambda (symbols values env cont)
    (set-cdr! (car env) (list->vector (cons values (vector->list (cdar env)))))
    (set-car! (car env) (cons symbols (caar env)))
    (apply-cont cont '())))


(define apply-env-improper
  (lambda (len vals)
    (cond
      [(= len 1) (list vals)]
      [else (append (list (car vals)) (apply-env-improper (- len 1) (cdr vals)))])))

(define find-position
  (lambda (sym ls)
    (cond [(null? ls) #f]
	  [(equal? sym (car ls)) 0]
	  [else (let ([index (find-position sym (cdr ls))])
		  (if (number? index)
		      (+ index 1)
		      #f))])))

(define environment?
  (lambda (env)
    (cond
      [(or (null? env) (null? (car env))) #t]
      [else (and (or ((list-of null?) (caar env)) ((list-of symbol?) (caar env))) (vector? (cdar env)) (environment? (cdr env)))])))

(define improper->proper
  (lambda (p)
    (if (not (pair? p))
      (cons p '())
      (cons (car p) (improper->proper (cdr p))))))

(define change-env
  (lambda (env sym val cont)
    (if (or (null? env) (null? (car env)))
	    (extend-global-env sym val cont)
    	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [env (cdr env)])
	     (let ((pos (find-position sym syms)))
	         (if (number? pos)
		       (begin (vector-set! vals pos val) (apply-cont cont '()))
		       (change-env env sym val cont)))))))



(define make-indices
  (lambda (n accu)
    (if (= n 0)
	(cons 0 accu)
	(make-indices (- n 1) (cons n accu)))))

(define extend-global-env
  (lambda (sym val cont)
    (if (assq sym global-env)
        (set-cdr! (assq sym global-env) (list val))
        (set! global-env (cons (cons sym (list val)) global-env)))
    (apply-cont cont '())))

(define apply-global-env
  (lambda (sym)
    (let ([ele (assq sym global-env)])
      (if ele
          (cadr ele)
          #f))))
