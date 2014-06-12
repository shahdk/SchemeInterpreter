(load "chez-init.ss")

;;; Rib cage implementation using lexical addresses and 
;;; A vector of values

(define empty-env
    (lambda ()
          '()))

(define extend-env
  (lambda (vals env)
    (cons (list->vector vals) env)))

(define apply-env
  (lambda (env depth position)
    (if (null? env)
	(eopl:error 'apply-env-lexical
		    "No binding for depth = ~s position = ~s"
		    depth position)
	(if (zero? depth)
	    (vector-ref (car env) position)
	    (apply-env (cdr env) (- depth 1) position)))))
