(load "chez-init.ss")

(define scheme-value? 
  (lambda (v) #t))

(define-datatype continuation continuation?
  (halt-cont)
  (multiplication-cont
    (n number?)
    (next-cont continuation?))
  (if-cont
    (n scheme-value?)
    (m scheme-value?)
    (next-cont continuation?))
  (intersection-cont
    (n scheme-value?)
    (m scheme-value?)
    (next-cont continuation?))
  (add-cont
   (num number?)
   (next-cont continuation?))
  (cons-cont
    (n scheme-value?)
    (next-cont continuation?))
  (andmap-cont
    (ls list?)
    (pred scheme-value?)
    (k continuation?))
   (matrix-cont
    (carls list?)
    (cdrls list?)
    (k continuation?))
  (length-cont 
   (ls scheme-value?)
   (next-cont continuation?))
  (eq-cont
   (x scheme-value?)
   (next-cont continuation?))
  (set?-cont
	(n scheme-value?)
	(k continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont ()
        val]
      [multiplication-cont (n next-cont)
        (apply-cont next-cont (* val n))]
	  [set?-cont (n k)
		(if val
			(apply-cont k #f)
			(set?-cps n k))]
      [if-cont (n m next-cont)
        (if val
            (apply-cont next-cont n)
            (apply-cont next-cont m))]
      [intersection-cont (n m next-cont)
        (if val
            (intersection-cps (cdr n) m (cons-cont (car n) next-cont))
            (intersection-cps (cdr n) m next-cont))]
      [cons-cont (n next-cont)
        (apply-cont next-cont (cons n val))]
      [add-cont (num next-cont)
		     (apply-cont next-cont (+ num val))]
      [andmap-cont (ls pred k)
        (if val
            (andmap-cps pred ls k)
            (apply-cont k #f))]
      [length-cont (ls next-cont) 
			(length-cps ls (eq-cont val next-cont))]
      [eq-cont (x next-cont)
		    (apply-cont next-cont (eq? val x))]
      [matrix-cont (carls cdrls k)
        (if val
            (andmap-cps (lambda (L k) (length-cps L (length-cont carls k))) cdrls k)
            (apply-cont k #f))])))

(define member?-cps
  (lambda (val ls k)
    (if (null? ls)
        (apply-cont k #f)
        (if (equal? val (car ls))
            (apply-cont k #t)
            (member?-cps val (cdr ls) k)))))

(define set?-cps
  (lambda (ls k)
    (if (null? ls) 
        (apply-cont k #t)
        (member?-cps (car ls) (cdr ls) (set?-cont (cdr ls) k)))))

(define intersection-cps
  (lambda (los1 los2 k)
    (if (null? los1)
        (apply-cont k '())
        (member?-cps (car los1) los2 (intersection-cont los1 los2 k)))))

(define make-cps
  (lambda (proc)
    (lambda (ls k)
      (apply-cont k (proc ls)))))

(define andmap-cps
  (lambda (pred ls k)
    (if (null? ls)
        (apply-cont k #t)
        (pred (car ls) (andmap-cont (cdr ls) pred k)))))

(define list?-cps
  (lambda (ls k)
    (apply-cont k (list? ls))))

(define length-cps
  (lambda (ls k)
    (if (null? ls)
	(apply-cont k 0)
	(length-cps (cdr ls) (add-cont 1 k)))))

(define matrix?-cps
  (lambda (m k)
    (if (and (list? m) (not (null? m)) (not (null? (car m))))
        (andmap-cps list?-cps m (matrix-cont (car m) (cdr m) k))
        (apply-cont k #f))))