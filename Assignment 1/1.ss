;;Dharmin Shah 1

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ (length (cdr ls)) 1))))

;;Problem 1
(define last
  (lambda (x)
    (if (= (length x) 1)
	(car x)
	(last (cdr x)))))
			
			
;;Problem 2
(define all-but-last
  (lambda (x)
    (cond
      [(= (length x) 1) '()]
      [(= (length x) 2) (cons (car x) '())]
      [else	(cons (car x) (all-but-last(cdr x)))])))

;;Problem 3
(define replace
  (lambda (old new ls)
    (cond
      [(null? ls) '()]
      [(eqv? (car ls) old) (cons new (replace old new (cdr ls)))]
      [else (cons (car ls) (replace old new (cdr ls)))])))
      

;;Problem 4
(define remove-first
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? (car ls) x) (cdr ls)]
      [else (cons (car ls) (remove-first x (cdr ls)))])))

;;Problem 5
;;;(define remove-last
;  (lambda (x ls)
;    (cond
;      [(null? ls) '()]
;      [(eqv? (last ls) x) (all-but-last ls)]
;      [else (append (remove-last x (all-but-last ls)) (cons (last ls) '()))])))
;;; Solution 1
;(define remove-last
;  (lambda (e ls)
;    (remove-last-helper e ls '() '())))

;(define remove-last-helper  
;  (lambda (e ls current backup)
;    (cond [(null? ls) current]
;	  [(eq? e (car ls))
;	   (remove-last-helper e (cdr ls) backup (append backup (list (car ls))))]
;	  [else (remove-last-helper e (cdr ls) (append current (list (car ls)))
;				               (append backup (list (car ls))))])))


;;; Solution 2
(define remove-last
  (lambda (e ls)
    (remove-last-helper ls (get-index e ls 0 0))))

(define remove-last-helper
  (lambda (ls index)
    (if (= index 1)
	(cdr ls)
	(cons (car ls) (remove-last-helper (cdr ls) (- index 1))))))

(define get-index
  (lambda (e ls n accu)
    (cond [(null? ls) accu]
	  [(eq? (car ls) e) (get-index e (cdr ls) (+ n 1) (+ n 1))]
	  [else (get-index e (cdr ls) (+ n 1) accu)])))

;;; Solution 3
;(define remove-last
;  (lambda (e ls)
;    (if (null? ls)
;	'()
;	(cdr (remove-last-helper e ls)))))

;(define remove-last-helper
;  (lambda (e ls)
;    (if (null? ls)
;	'(#f)
;	(let ([temp (remove-last-helper e (cdr ls))])
;	  (if (eq? (car ls) e)
;	      (if (car temp)
;		  (cons #t (cons (car ls) (cdr temp)))
;		  (cons #t (cdr temp)))
;	      (cons (car temp) (cons (car ls) (cdr temp))))))))

;;Problem 6
(define interval-contains?
  (lambda (intvl n)
    (if (>= n (car intvl))
        (if (<= n (cadr intvl))
            #t
            #f)
        #f)))

;;Problem 7
(define interval-intersects?
  (lambda (i1 i2)
    (cond
      [(and (<= (car i1) (car (cdr i2))) (>= (car i1) (car i2))) #t]
      [(and (<= (car (cdr i1)) (car (cdr i2))) (>= (car (cdr i1)) (car i2))) #t]
      [(and (<= (car i2) (car (cdr i1))) (>= (car i2) (car i1))) #t]
      [(and (<= (car (cdr i2)) (car (cdr i1))) (>= (car (cdr i2)) (car i1))) #t]
      [else #f]
      )))