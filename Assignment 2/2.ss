;;Dharmin Shah 2

;;Problem 1
(define left-most
  (lambda (ls)
    (if (atom? (car ls))
        (car ls)
        (left-most (car ls)))))

;;Problem 2
(define right-most
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (car ls)) (right-most (cdr ls))]
      [(atom? (car ls)) (if (null? (cdr ls))
                            (car ls)
                            (right-most (cdr ls)))]
      [(null? (cdr ls)) (right-most (car ls))]
      [else (right-most (cdr ls))])))

;;Problem 3
(define remove-left-most
  (lambda (n ls)
    (cond
      [(null? ls) '()]
      [(atom? (car ls)) (if (equal? n (car ls))
                              (cdr ls)
                              (cons (car ls) (remove-left-most n (cdr ls))))]
      [(equal? (car ls) n) (cdr ls)]
      [(contains-helper (car ls) '() n) 
       (if (equal? (car ls) n)
           (cdr ls)
           (cons (remove-left-most n (car ls)) (cdr ls)))]
      [else (if (equal? (cdr ls) n)
                (car ls)
                (cons (car ls) (remove-left-most n (cdr ls))))])))

(define contains-helper
  (lambda (ls rest accu)
    (cond
      [(null? ls)  #f]
      [(null? (car ls)) (contains-helper (cdr ls) rest accu)]
      [(atom? (car ls))
       (if (equal? (car ls) accu)
           #t
           (if (null? (cdr ls))
               (contains-helper rest '() accu)
               (contains-helper (cdr ls) rest accu)))]
      [(null? (cdr ls)) 
       (if (equal? (car ls) accu) 
           #t
           (contains-helper (car ls) rest accu))]
      [else (if (equal? (car ls) accu) 
                #t
                (contains-helper (car ls) (cons (cdr ls) rest) accu))])))
      

;;Problem 4
(define remove-right-most
  (lambda (n ls)
    (cond
      [(null? ls) '()]
      [(atom? (car ls)) (if (equal? n (car ls))
                              (cdr ls)
                              (cons (car ls) (remove-right-most n (cdr ls))))]
      [(contains-helper (cdr ls) '() n) 
       (if (equal? (cdr ls) n)
           (car ls)
           (cons (car ls) (remove-right-most n (cdr ls))))]
      [else (if (equal? (car ls) n)
                (cdr ls)
                (cons (remove-right-most n (car ls)) (cdr ls)))])))

;;Problem 5
(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (append (reverse (cdr ls)) 
              (list (if (atom? (car ls)) (car ls)
                        (reverse (car ls)))))])))
       

;;Problem 6
(define largest
  (lambda (ls)
    (if (null? ls) 
        #f
        (largest-helper ls '() 0))))

(define largest-helper
  (lambda (ls rest accu)
    (cond
      [(null? ls)  accu]
      [(null? (car ls)) (largest-helper (cdr ls) rest accu)]
      [(atom? (car ls))
       (if (> (car ls) accu)
           (if (null? (cdr ls))
               (largest-helper rest '() (car ls))
               (largest-helper (cdr ls) rest (car ls)))
           (if (null? (cdr ls))
               (largest-helper rest '() accu)
               (largest-helper (cdr ls) rest accu)))]
      [else (largest-helper (car ls) (cons (cdr ls) rest) accu)])))

;;Problem 7
(define interval-union
  (lambda (i1 i2)
    (cond
      [(wraps i1 i2) (list i1)]
      [(wraps i2 i1) (list i2)]
      [(int-left i1 i2) (list (list (car i1) (cadr i2)))]
      [(int-left i2 i1) (list (list (car i2) (cadr i1)))]
      [else (list i1 i2)])))

(define wraps
  (lambda (i1 i2)
    (and
      (if (>= (car i2) (car i1))
           (if (<= (car i2) (cadr i1))
               #t
               #f)
          #f)
      (if (>= (cadr i2) (car i1))
          (if (<= (cadr i2) (cadr i1))
              #t
              #f)
          #f))))

(define int-left
  (lambda (i1 i2)
    (if (<= (car i1) (car i2))
        (and (>= (cadr i1) (car i2)) (<= (cadr i1) (cadr i2)))
        #f)))

    

;;Problem 8
(define intersection
  (lambda (s1 s2)
    (cond
      [(null? s1) '()]
      [(null? s2) '()]
      [else (intersection-helper s1 s2 '())])))


(define intersection-helper
  (lambda (s1 s2 ilist)
    (cond
      [(null? s1) ilist]
      [(contains (car s1) s2) (intersection-helper (cdr s1) s2 (append ilist (list (car s1))))]
      [else (intersection-helper (cdr s1) s2 ilist)])))

(define contains
  (lambda (el ls)
    (cond
      [(null? ls) #f]
      [(equal? el (car ls)) #t]
      [else (contains el (cdr ls))])))

;;Problem 9
;;Constructor
(define empty-tree
  (lambda () '()))

;;insert in increasing order
(define insert-inc
  (lambda (el ls)
    (tree-insert el ls >)))

;;insert in decreasing order
(define insert-dec
  (lambda (el ls)
    (tree-insert el ls <)))

;;generic insert that takes in a parameter to distinguis between inc and dec
(define tree-insert
  (lambda (el ls proc)
    (cond
      [(null? ls) (list el '() '())]
      [(null? (car ls)) (list el '() '())]
      [(null? (cdr ls)) (insert-inc el (car ls))]
      [(equal? (car ls) el) ls]
      [(proc el (car ls)) (list (car ls) (cadr ls) 
                            (if (equal? proc >)
                                (insert-inc el (cddr ls))
                                (insert-dec el (cddr ls))))]
      [else (list (car ls) (if (equal? proc >)
                               (insert-inc el (cadr  ls))
                               (insert-dec el (cadr ls))) (caddr ls))])))

;;remove from inc binary tree
(define remove-inc
  (lambda (el ls)
    (tree-remove el ls >)))

;;remove from dec binary tree
(define remove-dec
  (lambda (el ls)
    (tree-remove el ls <)))

;;generic remove that takes in a parameter to differentiate between inc and dec
(define tree-remove
  (lambda (el ls proc)
    (cond
      [(null? ls) ls]
      [(equal? el (car ls))
       (cond
         [(leaf? ls) '()]
         [(right-only? ls) (caddr ls)]
         [else (join-trees (caddr ls) (cadr ls))])]
      [(proc el (car ls)) (list (car ls) (cadr ls) (if (equal? proc >)
                                                       (remove-inc el (caddr ls))
                                                       (remove-dec el (caddr ls))))]
      [else (list (car ls) (if (equal? proc >)
                               (remove-inc el (cadr ls))
                               (remove-dec el (cadr ls)))(caddr ls))])))

(define join-trees
  (lambda (t1 t2)
    (if (null? t2)
        t1
        (cond
          [(or (left-only? t2) (leaf? t2)) (list (car t2) (cadr t2) (join-trees t1 (caddr t2)))]
          [else (list (right-most-node t2) (join-trees-helper t2) t1)]))))

(define join-trees-helper
  (lambda (ls)
    (cond
      [(null? (caddr (caddr ls))) (list (car ls) (cadr ls) (cadr (caddr ls)))]
      [else (list (car ls) (cadr ls) (join-trees-helper (caddr ls)))])))

(define right-most-node
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (caddr ls)) (car ls)]
      [else (right-most-node (caddr ls))])))
        
(define right-only?
  (lambda (ls)
    (if (and (null? (cadr ls)) (not (null? (caddr ls))))
        #t
        #f)))

(define left-only?
  (lambda (ls)
    (if (and (null? (caddr ls)) (not (null? (cadr ls))))
        #t
        #f)))
        
(define leaf?
  (lambda (ls)
    (and
      (null? (cadr ls))
      (null? (caddr ls)))))