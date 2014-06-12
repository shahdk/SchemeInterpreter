;;Dharmin Shah 5

;;Problem 1
(define connected?
  (lambda (ls)
    (let ([vertices (get-vertex ls)])
      (cond
        [(null? vertices) #t]
        [(null? (cdr vertices)) #t]
        [else (if (= (count-elements vertices) (count-elements (bfs (get-edges-for-vertex (car vertices) ls) (list (car vertices)) ls)))
                  #t
                  #f)]))))

(define count-elements
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(null? (car ls)) 0]
      [(atom? (car ls)) (+ 1 (count-elements (cdr ls)))]
      [else (+ (count-elements (car ls)) (count-elements (cdr ls)))])))

(define bfs
  (lambda (edges visited-edges ls)
    (cond
      [(null? edges) visited-edges]
      [else (begin (if (not (contain-all? edges visited-edges)) (set! visited-edges (insert-into visited-edges edges)))
              (bfs (get-all-edges edges ls visited-edges '()) visited-edges ls))])))

(define insert-into
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [else (begin (if (not (contains? (car ls2) ls1)) 
                       (set! ls1 (append ls1 (list (car ls2))))) (insert-into ls1 (cdr ls2)))])))

(define get-all-edges
  (lambda (edges ls visited accu)
    (cond
      [(null? edges) (remove-visited accu visited)]
      [else (get-all-edges (cdr edges) ls visited (append (get-edges-for-vertex (car edges) ls) accu))])))

(define remove-visited
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(contains? (car ls1) ls2) (append '() (remove-visited (cdr ls1) ls2))]
      [else (append (list (car ls1)) (remove-visited (cdr ls1) ls2))])))

(define contain-all?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(contains? (car s1) s2) (contain-all? (cdr s1) s2)]
      [else #f])))

(define contains?
  (lambda (el ls)
    (cond
      [(null? ls) #f]
      [(equal? el (car ls)) #t]
      [else (contains? el (cdr ls))])))      

(define get-vertex
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (append (list (caar ls)) (get-vertex (cdr ls)))])))

(define get-edges-for-vertex
  (lambda (vertex ls)
    (cond
      [(null? ls) '()]
      [(equal? (caar ls) vertex) (cadar ls)]
      [else (get-edges-for-vertex vertex (cdr ls))])))

;;Problem 2
(define make-slist-leaf-iterator
  (lambda (ls)
    (let* ([stack (make-stack)]
          [temp (stack 'push ls)])
      (lambda ()
        (if stack
            (let loop ([node (stack 'pop)])
              (if (pair? node)
                  (begin (stack 'push (cdr node)) (loop (car node)))
                  (if (null? node)
                      (loop (stack 'pop))
                      node)))
            '())))))

(define make-stack
    (lambda ()
      (let ([stack '()])
        (lambda (proc . args)
          (case proc
            [(empty?) (null? stack)]
            [(push) (set! stack (cons (car args) stack))]
            [(pop) (if (null? stack)
                       #f
                      (let ([top (car stack)])
                       (set! stack (cdr stack))
                       top))]
            [(peek) (if (null? stack)
                        #f
                        (car stack))]
            [(getstack) (if (null? stack)
                        '()
                        stack)]
            [else (error 'stack "illegal message to stack object: ~a" proc)])))))

;;Problem 3
(define subst-leftmost
  (lambda (new old ls proc)
    (cond
      [(null? ls) '()]
      [(atom? (car ls)) (if (proc old (car ls))
                              (cons new (cdr ls))
                              (cons (car ls) (subst-leftmost new old (cdr ls) proc)))]
      [(pair? old) (if (proc (car ls) old) (cons new (cdr ls)))]
      [(contains-helper (car ls) '() old) 
       (if (pair? old)
           (if (proc (car ls) old)
               (cons new (cdr ls)))
           (cons (subst-leftmost new old (car ls) proc) (cdr ls)))]
      [else (if (pair? old)
                (if (proc (cdr ls) old)
                    (cons (car ls) new))
                (cons (car ls) (subst-leftmost new old (cdr ls) proc)))])))

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