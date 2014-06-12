(define (case-1) (eval-one-exp '(letrec ([c 1]
         [d (* c 4)])
  (list c d))))

(define (case-2) (eval-one-exp '(letrec ([c 1]
         [d (let ([e c]) e)])
  (list c d))))

(define (case-3) (eval-one-exp '(letrec ([c 1]
         [d (letrec ([e c]) e)])
  (list c d))))

(define (case-4) (eval-one-exp '(letrec ([c 1]
         [d (letrec ([c c]) c)])
  (list c d))))

(define (case-5) (eval-one-exp '(letrec ([c c]) c)))

(define (case-6) (eval-one-exp '(letrec ([c 1]
         [d (letrec ([e 1]) c)])
  (list c d))))

(define (case-7) (eval-one-exp '(letrec ([c 1]
         [d (letrec ([c 42]) c)])
  (list c d))))