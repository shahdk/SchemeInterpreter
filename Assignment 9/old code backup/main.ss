(load "chez-init.ss")
(load "parser.ss")
(load "environment.ss")
(load "interpreter.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([foo (read)])
      (if (not (equal? foo '(exit)))
      (begin (write (eval-one-exp foo)) (newline) (rep))))))

#|============================================== BNF ===================================================
                                                                                                      ||
<program>              ::=<form>*                                                                     ||
<form>                 ::=<definition>|<expression>                                                   ||
<definition>           ::=<variable definition>|(begin <definition>*)                                 ||
<variable definition>  ::=(define <variable> <expression>)                                            ||
<expression>           ::= <constant>                                                                 ||
                         | <variable>                                                                 ||
                         | (quote <datum>)                                                            ||
                         | (lambda <formals> <expression> <expression>*)                              ||
                         | (if <expression> <expression> <expression>)                                ||
                         | (let ([<variable> <expression>]*) <expression><expression>*)               ||
                         | (let <variable> ([<variable> <expression>]*) <expression><expression>*)    ||
                         | (let* ([<variable> <expression>]*) <expression><expression>*)              ||
                         | (letrec ([<variable> <expression>]*) <expression> <expression>*)           ||
                         | (begin <expression> <expression>*)                                         ||
                         | (set! <variable> <expression>)                                             ||
                       	 | (while <expression> <expression> <expression>*)			      ||
			 | (cond <clause> <clause>*)						      ||
			 | (case <expression> <clause> <clause>*)				      ||
			 | (and <expression>*)							      ||
			 | (or <expression>*)							      || 
                         | <application>                                                              ||
                                                                                                      ||
<constant>             ::= <boolean>|<number>|<character>|<string>|<vector>                           ||
<formals>              ::= <variable>                                                                 ||
                         | (<variable>*)                                                              ||
                         | (<variable> <variable>* . <variable>)                                      ||
<clause>               ::= (<expression>)						              ||
			 | (<expression> <expression> <expression>*)				      ||
<application>          ::= (<expression> <expression>*)                                               ||
<vector>               ::= #(<expression>*)                                                           ||
<variable>             ::= any Scheme identifier                                                      ||
<datum>                ::= any Scheme object                                                          ||
                                                                                                      ||
======================================================================================================|#

(define-syntax return-first
  (syntax-rules ()
    ((_ expr expr1 ...)
     (let ([first expr])
       (begin expr1 ... first)))))

(define-syntax for
  (syntax-rules (:)
    ((_ (init : test : update) body)
     (begin init 
       (letrec ([loop (lambda ()
                        (if test
                            (begin body update (loop))))])
         (loop))))))
