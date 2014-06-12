(load "chez-init.ss")
(load "parser.ss")
(load "environment.ss")
(load "interpreter.ss")

(define (rl) (load "interpreter.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([interpret (read)])
      (if (not (equal? interpret '(exit)))
      (begin (write (eval-one-exp interpret)) (newline) (rep))))))

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
                         | (letrec ([<variable> <expression>]*) <expression><expression>*)            ||
                         | (begin <expression> <expression>*)                                         ||
                         | (set! <variable> <expression>)                                             ||
                         | <application>                                                              ||
                                                                                                      ||
<constant>             ::= <boolean>|<number>|<character>|<string>|<vector>                           ||
<formals>              ::= <variable>                                                                 ||
                         | (<variable>*)                                                              ||
                         | (<variable> <variable>* . <variable>)                                      ||
<application>          ::= (<expression> <expression>*)                                               ||
<vector>               ::= #(<expression>*)                                                           ||
<variable>             ::= any Scheme identifier                                                      ||
<datum>                ::= any Scheme object                                                          ||
                                                                                                      ||
======================================================================================================|#