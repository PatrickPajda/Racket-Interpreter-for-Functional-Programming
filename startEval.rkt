#lang racket/base

(define (zip list1 list2)
  (cond
    [(or (null? list1) (null? list2)) '()]  ; If either list is empty, stop
    [else (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2)))]))


(define (lookup var env)
  (cond
    [(null? env) (error "Variable not found" var)]
    [(and (equal? (car (car env)) var) (not (eq? (cdar env) 'placeholder))) (cdr (car env))]
    [else (lookup var (cdr env))]))


(define (startEval expr env)
  (cond    
    ;; Handle `letrec` expressions
    [(and (list? expr) (eq? (car expr) 'letrec))
     (let* ([bindings (cadr expr)]  ; Extract the bindings list
            [body (caddr expr)]     ; Extract the body of the `letrec`
            ;; Step 1: Create a placeholder environment for all bindings
            [placeholder-env (append (map (lambda (binding)
                                             (cons (car binding) 'placeholder))
                                           bindings)
                                      env)])
       ;; Step 2: Evaluate all bindings using the placeholder environment
       (let ([extended-env (map (lambda (binding)
                                   (cons (car binding)
                                         (startEval (cadr binding) placeholder-env)))
                                 bindings)])
         ;; Step 3: Replace placeholders with actual bindings and evaluate the body
         (startEval body (append extended-env env))))]

    ;; Handle `let` expressions
    [(and (list? expr) (eq? (car expr) 'let))
     (let* ([bindings (cadr expr)]  ; Extract the bindings list
            [body (caddr expr)]     ; Extract the body of the `let`
            [extended-env (append (map (lambda (binding)
                                         (cons (car binding) (startEval (cadr binding) env)))
                                       bindings)
                                  env)])  ; Extend the environment
       (startEval body extended-env))]  ; Evaluate the body in the new environment

    ;; Handle conditional expressions
    [(and (list? expr) (eq? (car expr) 'if))
     (let ([condition (startEval (cadr expr) env)])  ; Evaluate the condition
       (if condition
           (startEval (caddr expr) env)  ; Evaluate the then-clause
           (startEval (cadddr expr) env)))]  ; Evaluate the else-clause

    ;; Handle lambda expressions
    [(and (list? expr) (eq? (car expr) 'lambda))
     (list 'closure (cadr expr) (caddr expr) env)]  ; Return a closure

    ;; Handle arithmetic expressions
    [(and (list? expr) (member (car expr) '(+ - * /)))
     (apply (case (car expr)
              [(+) +]
              [(-) -]
              [(*) *]
              [(/) /])
            (map (lambda (e) (startEval e env)) (cdr expr)))]  ; Recursively evaluate operands

    ;; Handle relational operators
    [(and (list? expr) (member (car expr) '(= < > <= >=)))
     (apply (case (car expr)
              [(=) =]
              [(<) <]
              [(>) >]
              [(<=) <=]
              [(>=) >=])
            (map (lambda (e) (startEval e env)) (cdr expr)))]  ; Recursively evaluate operands

    ;; Handle list operations
    [(and (list? expr) (eq? (car expr) 'cons))
     (cons (startEval (cadr expr) env) (startEval (caddr expr) env))]  ; Combine element and list

    [(and (list? expr) (eq? (car expr) 'car))
     (let ([lst (startEval (cadr expr) env)])  ; Evaluate the list
       (if (list? lst)
           (car lst)
           (error "car: expected a list" lst)))]  ; Error if not a list

    [(and (list? expr) (eq? (car expr) 'cdr))
     (let ([lst (startEval (cadr expr) env)])  ; Evaluate the list
       (if (list? lst)
           (cdr lst)
           (error "cdr: expected a list" lst)))]  ; Error if not a list

    [(and (list? expr) (eq? (car expr) 'null?))
     (null? (startEval (cadr expr) env))]  ; Check if the list is empty

    ;; Handle numbers
    [(number? expr) expr]

    ;; Handle Booleans
    [(boolean? expr) expr]

    ;; Handle quoted expressions
    [(and (list? expr)
          (>= (length expr) 2)
          (eq? (car expr) 'quote))
     (cadr expr)]

    ;; Handle function application
    [(list? expr)
     (let* ([closure (startEval (car expr) env)]  ; Evaluate the function
            [args (map (lambda (e) (startEval e env)) (cdr expr))])  ; Evaluate arguments
       (if (and (list? closure) (eq? (car closure) 'closure))
           (let* ([params (cadr closure)]
                  [body (caddr closure)]
                  [closure-env (cadddr closure)]
                  ;; Howard: don't forget to also append "env" which has the recursive definition
                  [new-env (append (append (zip params args) closure-env) env)])  ; Bind arguments to parameters
             (startEval body new-env))  ; Evaluate the body in the new environment
           (error "Not a valid function" closure expr)))]  ; Error if not a closure

    ;; Handle symbols via lookup in the environment
    [(and (symbol? expr) (not (eq? expr 'placeholder))) (lookup expr env)]

    [(and (symbol? expr) (eq? expr 'placeholder)) expr]
    
    ;; Handle unsupported cases
    [else (error "Unsupported expression" expr)]))





(startEval '(letrec ((factorial (lambda (n)
                                  (if (= n 0) 1 (* n (factorial (- n 1)))))))
              (factorial 10)) '())
; Expected: 120



(startEval '(let ((add (lambda (x y) (+ x y))))
              (let ((a 2) (b 3))
(+ a b))) '())




(startEval '(let ((x 1) (y 2)) (+ x y)) '())
; Expected: 3

(startEval (list 'let (list (list 'x 1) (list 'y 2)) (list '+ 'x 'y)) '())

(startEval (list 'let (list (list 'x 1))
                 (list 'let (list (list 'y 2))
                       (list '+ 'x 'y))) '())






(startEval '(cons 1 '(2 3)) '()) ; Expected: '(1 2 3)
(startEval '(car '(1 2 3)) '()) ; Expected: 1
(startEval '(cdr '(1 2 3)) '()) ; Expected: '(2 3)
(startEval '(null? '()) '()) ; Expected: #t
(startEval '(null? '(1 2 3)) '()) ; Expected: #f
(startEval '(cons x '(2 3)) '((x . 1))) ; Expected: '(1 2 3)
(startEval '(car (cons x '(2 3))) '((x . 1))) ; Expected: 1
(startEval '(cdr (cons x '(2 3))) '((x . 1))) ; Expected: '(2 3)
;(startEval '(car 1) '()) ; Expected: Error ("car: expected a list")
;(startEval '(cdr 1) '()) ; Expected: Error ("cdr: expected a list")
(startEval '(null? 1) '()) ; Expected: #f

; Expected: '((x . 1) (y . 2))

(zip '(a b c) '(10 20 30))
; Expected: '((a . 10) (b . 20) (c . 30))

(zip '(x y) '())
; Expected: '()


(startEval '((lambda (x) (+ x 1)) 5) '()) 
; Expected: 6

(startEval '((lambda (x y) (+ x y)) 3 4) '()) 
; Expected: 7

(startEval '((lambda (x) ((lambda (y) (+ x y)) 2)) 3) '()) 
; Expected: 5


(startEval '((lambda (x) (+ x y)) 3) '((y . 7))) 
; Expected: 10

;(startEval '((lambda (x) (+ x y)) 3) '()) 
; Expected: Error ("Variable not found: y")
;(startEval '(5 3) '()) 
; Expected: Error ("Not a valid function")





(startEval '(lambda (x) (+ x 1)) '()) 
; Expected: '(closure (x) (+ x 1) '())

(startEval '(lambda (x) (lambda (y) (+ x y))) '()) 
; Expected: '(closure (x) (lambda (y) (+ x y)) '())



(startEval '(if #t 42 0) '()) ; Expected: 42
(startEval '(if #f 42 0) '()) ; Expected: 0

(startEval '(if (= 1 1) 42 0) '()) ; Expected: 42
(startEval '(if (< 5 3) 42 0) '()) ; Expected: 0

(startEval '(if (= x 1) 42 0) '((x . 1))) ; Expected: 42
(startEval '(if (> x y) x y) '((x . 5) (y . 10))) ; Expected: 10


;(startEval '(if #t 42) '())       ; Expected: Error (missing else-clause)
;(startEval '(if (= x 1) 42 0) '()) ; Expected: Error ("Variable not found: x")





(startEval '(+ 1 2) '())       ; Expected: 3
(startEval '(- 5 3) '())       ; Expected: 2
(startEval '(* 2 3) '())       ; Expected: 6
(startEval '(/ 10 2) '())      ; Expected: 5

(startEval '(+ x 3) '((x . 7))) ; Expected: 10
(startEval '(* x y) '((x . 2) (y . 4))) ; Expected: 8





(startEval 42 '()) ; Expected: 42
(startEval -5 '()) ; Expected: -5


(startEval 'x '((x . 42))) ; Expected: 42
(startEval 'y '((x . 42) (y . 10))) ; Expected: 10
;(startEval '(+ 1) '())          ; Expected: Error (not enough operands)
;(startEval '(/ 1 0) '())        ; Expected: Error (division by zero)
;(startEval '(+ x z) '((x . 5))) ; Expected: Error ("Variable not found: z")



;(startEval 'z '((x . 42))) ; Expected: Error ("Variable not found: z")

(startEval '(quote x) '()) ; Expected: x
(startEval '(quote (1 2 3)) '()) ; Expected: (1 2 3)



(startEval '(letrec ((x 1)) x) '())
; Expected: 1


