;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Amish_lang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;Global Variables
(define listOfOperators '(+ - * / %))
(define listOfArithmeticBooleanOperators '(< > <= >= == !=))
(define listOfLogicalBooleanOperators '(&& ||)) ;Not added to language yet
(define listOfReservedWords '(if let while))

;generic helper functions
;returns true if val is found in lst
(define contains
  (lambda (val lst)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) val) #t)
      (else (contains val (cdr lst))))))


;Functions related to the variable environment
(define empty-env
  (lambda () '()))

(define empty-scope
  (lambda () '()))

(define extend-scope
  (lambda (var val scope)
    (cons (list var val) scope)))

(define extend-env
  (lambda (scope env)
    (cons scope env)))

(define apply-scope
  (lambda (var scope)
    (cond
      ((null? scope) #f)
      ((eq? (caar scope) var) (cadar scope))
      (else (apply-scope var (cdr scope))))))

(define apply-env
  (lambda (var env)
    (cond
      ((null? env) #f)
      (else (let ((resolved (apply-scope var (car env))))
        (if (eq? resolved #f)
            (apply-env var (cdr env))
            resolved))))))

(define extend-env-4-lambda-helper
  (lambda (lovars lovals scope)
    (cond
      ((not (null? lovars)) (extend-env-4-lambda-helper
                             (cdr lovars)
                             (cdr lovals)
                             (extend-scope (car lovars) (car lovals) scope)))
      (else scope))))

(define extend-env-4-lambda
  (lambda (lovars lovals env)
    (extend-env
     (extend-env-4-lambda-helper lovars lovals (empty-scope))
     env)))


(define extend-env-4-let
  (lambda (loexp env)
    (if (null? loexp)
        env
        (extend-env-4-let (cdr loexp)
              (extend-env-4-lambda
               (list (cadr (car (cdr (car loexp)))))
               (list
                (if (eq? (caadr (cdr (car loexp))) 'lambda-exp)
                    (cadr (cdr (car loexp)))
                    ((eval-exp (cadr (cdr (car loexp))) env))))
               env)))))


;Constructors related to the LCE types
(define lit-exp
  (lambda (lit) lit))

(define var-exp
  (lambda (id) id))

(define lambda-exp
  (lambda (params body)
    (list 'lambda params body)))

(define app-exp
  (lambda (rator rands)
    (append (list rator) rands)))


;return true if the given symbol is a reserved op and false otherwise

;Parser Helper Functions
;returns true is s is an operator and false otherwise
(define op?
  (lambda (s)
    (contains s listOfOperators)))

;returns true is s is an arithmetic boolean operator and false otherwise
(define boolean-arithmetic-op?
  (lambda (s)
    (contains s listOfArithmeticBooleanOperators)))

;returns true is s is a reserved word and false otherwise
(define reservedWord?
  (lambda (s)
    (contains s listOfReservedWords)))

;Core Parser Functions
(define parse-exp
  (lambda (lcExp)
    (cond
      ((boolean? lcExp) (list 'bool-exp lcExp))
      ((number? lcExp) (list 'lit-exp lcExp))
      ((symbol? lcExp)
       (cond
         ((op? lcExp) (list 'op-exp lcExp))
         ((boolean-arithmetic-op? lcExp) (list 'bool-arith-op-exp lcExp))
         ((reservedWord? lcExp)
          (cond
            ((eq? lcExp 'if) (list 'if-exp))
            ((eq? lcExp 'let) (list 'let-exp))
            ((eq? lcExp 'while) (list 'while-exp))))
         (else (list 'var-exp lcExp))))
      ((eq? (car lcExp) 'lambda)
       (list 'lambda-exp
             (cadr lcExp)
             (parse-exp (caddr lcExp))))
      (else (cons 'app-exp (append (list (parse-exp (car lcExp))) (map parse-exp (cdr lcExp))))))))

;evaluates an app expression whose car is a arithmetic boolean operator
(define eval-bool-arith-op-exp
  (lambda (appExp env)
    (let ((op1 (eval-exp (cadr appExp) env))
          (op2 (eval-exp (caddr appExp) env))
          (theOp (cadar appExp)))
      (cond
        ((eq? theOp '<) (< op1 op2))
        ((eq? theOp '<=) (<= op1 op2))
        ((eq? theOp '>) (> op1 op2))
        ((eq? theOp '>=) (>= op1 op2))
        ((eq? theOp '==) (= op1 op2))
        ((eq? theOp '!=) (not (= op1 op2)))))))

;evaluates an app expression whose car is an operator
(define eval-op-exp
  (lambda (appExp env)
    (let ((op1 (eval-exp (cadr appExp) env))
          (op2 (eval-exp (caddr appExp) env))
          (theOp (cadar appExp)))
      (cond
        ((eq? theOp '+) (+ op1 op2))
        ((eq? theOp '-) (- op1 op2))
        ((eq? theOp '*) (* op1 op2))
        ((eq? theOp '/) (/ op1 op2))
        ((eq? theOp '%) (modulo op1 op2))
        (else #f)))))

;evaluates an app expression whose car is an if-exp
(define eval-if-exp
  (lambda (appExp env)
    (let ((boolExp (eval-exp (car appExp) env))
          (trueExp (eval-exp (cadr appExp) env))
          (falseExp(caddr appExp)))
    (if boolExp trueExp (eval-exp falseExp env)))))
    
(define eval-exp
  (lambda (lce env)
    (cond
      ((eq? (car lce) 'bool-exp) (cadr lce))
      ((eq? (car lce) 'lit-exp) (cadr lce))
      ((eq? (car lce) 'var-exp) (apply-env (cadr lce) env))
      ((eq? (car lce) 'lambda-exp) (eval-exp (caddr lce) env))
      (else
       (cond
         ((eq? (list-ref (list-ref lce 1) 0) 'lambda-exp)
           ;first element of app-exp is a lambda
           (eval-exp (list-ref (list-ref lce 1) 2)
                     (extend-env-4-lambda
                      (list-ref (list-ref lce 1) 1)
                      (map (lambda (x)
                             (if (eq? (car x) 'lambda-exp)
                                 x
                                 (eval-exp x env))) (cddr lce)) env)))
         ((eq? (list-ref (list-ref lce 1) 0) 'op-exp)
          ;first element of app-exp is a op-exp
          (eval-op-exp (cdr lce) env))
         ((eq? (list-ref (list-ref lce 1) 0) 'bool-arith-op-exp)
          ;first element of app-exp is a bool-arith-op-exp
          (eval-bool-arith-op-exp (cdr lce) env))
         ((eq? (list-ref (list-ref lce 1) 0) 'if-exp)
          ;first element of app-exp is an if-exp
          (eval-if-exp (cddr lce) env))
         ((eq? (list-ref (list-ref lce 1) 0) 'let-exp)
          ;first element of app-exp is an let-exp
          (eval-exp (list-ref lce 3) (extend-env-4-let (cdr (list-ref lce 2)) env)))
         (else
          ;first element of app-exp is a var-exp
           (let ((theLambda (eval-exp (list-ref lce 1) env))
                 (theInputs (map (lambda (x)
                             (if (eq? (car x) 'lambda-exp)
                                 x
                                 (eval-exp x env))) (cddr lce))))
             (eval-exp theLambda (extend-env-4-lambda (list-ref theLambda 1)
                                                      theInputs
                                                      env)))))))))



(define run-program
  (lambda (lce)
    (eval-exp lce (empty-env))))


;(define anExp '(let ((a 5) (b 7)) (+ a (let ((c 3) (b (- b 2))) (+ b c)))))
;(define anExp '(let ((a 5) (b 7)) (+ a (let ((c 3) (b (* c 2))) (+ b c))))) ;should be 14 when working
;(define anExp '(let ((a 5) (b 4)) (+ a b)))
;(define anExp '(let ((a (lambda (x) (* x 2))) (b (lambda (x y) (+ x y)))) (b (a 5) (a 7))))
(define anExp '(let ((fact (lambda (x) (if (== x 1) 1 (* x (fact (- x 1))))))) (fact 4))) 
;(define anExp '((lambda (a b) (a b)) (lambda (x) (* x 2)) 5))

;Pass the above to apply-env to make sure it comes out


;(test val (empty-env))

;(list (cadr (car (cdr (car lst)))))))) list of vars
;(cadr (cdr (car lst))) list of vals
;(parse-exp anExp)

(run-program (parse-exp anExp))


;(map (lambda (lst) (list-ref (list-ref lst 1) 1)) val)