
(use-modules (ice-9 match))

(define (lookup sym sym-table)
  (let ([pair (assoc sym sym-table)])
    (if pair (cadr pair) pair)))

(define (op->instruction op)
  (match op
    ['+ 'add]
    ['* 'mul]
    ['- 'sub]
    ['/ 'sdiv]))

(define (literal? obj)
  (or (number? obj)
      (char? obj)
      (string? obj)
      (boolean? obj)))

(define (repr obj env)
  (cond
   [(symbol? obj) (lookup obj env)]
   [(literal? obj) obj]
   [else (throw 'object-not-representable)]))


(define (compile-binary-expr expr env)
  (let ([op (car expr)]
        [args (map (lambda (sym) (repr sym env)) (cdr expr))])
    `(,(op->instruction op) ,@args)))

;(define (compile-let-expr expr)
