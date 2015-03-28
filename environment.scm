
;; An environment is a list of association lists. The symbol/value pairs for
;; the inner-most lexical environment are contained in the head of the
;; list; the symbol/values for the next enclosing lexical environment are at
;; the next position in the list, and so on and so forth.

(define (inner-env env)
  (car env))

(define (outer-env env)
  (cdr env))

(define (lookup sym env)
  (if (null? env)
      (values '() #f)
      (let ([p (assoc sym (inner-env env))])
        (if (pair? p)
            (values (cdr p) #t)
            (lookup sym (outer-env env))))))

(define (add-entry sym val env)
  (cons (cons (cons sym val)
              (inner-env env))
        (outer-env env)))

(define (nest-env env)
  (cons '() env))
