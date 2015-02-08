
(use-modules (ice-9 match))

(define (Value? M)
  (match M
    [`(quote ,_) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [(? string?) #t]
    [(? char?) #t]
    [(? symbol?) #t]
    [(or '+ '- '* '/ '=) #t]
    [else #f]))

(define (normalize-term M)
  (normalize M (lambda(x) x)))

(define (normalize M k)
  (match M
    [`(lambda ,params ,body)
     (k `(lambda ,params ,(normalize-term body)))]
    [`(let ([,x ,M1]) ,M2)
     (normalize M1 (lambda (N1)
                     `(let ([,x ,N1])
                        ,(normalize M2 k))))]
    [`(if ,M1 ,M2 ,M3)
     (normalize-name M1 (lambda(t)
                          (k `(if ,t
                                  ,(normalize-term M2)
                                  ,(normalize-term M3)))))]
    [`(,F . ,M*)
     (normalize-name F (lambda (t)
                         (normalize-name* M* (lambda (t*)
                                               (k `(,t . ,t*))))))]
    [(? Value?)
     (k M)]))

(define (normalize-name M k)
  (normalize M (lambda (N)
                 (if (Value? N)
                     (k N)
                     (let ([t (gensym)])
                       `(let ([,t ,N])
                          ,(k t)))))))

(define (normalize-name* M* k)
  (if (null? M*)
      (k '())
      (normalize-name (car M*)
                      (lambda (t)
                        (normalize-name* (cdr M*)
                                         (lambda (t*)
                                           (k `(,t . ,t*))))))))
