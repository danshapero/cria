
(use-modules (ice-9 match))

(define (atomic? exp)
  (match exp
    [`(quote ,_)         #t]
    [(? number?)         #t]
    [(? boolean?)        #t]
    [(? string?)         #t]
    [(? char?)           #t]
    [(? symbol?)         #t]
    [(or '+ '- '* '/ '=) #t]
    [else                #f]))

(define (normalize-term exp)
  (normalize exp (lambda (x) x)))

(define (normalize exp k)
  (match exp
    [`(lambda ,params ,body)
     (k `(lambda ,params ,(normalize-term body)))]

    [`(let () ,exp)
     (normalize exp k)]

    [`(let ([,x ,exp1] . ,clause) ,exp2)
     (normalize exp1 (lambda (aexp1)
                       `(let ([,x ,aexp1])
                          ,(normalize `(let (,@clause) ,exp2) k))))]

    [`(if ,exp1 ,exp2 ,exp3)
     (normalize-name exp1 (lambda (t)
                            (k `(if ,t
                                    ,(normalize-term exp2)
                                    ,(normalize-term exp3)))))]

    [`(set! ,v ,exp)
     (normalize-name exp (lambda (t)
                           `(let ([,(gensym '_) (set! ,v ,t)])
                              ,(k '(void)))))]

    [`(,f . ,e*)
     (normalize-name f (lambda (t)
                         (normalize-name* e* (lambda (t*)
                                               (k `(,t . ,t*))))))]

    [(? atomic?)
     (k exp)]))

(define (normalize-name exp k)
  (normalize exp (lambda (aexp)
                   (if (atomic? aexp)
                       (k aexp)
                       (let ([t (gensym)])
                         `(let ([,t ,aexp]) ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (lambda (t)
                                   (normalize-name* (cdr exp*)
                                                    (lambda (t*)
                                                      (k `(,t . ,t*))))))))


;; Top-level normalization:
(define (normalize-define def)
  (match def
    [`(define (,f . ,params) ,body)
     `(define ,f ,(normalize-term `(lambda ,params ,body)))]

    [`(define ,v ,exp)
     `(begin ,@(flatten-top (normalize-term exp) v))]))


(define (flatten-top exp v)
  (match exp
    [`(let ([,x ,cexp]) ,exp)
     (cons `(define ,x ,cexp)
           (flatten-top exp v))]

    [else
     `((define ,v ,exp))]))


(define (normalize-program decs)
  (match decs
    ['()
     '()]

    [(cons `(define . ,_) rest)
     (cons (normalize-define (car decs))
           (normalize-program rest))]

    [(cons exp rest)
     (cons (normalize-term exp)
           (normalize-program rest))]))
