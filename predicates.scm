
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (non-empty-list? expr)
  (and (list? expr)
       (not (null? expr))))

(define (arithmetic? expr)
  (and (non-empty-list? expr)
       (member (car expr) '(+ - * /))))
