(define build (lambda (x y) (cons x (cons y (quote ())))))
(define new-entry build)
(define first car)
(define second (lambda (l) (car (cdr l))))
(define third (lambda (l) (car (cdr (cdr l)))))
(define look-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
        (look-in-entry-help name (cdr names) (cdr values) entry-f)
      )
    )
  )
)
(define look-in-entry
  (lambda (name entry entry-f)
    (look-in-entry-help name (first entry) (second entry) entry-f)
  )
)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
        (look-in-entry name (car table)
          (lambda (name)
            (lookup-in-table name (cdr table) table-f)
          )
        )
      )
    )
  )
)

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e))
    )
  )
)

(define consts (quote (#t #f cons car cdr null? eq? atom? zero? add1 sub1 number?)))

(define in-lat?
  (lambda (x lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) x) #t)
      (else (in-lat? x (cdr lat)))
    )
  )
)

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((in-lat? e consts) *const)
      (else *identifier)
    )
  )
)

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
        (cond
          ((eq? (car e) (quote quote)) *quote)
          ((eq? (car e) (quote lambda)) *lambda)
          ((eq? (car e) (quote cond)) *cond)
          (else *application)
        )
      )
      (else *application)
    )
  )
)

(define value
  (lambda (e)
    (meaning e (quote ()))
  )
)

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)
  )
)

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e))
    )
  )
)

(define text-of second)
(define *quote
  (lambda (e table)
    (text-of e)
  )
)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)
  )
)

(define initial-table
  (lambda (name)
    (car (quote ()))
  )
)

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
      (cons table (cdr e))
    )
  )
)

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcond
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
        (meaning (answer-of (car lines)) table)
      )
      ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table)
      )
      (else (evcond (cdr lines) table))
    )
  )
)
(define question-of first)
(define answer-of second)
(define else?
  (lambda (e)
    (cond
      ((atom? e) (eq? e (quote else)))
      (else #f)
    )
  )
)

(define *cond
  (lambda (e table)
    (evcond (cdr e) table)
  )
)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
        (cons
          (meaning (car args) table)
          (evlis (cdr args) table)
        )
      )
    )
  )
)

(define *application
  (lambda (e table)
    (apply-s
      (meaning (car e) table)
      (evlis (cdr e) table)
    )
  )
)

(define primitive?
  (lambda (e)
    (eq? (first e) (quote primitive))
  )
)

(define non-primitive?
  (lambda (e)
    (eq? (first e) (quote non-primitive))
  )
)

(define apply-s
  (lambda (fun args)
    (cond
      ((primitive? fun)
        (apply-primitive (second fun) args)
      )
      ((non-primitive? fun)
        (apply-closure (second fun) args)
      )
    )
  )
)

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons)) (cons (first vals) (second vals)))
      ((eq? name (quote car)) (car (first vals)))
      ((eq? name (quote cdr)) (cdr (first vals)))
      ((eq? name (quote null?)) (null? (first vals)))
      ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
      ((eq? name (quote atom?)) (:atom? (first vals)))
      ((eq? name (quote zero?)) (zero? (first vals)))
      ((eq? name (quote add1)) (add1 (first vals)))
      ((eq? name (quote sub1)) (sub1 (first vals)))
      ((eq? name (quote number?)) (number? (first vals)))
    )
  )
)

(define :atom?
  (lambda (val)
    (cond
      ((atom? val) #t)
      ((null? val) #f)
      ((eq? (car x) (quote non-primitive)) #t)
      ((eq? (car x) (quote primitive)) #t)
      (else #f)
    )
  )
)

(define apply-closure
  (lambda (closure args)
    (meaning
      (body-of closure)
      (cons
        (new-entry (formals-of closure) args)
        (table-of closure)
      )
    )
  )
)

(value (quote (add1 8)))

(value
  (quote
    (
      (
        (lambda (fun)
          ((lambda (f) (f f))
           (lambda (f) (fun (lambda (x) ((f f) x))))
          )
        )
        (lambda (s)
          (lambda (n)
            (cond
              ((zero? n) 0)
              (else (add1 (s (sub1 n))))
            )
          )
        )
      )
      42
    )
  )
)
