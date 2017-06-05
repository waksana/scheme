(define Y
  (lambda (fun)
    ((lambda (f) (f f))
     (lambda (f) (fun (lambda (x) ((f f) x))))
    )
  )
)


(define id
  (Y
    (lambda (s)
      (lambda (n)
        (cond
          ((zero? n) 0)
          (else (add1 (s (sub1 n))))
        )
      )
    )
  )
)


(define id2
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (add1 (id2 (sub1 1))))
    )
  )
)

(id 9)
(id2 10)
(quote (1 2 3 4 5 6 7 8 9 10))
