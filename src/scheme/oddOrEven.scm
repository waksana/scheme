(
 (assign env (quote ()))
 (assign exp (quote 

(((lambda (even odd) (
                      (
                       (lambda (f) (f f))
                       (lambda (self)
                         (lambda (fn) (fn
                                        (lambda (x) (((self self) even) x))
                                        (lambda (x) (((self self) odd) x))))))
                      even))
  (lambda (even odd) (lambda (n) (cond ((= n 0) #t) ((= n 1) #f) (#t (odd (- n 1))))))
  (lambda (even odd) (lambda (n) (cond ((= n 1) #t) ((= n 0) #t) (#t (even (- n 1)))))))
 98)

)))
