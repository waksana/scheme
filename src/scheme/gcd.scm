(
 (assign env (quote ()))
 (assign exp (quote 
               (((
                 (lambda (fun)
                   ((lambda (f) (f f))
                    (lambda (f) (fun (lambda (x) ((f f) x))))))
                 (lambda (gcd)
                   (lambda (a)
                     (lambda (b) (
                                  cond
                                  ((= b 0) a)
                                  (#t ((gcd b) (% a b)))
                                  ))
                     )))
                15) 6)
               )))
