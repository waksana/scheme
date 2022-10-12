(
    (assign env (quote ()))
    (assign exp (quote 
        ((
            (lambda (fun)
                ((lambda (f) (f f))
                (lambda (f) (fun (lambda (x) ((f f) x))))))
            (lambda (fib)
                (lambda (n)
                (cond
                    ((= n 0) 0)
                    ((= n 1) 1)
                    (#t (+ (fib (- n 1)) (fib (- n 2))))))))
        10)
    ))
)
