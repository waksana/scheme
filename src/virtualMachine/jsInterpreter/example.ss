(define gcd (lambda (a b)
        (cond
            ((zero? b) a)
            (else (gcd b (mod a b))))))
(gcd 6 15)
(define factorial (lambda (n)
                          (cond
                                ((eq? n 1) n)
                                (else (* n (factorial (- n 1)))))))
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
