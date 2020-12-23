(define Y
  (lambda (fun)
    ((lambda (f) (f f))
     (lambda (f) (fun (lambda (x) ((f f) x)))))))
(define add
  (lambda (x y)
    (cond
      ((eq? x 0) y)
      (else
        (add (sub1 x) (add1 y))))))
(define sub
  (lambda (x y)
    (cond
      ((eq? y 0) x)
      (else
        (sub (sub1 x) (sub1 y))))))
((Y (lambda (fib)
      (lambda (n)
        (cond
          ((eq? n 0) 0)
          ((eq? n 1) 1)
          (else
            (add (fib (sub1 n)) (fib (sub n 2))))))))
 10)
