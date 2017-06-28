jscheme
========

scheme in js [demo](https://waksana.github.io/jscheme/?(define%20Y%0A%20%20(lambda%20(fun)%0A%20%20%20%20((lambda%20(f)%20(f%20f))%0A%20%20%20%20%20(lambda%20(f)%20(fun%20(lambda%20(x)%20((f%20f)%20x)))))))%0A(define%20add%0A%20%20(lambda%20(x%20y)%0A%20%20%20%20(cond%0A%20%20%20%20%20%20((eq%3F%20x%200)%20y)%0A%20%20%20%20%20%20(else%0A%20%20%20%20%20%20%20%20(add%20(sub1%20x)%20(add1%20y))))))%0A(define%20sub%0A%20%20(lambda%20(x%20y)%0A%20%20%20%20(cond%0A%20%20%20%20%20%20((eq%3F%20y%200)%20x)%0A%20%20%20%20%20%20(else%0A%20%20%20%20%20%20%20%20(sub%20(sub1%20x)%20(sub1%20y))))))%0A((Y%20(lambda%20(fib)%0A%20%20%20%20%20%20(lambda%20(n)%0A%20%20%20%20%20%20%20%20(cond%0A%20%20%20%20%20%20%20%20%20%20((eq%3F%20n%200)%200)%0A%20%20%20%20%20%20%20%20%20%20((eq%3F%20n%201)%201)%0A%20%20%20%20%20%20%20%20%20%20(else%0A%20%20%20%20%20%20%20%20%20%20%20%20(add%20(fib%20(sub1%20n))%20(fib%20(sub%20n%202))))))))%0A%2010))

# available syntaxs:

- quote
- lambda
- cond
- define

# basic functions:

- eq?
- zero?
- number?
- add1
- sub1
- cons
- car
- cdr
- null?
- atom?
