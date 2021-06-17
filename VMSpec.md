# Register Machine

## Machine

Every program is a Machine which can achieve some sort of functionality. Basically the machine has three parts: Datapath, Controller and Stack.

### Datapath

Datapath is composed of some registers, calculators and indicators. Registers stores the data need to be calculated. Calculators actually do the math. Indicator shows whether a condition is satisfied.

#### Registers

there a branch of registers. some of them has specific purpose, some of them can store any data you want. here is the register list.

- Continue
This register stores where the control flow shoud goto

#### Calculator

- arithmatic operations like add, sub, mul, div

- logistic operations like and, or

### Stack

Stack can store data in a FIFO order. it's an important component to implement a recurcive progress.

### Controller

Controller is a finite state machine which controls how the datapath and stack works. It can store a calculate result to a registor, goto another state based on indicators, read and write data in stack.

## Instruction

We can define our controller, datapath using instructions.  With a set of instructions, we can define any machine we want.

- define calculators: add, sub, and, or, add, sub, mul, div, remainder.

- define register operations: assign, fetch.

- define indicators: zero?, eq?.

- define state transfers: goto, branch.

- control stack: push, pop.

## Example

### GCD

```scheme
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (mod a b))))
```

```
(define-machine gcd
  (registers a b t)
  (controller
    loop
    (branch (zero? b) done)
    (assign t (remainder (fetch a) (fetch b)))
    (assign a (fetch b))
    (assign b (fetch t))
    (goto loop)
    done)
)
```

### Factorial

```scheme
(define (factorial n)
        (if (= n 1)
            n
            (* n (factorial (- n 1)))))
```

```
(define-machine factorial
  (registers n val)
  (controller
    (assign continue done)
    main
    (branch (equal? n 1) (fetch continue))
    (push (fetch n))
    (push (fetch continue))
    (assign continue calculate)
    (assign n (sub (fetch n) 1))
    (goto main)
    calculate
    (assign continue (pop))
    (assign val (pop))
    (assign n (mul n val))
    (goto continue)
    done
  ))
```