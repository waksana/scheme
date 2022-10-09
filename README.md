scheme
========

- [x] Implement Register Machine
- [x] Use Register Machine instructions to interpret scheme
- [x] Use Scheme to interpret scheme
- [ ] Compile scheme to Register Machine Instructions using scheme

## Register Machine

Every program defines a Register Machine which can achieve some sort of functionality. Basically the machine has three parts: Datapath, Controller and Stack.

### Datapath

Datapath is composed of some registers, calculators and indicators. Registers stores the data need to be calculated. Calculators actually do the math. Indicator shows whether a condition is satisfied.

#### Registers

there a branch of registers. some of them has specific purpose, some of them can store any data you want. here is the register list.

- exp: point to the expression to be evaluated.
- env: evaluation environment
- fun: procedure to be applied
- argl: list of evaluated arguments.
- continue: this register stores where the control flow shoud goto
- val: returned value
- unev: temp register for expressions

#### Calculator

- arithmatic operations like add, sub, mul, div

- logistic operations like and, or

### Stack

Stack can store data in a FIFO order. it's an important component to implement a recurcive progress.

### Controller

Controller is a finite state machine which controls how the datapath and stack works. It can store a calculate result to a registor, goto another state based on indicators, read and write data in stack.

## Instruction

We can define our controller, datapath using instructions.  With a set of instructions, we can define any machine we want.

- define calculators: `!`, `&`, `|`, `+`, `-`, `*`, `/`, `%`

- define register operations: `assign`, `fetch`.

- define indicators: `=`, `>=`, `<=`, `>`, `<`.

- define state transfers: `goto`, `branch`.

- control stack: `save`, `restore`.

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
    (branch (= (fetch b) 0) done)
    (assign t (% (fetch a) (fetch b)))
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
    (branch (= n 1) base)
    (save continue)
    (save n)
    (assign n (- (fetch n) 1))
    (assign continue calculate)
    (goto main)
    calculate
    (restore n)
    (restore continue)
    (assign val (* (fetch n) (fetch val)))
    (goto (fetch continue))
    base
    (assign val (fetch n))
    (goto (fetch continue))
    done
  ))
```
