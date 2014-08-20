# natlang
My experiments in implementing a programming language in Haskell, not a general purpose one, but one for exploring natural numbers.

This is still under development, so the parser is quite rough, the error messages even more so, and the file to run is currently hardcoded.

# Example
The code below defines addition and multiplication of natural numbers through recursion and then defines fibonacci.
It evaluates to my natural number representation of 55.

```haskell
add(a, 0) = a
add(a, Sb) = S(add(a, b))

mul(_, 0) = 0
mul(a, S0) = a
mul(a, Sb) = add(a, mul(a, b))
  
fib(0) = 0
fib(S0) = S0
fib(SSn) = add(fib(S(n)), fib(n))

ten = mul(SS0, SSSSS0)

main = fib(ten)
```

# Copyright and license
Copyright (c) 2014 Andreas H. From

Licensed under the MIT License (see the file LICENSE)
