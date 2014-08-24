# natlang
My experiments in implementing a little language in Haskell for exploring natural numbers (incl. 0).

This is still under development, so the error messages are still quite rough and the implementation is trivial, leaving some to be desired for numerical performance (2^9 naively takes 16 seconds), but that is not the point anyway.
The language should serve as a playground fo experimentation, providing the raw building blocks for understanding concepts such as addition and recursion.

# Example

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

-- Evaluates to 55
main = fib(ten)
```

# Copyright and license
Copyright (c) 2014 Andreas H. From

Licensed under the MIT License (see the file LICENSE)
