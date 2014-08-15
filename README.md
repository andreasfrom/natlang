# natlang
My experiments in implementing a programming language in Haskell, not a general purpose one, but one for exploring natural numbers.

This is still under development, so the parser is quite rough, the error messages even more so, and the file to run is currently hardcoded.

# Example
The code below defines addition and multiplication of natural numbers through recursion and then defines fibonacci.
It evaluates to the natural number equivalent of 55.

```ruby
add(a, Z) = a
add(a, Sb) = S add(a, b)

mul(a, Z) = Z
mul(a, SZ) = a
mul(a, Sb) = add(a, mul(a, b))
  
fib(Z) = Z
fib(SZ) = SZ
fib(SSn) = add(fib(S n), fib(n))

ten = mul(SSZ, SSSSSZ)

main() = fib(ten)
```

# Copyright and license
Copyright (c) 2014 Andreas H. From

Licensed under the MIT License (see the file LICENSE)
