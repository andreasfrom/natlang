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

