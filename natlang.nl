add(a, Z) = a
add(a, Sb) = S add(a, b)

sub(a, Z) = a
sub(Z, b) = Z
sub(Sa, Sb) = sub(a,b)
  
mul(a, Z) = Z
mul(a, SZ) = a
mul(a, Sb) = add(a, mul(a, b))

exp(a, Z) = SZ
exp(a, SZ) = a
exp(a, Sb) = mul(a, exp(a, b))

fib(Z) = Z
fib(SZ) = SZ
fib(SSn) = add(fib(S n), fib(n))
  
ten = mul(SSZ, SSSSSZ)

main() = exp(SSZ, SSSZ)

