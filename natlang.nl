add(a, Z) = a
add(a, Sb) = S add(a, b)

sub(a, Z) = a
sub(Z, _) = Z
sub(Sa, Sb) = sub(a,b)
  
mul(_, Z) = Z
mul(a, SZ) = a
mul(a, Sb) = add(a, mul(a, b))

exp(_, Z) = SZ
exp(a, SZ) = a
exp(a, Sb) = mul(a, exp(a, b))

fib(Z) = Z
fib(SZ) = SZ
fib(SSn) = add(fib(S n), fib(n))
  
ten = mul(SSZ, SSSSSZ)

-- 2^3 evaluates to 8
main = exp(SSZ, SSSZ)

