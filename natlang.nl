add(a, 0) = a
add(a, Sb) = S(add(a, b))

sub(a, 0) = a
sub(0, _) = 0
sub(Sa, Sb) = sub(a,b)
  
mul(_, 0) = 0
mul(a, S0) = a
mul(a, Sb) = add(a, mul(a, b))

exp(_, 0) = S0
exp(a, S0) = a
exp(a, Sb) = mul(a, exp(a, b))

fib(0) = 0
fib(S0) = S0
fib(SSn) = add(fib(S(n)), fib(n))

ten = mul(SS0, SSSSS0)

-- 2^3 evaluates to 8
main = exp(SS0, SSS0)