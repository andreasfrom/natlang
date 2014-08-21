sub(a, 0) = a
sub(0, _) = 0
sub(Sa, Sb) = sub(a,b)

modi(a, a, _) = 0
modi(0, _, orig) = orig
modi(_, S0, _) = 0
modi(a, b, _) = modi(sub(a,b), b, a)

mod(a, b) = modi(a,b,a)

primei(_, S0, _) = S0
primei(_, _, 0) = 0
primei(a, Sn, _) = primei(a, n, mod(a,n))

prime(S0) = 0
prime(Sa) = primei(S(a), a, mod(S(a), a))

eleven = SSSSSSSSSSS0

-- returns S0 for true
main = prime(eleven)
