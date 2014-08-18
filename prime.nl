sub(a, Z) = a
sub(Z, _) = Z
sub(Sa, Sb) = sub(a,b)

modi(a, a, _) = Z
modi(Z, _, orig) = orig
modi(_, SZ, _) = Z
modi(a, b, _) = modi(sub(a,b), b, a)

mod(a, b) = modi(a,b,a)

primei(_, SZ, _) = SZ
primei(_, _, Z) = Z
primei(a, Sn, _) = primei(a, n, mod(a,n))

prime(SZ) = SZ
prime(Sa) = primei(S a, a, mod(S a, a))

eleven = SSSSSSSSSSSZ

-- returns SZ for true
main = prime(eleven)