sub(a, Z) = a
sub(Z, _) = Z
sub(Sa, Sb) = sub(a,b)

-- Convention: Z is false, SZ is true
equals(Z, Z) = SZ
equals(_, Z) = Z
equals(Z, _) = Z
equals(Sa, Sb) = equals(a,b)

modi(_, _, _, SZ) = Z
modi(Z, _, orig, _) = orig
modi(_, SZ, _, _) = Z
modi(a, b, _, _) = modi(sub(a,b), b, a, equals(sub(a,b), b))

mod(a, b) = modi(a,b,a,equals(a,b))

primei(_, SZ, _) = SZ
primei(_, _, Z) = Z
primei(a, Sn, _) = primei(a, n, mod(a,n))

prime(SZ) = SZ
prime(Sa) = primei(S a, a, mod(S a, a))

eleven = SSSSSSSSSSSZ

-- returns SZ, the convention for true
main = prime(eleven)