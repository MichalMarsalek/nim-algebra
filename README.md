# nim-algebra
Small pure Nim algebra library

Work in progress. Created in a process of trying to learn more about the Nim type system, macro system and compilation time shenanigans in general.
The central design decision is that each mathematical structure (ring, field, vector space etc.) has a unique type. This means that it is not possible to say iterate over different finite fields at runtime.

# Features

## Number rings
Types:  
* `ZZ` - integers
* `QQ` - rational numbers
* `RR` - real numbers
* `CC` - complex numbers
* `ZZ/(n)` - integers mod n
* `ZZ_i` - Gaussian integers
* `QF[d]` - quadratic extension of rational numbers
* `QQ_i` - alias for `QF[-1]`

Functions:  
* Usual arithmetic operations
* Modular arithmetic goodies - CRT, Jacobi symbols, etc.

## Finite fields
Types:  
* `GF(p,k)` or `GF(p^k)` - Galois field of cardinality p^k

Functions:  
* Usual arithmetic operations
* `trace`

Examples:  
```nim
type F = GF(2^16, α) # this defines the field and binds the generator to the variable α
let a = α^2 + α + 1
let b = α^3 + α + 1
echo a*b
```

## Polynomial rings
* `PR(R,x)` - polynomial ring in variable `x` over the base ring `R`
* `PR(R,x,y,z)` - multivariate polynomials over the base ring `R`

Functions:  
* Usual arithmetic operations
* `deg` - degree
* maybe common poly algos - factoring etc.

Examples:  
```nim
type R = PR(ZZ,x) # this defines the poly ring and binds the generator to the variable x
type S = PR(ZZ,y)
let f = x^2 + x + 1
echo f
echo f("w") # substituting a different variable
echo f(2)   # substituting a value
echo f(x^2) # substituting a polynomial
echo f(y)   # substituting a different variable
```

## Vectors & Matrices
Types:  
* `S^n` - column vectors of dimension `n` over the ring `S`
* `S^(n,m)` matrices of type `n×m` over the ring `S`

Functions:
* Usual arithmetic operations
* `T` - transposition
* `det` - determinant
* `\` - left division
* `dump` - correctly aligned multiline dump

Examples:  
```nim
type M = ZZ^(3,2)
let m:M = [[1,2],[33,4],[5,6]]
dump m
dump m.T

type V = ZZ^(1,3) #row vectors
let v:V = [1,2,3]
dump v
dump v.T
```

## Some prime stuff, mainly to support the above
## Maybe some groups
Probably mainly finitely generated abelian groups
Some basic dlog
