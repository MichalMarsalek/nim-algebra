# nim-algebra
Small pure Nim algebra library

Work in progress. Created in a process of trying to learn more about the Nim type system, macro system and compilation time shenanigans in general.
The central design decision is that each mathematical structure (ring, field, vector space etc.) has a unique type. This means that it is not possible to say iterate over different finite fields at runtime.

## Embeddings /implicit conversions
In usual mathematical notation many embeddings are implicit, for example we usualy doesn't distinquish between `1` as an integer, as a complex number of as a constan polynomial over the rationals. In a programming language all of the above are different things (different types) but they must be compatible when using the usual arithmetic operations. For example, when we type
```nim
x + 1
```
where the left operand is a linear polynomial while the right operand is an integer, the integer must be implicitly understood (embedded) as a constant polynomial in order to make the addition work.
Since converters are pain (and for other reasons) I have to create my own way of handling these.
There are essentially 2 cases we need to support
* T -> S ... both types are an atomic types that (usually) have the set inclusion relation (ZZ->QQ)
* R[T] -> R[S] ... R is a container types, T, S as in bullet 1 (ZZ^3 -> QQ^3)
* T -> R[T] ... T is any type and R is a container type (ZZ -> PR(ZZ,x))

All the three cases need to work in combination with each other (it must support chaining).

# Features

## Number rings & fields
Types:  
* [x] `ZZ` - integers
* [x] `QQ` - rational numbers
* [x] `RR` - real numbers
* [x] `CC` - complex numbers
* [x] `ZZ/(n)` - integers mod n
* [x] `ZZQ[d]` - quadratic extension of integers
* [x] `ZZ_i` - Gaussian integers - alias for `ZZQ[-1]`
* [x] `QQQ[d]` - quadratic extension of rationals
* [x] `QQ_i` - alias for `QQQ[-1]`

Functions:  
* [x] Usual arithmetic operations
* [ ] `gcd` - greatest common divisor over `ZZ`
* [ ] `egcd` - Bezout coefficients over `ZZ` using the Extended Euclidean algorithm
* [ ] Modular arithmetic goodies - CRT, Jacobi symbols, euler totient function etc.
* [ ] `random(ZZ/(n))` - random element
* [ ] `items(ZZ/(n))` - iterator over all elements
* [ ] `nonzero(ZZ/(n))` - iterator over all nonzero elements
* [ ] `invertible(ZZ/(n))` - iterator over all invertible elements

Supported embeddings:  
* `ZZ` -> `QQ` -> `RR` -> `CC`
* `QQQ[d]` -> `CC`
* `ZZZ[d]` -> `CC`
* `ZZ` -> `ZZ/(n)`
* `ZZ_i` -> `CC`


## Finite fields
Types:  
* [x] `GF(p^k)` - Galois field of cardinality p^k

Functions:  
* Usual arithmetic operations
* [ ] `trace`
* [ ] `norm`
* [ ] `random` - random element
* [x] `items` - iterator over all elements
* [x] `nonzero`/`invertible` - iterator over all nonzero elements

Supported embeddings:  
`ZZ` -> `GF(p,k)`

Examples:  
```nim
type F = GF(2^16, α) # this defines the field and binds the generator to the variable α
let a = α^2 + α + 1
let b = α^3 + α + 1
echo a*b
```

Notes:  
Finite fields of small characteristic and extension degree are instantiated using a built int database of Conway polynomials.  
For all fields instantiated using `GF()`, the ring generator is also a generator of the multiplicative group (property of Conway polynomials).
If you need to work with larger fields or with custom irreducible modulus, you can instantiate them manually using a general FactorRing.  
Binary fields of size <= 2^64 have a special opzimized implementation.

## Polynomial rings
Types:  
* [x] `PR(R,x)` - polynomial ring in variable `x` over the base ring `R`
* [ ] `PR(R,x,y,z)` - multivariate polynomials over the base ring `R`

Functions:  
* [x] Usual arithmetic operations
* [x] `deg` - degree
* [ ] `roots`
* [ ] `gcd` - greatest common divisor
* [ ] `egcd` - Bezout coefficients using the Extended Euclidean algorithm
* [ ] `random(d)` - random polynomial with max degree `d` (over finite rings)
* [ ] `items` - iterator over all elements (over finite rings)
* [ ] `nonzero` - iterator over all nonzero elements (over finite rings)
* [ ] `invertible` - iterator over all invertible elements (over finite rings)
* [ ] maybe common poly algos - factoring etc.

Supported embeddings:  
* `R` -> `PR(R,...)`
* `ZZ` -> `PR(R,...)` where `R` is in {`QQ`, `RR`, `CC`, `ZZ/(n)`, `GF(p)`}

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

Notes:  
Multivariate polynomials are internally represented as polynomials in the last variable with coefficents being polynomials in the rest of the variables.
This means that `PR(PR(QQ,x),y)` is the same as `PR(QQ,x,y)`.
The order of variables matter. `PR(QQ,x,y)` is not compatible with `PR(QQ,y,x)`.  
The `roots` function is only available over fields and returns only the roots in the field. If you wish to find all the roots, search in an extension.

## Ideals & Factor Rings
Types:  
* [x] `Ideal[R]` - a finitely generated ideal in a ring `R`
* [x] `R/I` - ring `R` factorized by and ideal `I`
* [ ] `random` - random element
* [ ] `items` - iterator over all elements
* [ ] `nonzero` - iterator over all nonzero elements
* [ ] `invertible` - iterator over all invertible elements

Supported embeddings:  
* `R` -> `R/I`
* `R` -> `PR(R,...)/I`
* `ZZ` -> `PR(R,...)/I` where `R` is in {`QQ`, `RR`, `CC`, `ZZ/(n)`, `GF(p)`}

Functions:  
* [x] Usual arithmetic operations
* [x] `I(f)` - principal ideal generated by polynomial `f`
* [x] `R/(f)` - rings fcatorized by a principal ideal `I(f)`

Notes:  
Only factors of polynomial rings are supported. Only main ideals are currently supported.

## Vectors & Matrices
Types:  
* [x] `S^n` - column vectors of dimension `n` over the ring `S`
* [x] `S^(n,m)` matrices of type `n×m` over the ring `S`
* [x] `AffineSpace[V]` - vector/affine subspace of `V`

Functions:
* [x] Usual arithmetic operations
* [x] `T` - transposition
* [x] `det` - determinant
* [x] `trace` - determinant
* [x] `diag` - create diagonal matrix or extract a diagonal
* [x] `\` - left division
* [ ] `A\\b` - solution to Ax = b
* [ ] `b//A` - solution to xA = b
* [x] `dump` - correctly aligned multiline dump
* [x] `span` - creates a vector subspace generated by given vectors
* [x] `AffineSpace[V] + V` - creates affine subspace
* [ ] `random` - random element (over finite rings)
* [ ] `items` - iterator over all elements (over finite rings)
* [ ] `nonzero` - iterator over all nonzero elements (over finite rings)
* [ ] `invertible(R^(n,n))` - iterator over all invertible elements (over finite rings)

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
* [ ] Probably mainly finitely generated abelian groups  
* [ ] Some basic dlog
