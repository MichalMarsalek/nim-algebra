# nim-algebra
Small pure Nim algebra library

Work in progress. Created in a process of trying to learn more about the Nim type system, macro system and compilation time shenanigans in general.
The central design decision is that each mathematical structure (ring, field, vector space etc.) has a unique type. This means that it is not possible to say iterate over different finite fields at runtime.

# Features

This package implements:
* Commutative rings and fields:
  * 64bit approximations of Real and Complex numbers
  * Integers, Rationals and their quadratic extensions
  * Finite fields
  * Polynomials
  * General field of fractions
  * General factor rings
* Linear algebra
  * Vectors
  * Matrices
  * Vector spaces
* Finitely generated abelian groups

and basic operations and algorithms on those structures.

## Common ring functions
The following operators/functions are available for a ring `R` and `a,b` in `R`, `k` in `ZZ`. Note that some of the functions might not make sense for a particular ring.
* `zero R` - returns a zero of the ring,
* `one R` - returns a one of the ring,
* `gen R` - returns a generator of the ring,
* `a.inv` - returns an inverse of a. Raises ElementNotInvertibleError for noninvertible elements,  
* `isInvertible a` - indicates whether `a` is invertible,
* `-a, a+b, a-b, a*b, a/b, a^k` - common arithmetic operations
* `a mod b`,`a div b` - floored Euclidean division
* `a//b` - division in the field of fractions
* `card R` - cardinality of the ring. Raises InfiniteCardinalityError for infinite rings,
* `items R` - iterates over all elements of the ring. Raises UncountableCardinalityError for uncountable rings.
* `nonzero R` - iterates over all nonzero elements of the ring,
* `invertible R` - iterates over all invertible elements of the ring,
* `random R` - returns a random element. Raises InfiniteCardinalityError for infinite rings,
* `divisors a` - iterator over all positive divisors
* `positive R` - iterator over all positive elements
* `primes R` - iterator over all (positive) primes
* `isPrime a` - checks whether `a` is a prime
* `isIrreducible a` - checks whether `a` is irreducible
* `factor a` - factors into irreducible factors
* `gcd(a, b)` - greatest common divisor
* `egcd(a, b)` - Bezout coefficients using the Extended Euclidean algorithm

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

## Number rings & fields
Types:  
* [x] `int` - integers modulo 2^64 (TODO ops)
* [x] `RR, CC` - (approximate) real and complex numbers
* [x] `ZZ, QQ` - integers and rationals
* [x] `ZZ/(n)` - integers mod n
* [x] `ZZQ[d]` - quadratic extension of integers
* [x] `ZZ_i` - Gaussian integers - alias for `ZZQ[-1]`
* [x] `QQQ[d]` - quadratic extension of rationals
* [x] `QQ_i` - alias for `QQQ[-1]`

Additional functions:  
* [x] Euler totient function
* [ ] Modular arithmetic goodies - CRT, Jacobi symbols...
* [x] `sqrt(d)` - returns the generator of `ZZQ[d]` that is not `1`. `i` is written as `sqrt(-1)`
* [x] `conjugate(a)` - conjugate of quadratic extension element `a`
* [x] `norm(a)` - norm of quadratic extension element `a`

Supported embeddings:  
* `int` -> `ZZ` -> `QQ` -> `RR` -> `CC`
* `QQQ[d]` -> `CC`
* `ZZQ[d]` -> `CC`
* `ZZ` -> `ZZ/(n)`

Examples:  
```nim
let a = 2 + 3*sqrt(5)   # 2 + 3√5
let b = 3//4 + 2*sqrt(-1) # ¾ + 2i
```

## Finite fields
Types:  
* [x] `GF(p^k)` - Galois field of cardinality p^k

Additional functions:  
* [ ] `trace`
* [ ] `norm`

Supported embeddings:  
`int` -> `ZZ` -> `ZZ/(n)` -> `GF(p,k)`

Examples:  
```nim
type F = GF(2^16, α) # this defines the field and binds the generator to the variable α
let a = α^2 + α + 1
let b = α^3 + α + 1
echo a*b
```

Notes:  
Finite fields of small characteristic and extension degree are instantiated using a built in database of Conway polynomials.  
For all fields instantiated using `GF()`, the ring generator is also a generator of the multiplicative group (property of Conway polynomials).
If you need to work with larger fields or with custom irreducible modulus, you can instantiate them manually using a general FactorRing.  
Binary fields of size <= 2^64 have a special optimized implementation.

## Polynomial rings
Types:  
* [x] `R+[x]` - polynomial ring in variable `x` over the base ring `R`
* [x] `R+[x,y,z]` - multivariate polynomials over the base ring `R`

Additional functions:  
* [x] `deg` - degree
* [x] `roots` - done for ZZ,QQ
* [x] `lc` - leading coefficient
* [x] `cc` - constant coefficient

Supported embeddings:  
* `R` -> `R+[...]`

Examples:  
```nim
type R = ZZ+[x] # this defines the poly ring and binds the generator to the variable x
type S = ZZ+[y]
let f = x^2 + x + 1
echo f
echo f("w") # substituting a different variable
echo f(2)   # substituting a value
echo f(x^2) # substituting a polynomial
echo f(y)   # substituting a different variable
```

Notes:  
Multivariate polynomials are internally represented as polynomials in the last variable with coefficents being polynomials in the rest of the variables.
This means that `QQ+[x]+[y]` is the same as `QQ+[x,y]`.
The order of variables matter. `QQ+[x,y]` is not compatible with `QQ+[y,x]`.  
The `roots` returns only the roots in the field/ring. If you wish to find all the roots, search in an extension.

## Ideals & Factor Rings
Types:  
* [x] `Ideal[R]` - a finitely generated ideal in a ring `R`
* [x] `R/I` - ring `R` factorized by and ideal `I`

Additional functions:  
* [x] `I(f)` - principal ideal generated by polynomial `f`
* [x] `R/(f)` - ring factorized by a principal ideal `I(f)`

Supported embeddings:  
* `R` -> `R/I`
* `R` -> `PR(R,...)/I`
* `ZZ` -> `PR(R,...)/I` where `R` is in {`QQ`, `RR`, `CC`, `ZZ/(n)`, `GF(p)`}

Notes:  
Only principal ideals are currently supported.

## Field of fractions
Types:  
* [x] `R/R` - field of fractions of the ring `R` or `R` if `R` is a field

Supported embeddings:  
* `R` -> `R/R`

Notes:  
One can embed a division to the field of fractions using the `//` operator.  
Note that `QQ` is just `ZZ/ZZ`. As a special rule, `int/int` is the same, while `ZZQ[D]/ZZQ[D]` is `QQQ[D]/QQQ[D]`.

## Quadratic extensions
There is a more optimized implementation of `(R+[x])/(x^2+D)`. This implementation is `QuadraticExtension[T, D]`. Altough it is generic, it probably is mostly useful for cases where `T` is `ZZ` or `QQ`, which is how `ZZQ[D]`and `QQQ[D]` is defined.

## Vectors & Matrices
Types:  
* [x] `S^n` - column vectors of dimension `n` over the ring `S`
* [x] `S^(n,m)` matrices of type `n×m` over the ring `S`
* [x] `AffineSpace[V]` - vector/affine subspace of `V`

Functions:
* [x] Usual arithmetic operations
* [x] `T` - transposition
* [x] `A[i,j]` - indexing
* [x] `v.norm(p=2)` - norm of a vector - p can be in {0,1,2,-1} where -1 indicates the oo-norm. 2-norm is only available over the reals
* [x] `v.norm2` - norm of a vector squared
* [ ] `todo` - matrix norms
* [x] `det` - determinant
* [x] `trace` - determinant
* [x] `diag` - create diagonal matrix or extract a diagonal
* [x] `\` - left division
* [ ] `A\\b` - solution to Ax = b
* [ ] `b//A` - solution to xA = b
* [x] `rowEcheleon` - rowEcheleon form of given matrix
* [x] `rank` - rank
* [x] `.rankAtLeast k` - indicates whether a rank is at least
* [x] `isRegular`/`isInvertible` - indicates whether given matrix is regular
* [ ] `ker` - kernel
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

## Maybe some groups
* [ ] Probably mainly finitely generated abelian groups  
* [ ] Some basic dlog
