include prelude
import errors
import sugar, macros, typetraits, randoms

type FactorRing*[TT;M:static TT] = object
    val:TT
func `$`*[TT,M](R:typedesc[FactorRing[TT,M]]):string =
    $TT & "/(" & $M & ")"
func zero*[TT,M](R:typedesc[FactorRing[TT,M]]):R =
    R(val: TT.zero)
func one*[TT,M](R:typedesc[FactorRing[TT,M]]):R =
    R(val: TT.one)

#Ideals
#TODO convert small ZZ gens to ints implicitly
type Ideal*[P] = object
    generator*: P
func I*[P](p:P):Ideal[P] =
    result.generator = p
func `*`*[P](p:P, _:typedesc[P]):Ideal[P] =
    I(p)
template `/`*(T:typedesc,p:Ideal[T]):typedesc =
    FactorRing[T,p.generator]
template `/`*(T:typedesc,p:T):typedesc =
    FactorRing[T,p]
template `/`*(T:typedesc[ZZ],p:Ideal[int]):typedesc =
    FactorRing[int,p.generator]
template `/`*(T:typedesc[ZZ],p:range[0..2147483647]):typedesc =
    FactorRing[int,p]
func `$`*(p:Ideal):string =
    "I(" & $p.generator & ")"

#value + ideal -> [value]
func `+`*[TT](p:TT, M:static[Ideal[TT]]):auto =
    FactorRing[TT,M.generator](val: p)

#ops in R/I
func `+`*[TT, M](f,g:FactorRing[TT,M]):FactorRing[TT, M] =
    result.val = (f.val + g.val) mod M
func `-`*[TT, M](f,g:FactorRing[TT,M]):FactorRing[TT, M] =
    result.val = (f.val - g.val) mod M
func `-`*[TT, M](f:FactorRing[TT,M]):FactorRing[TT, M] =
    result.val = (-f.val) mod M
func `*`*[TT, M](f,g:FactorRing[TT,M]):FactorRing[TT, M] =
    result.val = (f.val * g.val) mod M
func inv*[TT, M](f:FactorRing[TT,M]):FactorRing[TT, M] =
    let (g,u,v) = f.val.egcd(M)
    if g != TT.one:
        raiseInvertError(f)
    return u + I(M)

func `$`*[TT, M](f:FactorRing[TT, M]):string =
    $f.val & " + I(" & $M & ")"
func `$`*[M](f:FactorRing[int, M]):string =
    "[" & $f.val & "]"
func `$`*[M](f:FactorRing[ZZ, M]):string =
    "[" & $f.val & "]"

#temp for ZZ/(n)
import options
proc random*[M](R:typedesc[FactorRing[ZZ,M]]):R =
    const mx = toSignedInt[int](M).get-1
    R(val: randInt(mx).initZZ)

iterator items*[M](R:typedesc[FactorRing[ZZ,M]]):R =
    for i in 0.initZZ..<M.initZZ:
        yield R(val: i)
iterator nonzero*[M](R:typedesc[FactorRing[ZZ,M]]):R =
    for i in 1.initZZ..<M.initZZ:
        yield R(val: i)
iterator invertible*[M](R:typedesc[FactorRing[ZZ,M]]):R =
    for i in 1.initZZ..<M.initZZ:
        if gcd(i, M) == 1.initZZ:
            yield R(val: i)