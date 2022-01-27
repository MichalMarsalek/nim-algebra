import sequtils, sugar
import randoms
include numbers, finite_fields, polynomials, matrices

#Numbers
func embed*(a:int, R:typedesc[ZZ]):R =
    a.initZZ
func embed*[TT](a:TT, R:typedesc[Fractions[TT]]):R =
    result.num = a
    result.den = TT.one
func embed*(a:QQ, _:typedesc[RR]):RR =
    a.toFloat
func embed*(a:RR, _:typedesc[CC]):CC     =
    complex(a, RR.zero)
func embed*[D: static QQ](a:QQQ[D], _:typedesc[CC]):CC     =
    a.x.embed(CC) + D.embed(CC).sqrt * a.y.embed(CC)
func embed*[D: static ZZ](a:ZZQ[D], R:typedesc[QQQ]):R     =
    when D == R.D:
        result.x = a.x.embed(QQ)
        result.y = a.y.embed(QQ)

func embed*[M](a:ZZ, R:typedesc[ZZMod[M]]):R =
    R a

func embed*(a:ZZ, _:typedesc[RR]):RR =
    a.embed(QQ).embed(RR)
func embed*(a:ZZ, _:typedesc[CC]):CC =
    a.embed(RR).embed(CC)
func embed*(a:QQ, _:typedesc[CC]):CC =
    a.embed(RR).embed(CC)
func embed*(a:int, R:typedesc[QQ|RR|CC]):R =
    a.embed(ZZ).embed(R)
func embed*[D](a:ZZQ[D], _:typedesc[CC]):CC =
    a.embed(QQQ[D]).embed(CC)

#Polynomials
func embed*[T1,T2,V](a:PolynomialRing[T1,V],R:typedesc[PolynomialRing[T2,V]]):R =
    result.coeffs = a.coeffs.mapIt(it.embed(T2))
func embed*[TT,V](a:TT,R:typedesc[PolynomialRing[TT,V]]):R =
    result.coeffs = @[a]
func embed*[T1,T2,V](a:T1,R:typedesc[PolynomialRing[T2,V]]):R =
    result.coeffs = @[a.embed(T2)]

#Matrices
func embed*[T1,T2,M,N](a:MatrixSpace[T1,M,N],R:typedesc[MatrixSpace[T2,M,N]]):R =
    for i in 0..<M*N:
        result.entries[i] = a.entries[i].embed(T2)

#FiniteFields
func embed*[DEG,MOD, V](a:int,R:typedesc[BinaryField[DEG,MOD, V]]):R =
    R(a mod 2)
#TODO ZZ -> GF(2^k)
#TODO intMod[2] -> GF(2^k)
func embed*[P,DEG,MOD, V](a:int,R:typedesc[GenFiniteField[P,DEG,MOD, V]]):R =
    result.coeffs[0] = a mod P
#TODO ZZ -> GF(p^k)
#TODO intMod[P] -> GF(P^k)

#Factor rings
func embed*[T,C](a:T,R:typedesc[FactorRing[T,C]]):R =
    result.val = a
func embed*[T1,T2,C](a:T1,R:typedesc[FactorRing[T2,C]]):R =
    result.val = a.embed(T2)

template `/`*(T:typedesc,p:auto):typedesc =
    FactorRing[T,p.embed(T)]

type Embeddable = concept type T
    T is Number or T is PolynomialRing or T is FiniteField or T is ZZQ or T is QQQ or T is FactorRing or T is MatrixSpace

type Ring = Embeddable #TODO

#TODO avoid duplicities
template `*`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) * b
    elif compiles(b.embed(typeof(a))):
        a * b.embed(typeof(a))
    else:
        {.error: "Cannot embed " & $typeof(a) & " in " & $typeof(b) & " nor " & $typeof(b) & " in " & $typeof(a) & "."}
template `/`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) / b
    elif compiles(b.embed(typeof(a))):
        a / b.embed(typeof(a))
    else:
        {.error: "Cannot embed " & $typeof(a) & " in " & $typeof(b) & " nor " & $typeof(b) & " in " & $typeof(a) & "."}
template `+`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) + b
    elif compiles(b.embed(typeof(a))):
        a + b.embed(typeof(a))
    else:
        {.error: "Cannot embed " & $typeof(a) & " in " & $typeof(b) & " nor " & $typeof(b) & " in " & $typeof(a) & "."}
template `-`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) - b
    elif compiles(b.embed(typeof(a))):
        a - b.embed(typeof(a))
    else:
        {.error: "Cannot embed " & $typeof(a) & " in " & $typeof(b) & " nor " & $typeof(b) & " in " & $typeof(a) & "."}

func `^`*(value:Ring, exp:int|ZZ):typeof(value) =
    result = typeof(value).one
    var intermediate = value
    var exp1 = exp
    while exp1 > 0:
        if exp1 mod 2 == 1:
            result *= intermediate
        intermediate *= intermediate
        exp1 = exp1 shr 1

func `/`*(a,b:Ring):typeof(value) =
    a * b.inv

func `//`*(a,b:Ring):(typeof(a)/typeof(a)) =
    type FR = (typeof(a)/typeof(a))
    a.embed(FR) / b.embed(FR)
    

when isMainModule:
    type R = ZZ+[x,y,z,w]
    echo 2 + 3*x + 5*y
    echo x*y + 4*y - 58*x^2 * w
    #echo (1 + sqrt(-2)) #why doesnt work suddenly
    type R2 = ((GF(16, "beta")+[X])/X)^3
    echo R2
    type R3 = GF(16, "b")^(3,3)
    echo R3.random
    type R4 = ZZ/5+[x]
    echo R4
    for e in R4:
        echo e
    #[
    let m = ((ZZ/10)^(3,3)).random
    dump m
    let v = (ZZ^3) [11,12,13]
    let mshift = m + diag(v)
    dump mshift]#
