import sequtils
import numbers, quadratic_extensions, finite_fields, matrices, polynomials, factor_rings
#[
EMBEDDINGS = [
  #Numbers
  [int, ZZ]
  
  [QQ, RR, CC],
  [QQQ[?D], CC],
  [ZZQ[?D], CC],
  [ZZ, ZZMod[?M]],
  [int, intMod[?M]],
  #Finite rings
  [ZZ, BinaryField[?DEG,?MOD,?V]]
  [ZZ, GenFiniteField[?P,?DEG,?MOD,?V]]
  #Polynomials
  [?D, PolynomialRing[?D]],
  [PolynomialRing[?T1,?V],PolynomialRing[?T2,?V]] #TODO
  #Factor rings
  [?R, FactorRing[?R,?C]]
  #Matrices
  [MatrixSpace[?T1],MatrixSpace[?T2]] #TODO
]#
  
  
const MAX_EMBED_DEPTH = 10
#[
val.embed(T) atomic embed
val.embed(T1,T2,T3) ---> val.embed(T1).embed(T2).embed(T3)

a:T + b:S --> 
    add(a.embed(path,path,S), b)
    or
    add(a, b.embed(path,path,T))
]#

#Numbers
func embed*(a:ZZ, _:typedesc[QQ]):QQ =
    a // ZZ.one
func embed*(a:QQ, _:typedesc[RR]):RR =
    a.toFloat
func embed*(a:RR, _:typedesc[CC]):CC     =
    complex(a, RR.zero)
func embed*[D](a:QQQ[D], _:typedesc[CC]):CC     =
    a.x.embed(CC) + D.embed(CC).sqrt * a.y.embed(CC)
func embed*[D](a:ZZQ[D], R:typedesc[QQQ[D]]):R     =
    result.x = a.x.embed(QQ)
    result.y = a.y.embed(QQ)

func embed*(a:ZZ, _:typedesc[RR]):RR =
    a.embed(QQ).embed(RR)
func embed*(a:ZZ, _:typedesc[CC]):CC =
    a.embed(RR).embed(CC)
func embed*(a:QQ, _:typedesc[CC]):CC =
    a.embed(RR).embed(CC)
func embed*(a:int, R:typedesc[ZZ|QQ|RR|CC]):R =
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
    result.entries = a.entries.mapIt(it.embed(T2))

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

type Embeddable = concept type T
    T is Number or T is PolynomialRing or T is FiniteField or T is ZZQ or T is QQQ or T is FactorRing

#TODO avoid duplicities
template `*`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) * b
    elif compiles(b.embed(typeof(a))):
        a * b.embed(typeof(a))
    else:
        raise newException(Exception, "Cannot embed " & $T1 & " in " & $T2 & " nor vice versa.")
template `/`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) / b
    elif compiles(b.embed(typeof(a))):
        a / b.embed(typeof(a))
    else:
        raise newException(Exception, "Cannot embed " & $T1 & " in " & $T2 & " nor vice versa.")
template `+`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) + b
    elif compiles(b.embed(typeof(a))):
        a + b.embed(typeof(a))
    else:
        raise newException(Exception, "Cannot embed " & $T1 & " in " & $T2 & " nor vice versa.")
template `-`[T1,T2:Embeddable](a:T1, b:T2):untyped =
    when compiles(a.embed(typeof(b))):
        a.embed(typeof(b)) - b
    elif compiles(b.embed(typeof(a))):
        a - b.embed(typeof(a))
    else:
        raise newException(Exception, "Cannot embed " & $T1 & " in " & $T2 & " nor vice versa.")
    

when isMainModule:
    type R = PR(ZZ, x,y,z,w)
    echo 2 + 3*x + 5*y
    echo x*y + 4*y - 58*x^2 * w
