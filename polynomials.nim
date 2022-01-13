include prelude
import numbers
import sugar, macros, math
{.experimental: "callOperator".}

type PolynomialRing*[TT; V:static string] = object
    coeffs*:seq[TT]
type PolynomialRing2*[TT;V1,V2:static string] = PolynomialRing[PolynomialRing[TT,V1],V2]
type PolynomialRing3*[TT;V1,V2,V3:static string] = PolynomialRing[PolynomialRing2[TT,V1,V2],V3]
type PolynomialRing4*[TT;V1,V2,V3,V4:static string] = PolynomialRing[PolynomialRing3[TT,V1,V2,V3],V4]

template `$`*(T:typedesc[PolynomialRing]):string =
  var inner = $T.TT
  if inner.startsWith "PR(":
    inner = inner[3..^2]
  "PR(" & inner & "," & T.V & ")"

type Ring* = Number | PolynomialRing

#Type construction macros
macro PR*(ring:typedesc[Ring],variable:untyped{ident}):typedesc[PolynomialRing] =
    let varname = $variable
    result = quote do:
        when not declared(`variable`):
            const `variable` = PolynomialRing[`ring`, `varname`](coeffs: @[`ring`(0), `ring`(1)])
        type temp = PolynomialRing[`ring`, `varname`]
        temp

#Lifting macros TODO make it general
func ConstantPoly*[TT,V](a:TT):PolynomialRing[TT,V] =
    PolynomialRing[TT, V](coeffs: @[a])


template zero*(R:typedesc[Number]):R = R(0)
template one*(R:typedesc[Number]):R = R(1)

template zero*[TT;V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    discard
template one*[TT; V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    PolynomialRing[TT, V](coeffs: @[one(typeof TT)])

template gen*[TT; V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    PolynomialRing[TT, V](coeffs: @[zero(typeof TT), one(typeof TT)])


macro liftOp1(op0:string):untyped =
    let op = newIdentNode($op0)
    result = quote do:
        func `op`*[TT,V](f:PolynomialRing[TT,V],g:TT):PolynomialRing[TT,V] =
            `op`(f, ConstantPoly[TT,V](g))
        func `op`*[TT,V](f:TT,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
            `op`(ConstantPoly[TT,V](f), g)
    #echo toStrLit(result)
macro liftOps1():untyped =
    result = quote do:
        liftOp1 "+"
        liftOp1 "-"
        liftOp1 "*"

func deg*[TT,V](f: PolynomialRing[TT,V]):int {.inline.} =
    f.coeffs.len - 1

iterator pairs*[TT,V](f: PolynomialRing[TT,V]): (int,TT) =
    for i,fi in f.coeffs:
        yield (i,fi)

func `$`*[TT; V:static string](f:PolynomialRing[TT, V]): string =
    var parts: seq[string]
    for e,c in f:
        if c == TT(0): continue
        if e == 0:
            parts.add($c)
        else:
            let typ = if e > 1: fmt"{V}^{e}" else: $V
            let sc = $c
            if sc == "1":
                parts.add(typ)
            elif sc == "-1":
                parts.add("-"&typ)
            elif "+" in sc or "-" in sc[1..^1]:
                parts.add(fmt"({sc}){typ}")
            else:
                parts.add(fmt"{sc}{typ}")
    if parts.len == 0: return "0"
    parts.join("+").replace("+-","-")

func rename[TT,V1](f:PolynomialRing[TT,V1], V2: static string):PolynomialRing[TT,V2] =
    PolynomialRing[TT,V2](f) #deprecate?

func normalize[TT,V](f: var PolynomialRing[TT,V]) =
    var i = deg(f)
    while f.coeffs[i] == 0:
        dec i
    f.coeffs.setlen(i+1)

func `+=`*[TT,V](f: var PolynomialRing[TT,V],g:PolynomialRing[TT,V]) =
    let needsNormalizing = deg(f) == deg(g)
    let mindeg = min(deg f, deg g)
    let maxdeg = max(deg f, deg g)
    for i in 0..mindeg:
        f.coeffs[i] += g.coeffs[i]
    for i in (deg(f)+1)..maxdeg:
        f.coeffs.add g.coeffs[i]
    if needsNormalizing:
        normalize f
func `+`*[TT,V](f,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    result = f
    result += g
func `-=`*[TT,V](f: var PolynomialRing[TT,V],g:PolynomialRing[TT,V]) =
    let needsNormalizing = deg(f) == deg(g)
    let mindeg = min(deg f, deg g)
    let maxdeg = max(deg f, deg g)
    for i in 0..mindeg:
        f.coeffs[i] -= g.coeffs[i]
    for i in (deg(f)+1)..maxdeg:
        f.coeffs.add -g.coeffs[i]
    if needsNormalizing:
        normalize f
func `-`*[TT,V](f,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    result = f
    result -= g
func `-`*[TT,V](f:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    for i in 0..deg(f):
        f.coeffs[i] = -f.coeffs[i]

func `*`*[TT,V](f,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    result.coeffs = newSeq[TT](deg(f)+deg(g)+1)
    for i,fi in f:
        for j,gj in g:
            result.coeffs[i+j] += fi*gj
func `*=`*[TT,V](f: var PolynomialRing[TT,V],g:PolynomialRing[TT,V]) =
    f = f*g

#TODO generalise this lifting
liftOps1()

include algos
func `^`*[TT,V](f:PolynomialRing[TT,V], exp:int):PolynomialRing[TT,V] =
    binaryExponentiation(f, exp)

func `()`*[TT,V1](f:PolynomialRing[TT,V1], V2: static string):PolynomialRing[TT,V2] =
    PolynomialRing[TT,V2](f)

func `()`*[TT,V,TT2](f:PolynomialRing[TT,V], val: TT2):TT2 =
    for i in countdown(deg f, 0):
        result = result * val + f.coeffs[i]

#func `()`*[TT,V1,V2](f:PolynomialRing[TT,V1], val: PolynomialRing[TT,V2]):PolynomialRing[TT,V2] =
#    for i in countdown(deg f, 0):
#        result = result * val + f.coeffs[i]
when isMainModule:
    echo int.zero
    type R = PR(ZZ,x)
    type S = PR(ZZ,y)
    let f = 1 + x^2 + x
    echo f
    echo f("w")
    echo f(2)
    echo f(x^2)
    echo f(y)
    echo R
    
    