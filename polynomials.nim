include prelude
import numbers
import sugar, macros, math, algorithm
import algos, factorisations
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
            const `variable` = PolynomialRing[`ring`, `varname`](coeffs: @[zero `ring`, one `ring`])
        type temp = PolynomialRing[`ring`, `varname`]
        temp

#Lifting macros TODO make it general
func ConstantPoly*[TT,V](a:TT):PolynomialRing[TT,V] =
    PolynomialRing[TT, V](coeffs: @[a])

template zero*[TT;V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    PolynomialRing[TT, V](coeffs: @[zero(typeof TT)])
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

func lc*[TT,V](f: PolynomialRing[TT,V]):TT {.inline.} =
    f.coeffs[^1]
func cc*[TT,V](f: PolynomialRing[TT,V]):TT {.inline.} =
    f.coeffs[0]

iterator pairs*[TT,V](f: PolynomialRing[TT,V]): (int,TT) =
    for i,fi in f.coeffs:
        yield (i,fi)

func `$`*[TT; V:static string](f:PolynomialRing[TT, V]): string =
    var parts: seq[string]
    for e,c in f:
        if c == zero TT: continue
        if e == 0:
            parts.add($c)
        else:
            let typ = if e > 1: fmt"{V}^{e}" else: $V
            let sc = $c
            if c == TT.one:
                parts.add(typ)
            elif c == -TT.one:
                parts.add("-"&typ)
            elif "+" in sc or "-" in sc[1..^1]:
                parts.add(fmt"({sc}){typ}")
            else:
                parts.add(fmt"{sc}{typ}")
    if parts.len == 0: return "0"
    parts.reversed.join("+").replace("+-","-")

func rename[TT,V1](f:PolynomialRing[TT,V1], V2: static string):PolynomialRing[TT,V2] =
    PolynomialRing[TT,V2](f) #deprecate?

func normalize[TT,V](f: var PolynomialRing[TT,V]) =
    var i = deg(f)
    while (i >= 0) and (f.coeffs[i] == zero TT):
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
    for ci in f.coeffs:
        result.coeffs.add -ci

func `*`*[TT,V](f,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    result.coeffs = newSeqWith(deg(f)+deg(g)+1, zero(TT))
    for i,fi in f:
        for j,gj in g:
            result.coeffs[i+j] += fi*gj
func `*=`*[TT,V](f: var PolynomialRing[TT,V],g:PolynomialRing[TT,V]) =
    f = f*g
    
func divmod[TT,V](f,g:PolynomialRing[TT,V]):(PolynomialRing[TT,V],PolynomialRing[TT,V]) =
    #TODO make this more efficient
    var deg_fg = deg(f) - deg(g)
    result[1] = f
    result[0].coeffs = newSeqWith[TT](max(0, deg_fg+1), zero(TT))
    const x = typeof(f).gen
    while deg_fg >= 0:
        let c = result[1].lc / g.lc
        result[0].coeffs[deg_fg] = c
        #result[0] += c * x^deg_fg
        result[1] -= g * c * x^deg_fg
        deg_fg = deg(result[1]) - deg(g)

func `div`*[TT,V](f,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    divmod(f,g)[0]

func `mod`*[TT,V](f,g:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    divmod(f,g)[1]
        

#TODO generalise this lifting
liftOps1()

func `^`*[TT,V](f:PolynomialRing[TT,V], exp:int):PolynomialRing[TT,V] =
    binaryExponentiation(f, exp)

func `()`*[TT,V1](f:PolynomialRing[TT,V1], V2: static string):PolynomialRing[TT,V2] =
    PolynomialRing[TT,V2](f)

func `()`*[TT,V,TT2](f:PolynomialRing[TT,V], val: TT2):TT2 =
    result = zero(TT2)
    for i in countdown(deg f, 0):
        result = result * val + f.coeffs[i]

#func `()`*[TT,V1,V2](f:PolynomialRing[TT,V1], val: PolynomialRing[TT,V2]):PolynomialRing[TT,V2] =
#    for i in countdown(deg f, 0):
#        result = result * val + f.coeffs[i]

func roots*[V](f:PolynomialRing[ZZ,V]):seq[ZZ] =
    if f.coeffs[0] == zero(ZZ):
        result.add zero(ZZ)
    var ci = 0
    while ci <= deg(f) and f.coeffs[ci] == zero(ZZ):
        inc ci
    for p in f.coeffs[ci].divisors:
        for q in f.coeffs[deg f].divisors:
            if p mod q == 0:
                let x = p div q
                if f(x) == zero(ZZ):
                    result.add x
                if f(-x) == zero(ZZ):
                    result.add -x

func equalentZZPoly*[V](f:PolynomialRing[QQ,V]):PolynomialRing[QQ,V] =
    let L = lcm(f.coeffs.mapIt(it.den))
    result.coeffs = f.coeffs.mapIt(it * L)

func roots*[V](f:PolynomialRing[QQ,V]):seq[QQ] =
    var f = f.equalentZZPoly
    if f.coeffs[0] == zero(QQ):
        result.add zero(QQ)
    var ci = 0
    while ci <= deg(f) and f.coeffs[ci] == zero(QQ):
        inc ci
    for p in f.coeffs[ci].num.divisors:
        for q in f.coeffs[deg f].num.divisors:
            let x = p//q
            if f(x) == zero(QQ):
                result.add x
            if f(-x) == zero(QQ):
                result.add -x

func factor*[V](f:PolynomialRing[QQ,V]):Factorisation[typeof f] =
    #TODO this only factors out linear factors
    let roots = f.roots
    let x = gen(typeof f)
    result.unit = f.lc * x^0
    var f = f div result.unit
    for a in roots:
        var exp = 0
        while f(a) == zero(QQ):
            inc exp
            f = f div (x-a)
        if exp > 0:
            result.factors[(x-a)] = exp
    if deg(f) > 0:
        result.factors[f] = 1
    

when isMainModule:
    echo int.zero
    type R = PR(ZZ,x)
    type S = PR(ZZ,y)
    type S2 = PR(QQ,q)
    let f = 1 + x^2 + x
    echo f
    echo f("w")
    echo f(2)
    echo f(x^2)
    echo f(y)
    echo R
    dump 6//11*q^2-7//1*q^3+1//1*q^5 - q^5
    dump roots(x^5 - 7*x^3 + 6*x^2)
    dump roots(3//1*q^3 - 5//1*q^2 + 5//1* q - 2//1)
    dump factor((q^5 - 7//1*q^3 + 6//1*q^2)*(q^2 + 1//1))
    
    