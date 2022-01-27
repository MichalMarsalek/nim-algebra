include prelude
import numbers, finite_fields
import sugar, macros, math, algorithm
import factorisations
import errors
{.experimental: "callOperator".}

type PolynomialRing*[TT; V:static string] = object
    coeffs*:seq[TT]

func `$`*(T:typedesc[PolynomialRing]):string =
  result = $T.TT
  if result.endsWith "]":
    result = result[0..^2] & "," & T.V & "]"
  else:
    result &= "+[" & T.V & "]"

func zero*[TT;V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    PolynomialRing[TT, V](coeffs: @[zero(typeof TT)])
func one*[TT; V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    PolynomialRing[TT, V](coeffs: @[one(typeof TT)])

func gen*[TT; V:static string](R:typedesc[PolynomialRing[TT, V]]):R =
    PolynomialRing[TT, V](coeffs: @[zero(typeof TT), one(typeof TT)])

#Type construction macros
macro `+`*(R:typedesc,variables:untyped{nkBracket}):typedesc[PolynomialRing] =
    result = nnkStmtList.newTree
    var prevTypeSym, currTypeSym: NimNode
    prevTypeSym = R
    for vari in variables:
        currTypeSym = genSym(nskType, "polyring")
        #define the type
        result.add(
            nnkTypeSection.newTree(nnkTypeDef.newTree(
              currTypeSym,
              newEmptyNode(),
              nnkBracketExpr.newTree(
                newIdentNode("PolynomialRing"),
                prevTypeSym,
                newLit($vari)
              )
            ))            
        )
        #introduce the generator
        if vari.kind == nnkIdent:
            result.add(nnkWhenStmt.newTree(
                nnkElifBranch.newTree(
                  nnkPrefix.newTree(
                    newIdentNode("not"),
                    nnkCall.newTree(
                      newIdentNode("declared"),
                      newIdentNode($vari)
                    )
                  ),
                  nnkConstSection.newTree(nnkConstDef.newTree(
                    newIdentNode($vari),
                    newEmptyNode(),
                    nnkDotExpr.newTree(
                      currTypeSym,
                      newIdentNode("gen")
                    )
                  ))
                )
            ))
        prevTypeSym = currTypeSym
    result.add prevTypeSym

func ConstantPoly*[TT,V](a:TT):PolynomialRing[TT,V] =
    PolynomialRing[TT, V](coeffs: @[a])

func deg*[TT,V](f: PolynomialRing[TT,V]):int {.inline.} =
    f.coeffs.len - 1

func lc*[TT,V](f: PolynomialRing[TT,V]):TT {.inline.} =
    f.coeffs[^1]
func cc*[TT,V](f: PolynomialRing[TT,V]):TT {.inline.} =
    f.coeffs[0]

iterator pairs*[TT,V](f: PolynomialRing[TT,V]): (int,TT) =
    for i,fi in f.coeffs:
        yield (i,fi)


func `==`*[TT,V](f,g: PolynomialRing[TT,V]):bool =
    f.coeffs == g.coeffs
func `!=`*[TT,V](f,g: PolynomialRing[TT,V]):bool =
    f.coeffs != g.coeffs

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
    parts.reversed.join(" + ").replace("+ -","- ")

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
    normalize result
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

func `()`*[TT,V1](f:PolynomialRing[TT,V1], V2: static string):PolynomialRing[TT,V2] =
    PolynomialRing[TT,V2](f)

func `()`*[TT,V,TT2](f:PolynomialRing[TT,V], val: TT2):TT2 =
    result = zero(TT2)
    for i in countdown(deg f, 0):
        result = result * val + f.coeffs[i]

func derivative*[TT,V](f:PolynomialRing[TT,V]):PolynomialRing[TT,V] =
    for i in 1..deg(f):
        result.coeffs.add f.coeffs[i] * (i-1)

#ROOTS & FACTORING

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

func roots*[TT:FiniteField, V](f:PolynomialRing[TT,V]):seq[TT] =
    for x in TT:
        if f(x) == TT.zero:
            result.add x
func roots*[V](f:PolynomialRing[QQ,V]):seq[QQ] =
    mixin `//`
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

func roots*[V](f:PolynomialRing[RR,V]):seq[RR] =
    import math
    if f.deg == -1:
        raiseInfiniteError(RR)
    elif f.deg == 0:
        return
    elif f.deg == 1:
        return -f.cc / f.lc
    elif f.deg == 2:
        let (a,b,c) = (f.entries[2], f.entries[1], f.entries[0])
        let D = b*b - 4*a*c
        if D < 0.0: return
        if D == 0.0: return @[-b/(2.0*a)]
        let sqrtD = sqrt D
        return @[(-b+sqrtD)/(2.0*a), (-b-sqrtD)/(2.0*a)]
    else:
        assert false, "roots implemented for quadratic real polynomials"
func roots*[V](f:PolynomialRing[CC,V]):seq[CC] =
    import complex
    if f.deg == -1:
        raiseInfiniteError(CC)
    elif f.deg == 0:
        return
    elif f.deg == 1:
        return -f.cc / f.lc
    elif f.deg == 2:
        let (a,b,c) = (f.entries[2], f.entries[1], f.entries[0])
        let D = b*b - 4*a*c
        if D == complex(0.0, 0.0): return @[-b/(2.0*a)]
        let sqrtD = sqrt D
        return @[(-b+sqrtD)/(2.0*a), (-b-sqrtD)/(2.0*a)]
    else:
        assert false, "roots implemented for quadratic complex polynomials"

func factor*[TT,V](f:PolynomialRing[TT,V]):Factorisation[typeof f] =
    #TODO this only factors out linear factors
    let roots = f.roots
    let x = gen(typeof f)
    result.unit = f.lc * x^0
    var f = f div result.unit
    for a in roots:
        var exp = 0
        while f(a) == zero(TT):
            inc exp
            f = f div (x-a)
        if exp > 0:
            result.factors[(x-a)] = exp
    if deg(f) > 0:
        result.factors[f] = 1

#RANDOM & ITERATORS
iterator items*[TT,V](R:typedesc[PolynomialRing[TT,V]]):R =
    let base = toSeq items TT
    var it = R.zero
    var indeces = @[0]
    while true:
        yield it
        inc indeces[0]
        var i = 0
        while i+1 < indeces.len and indeces[i] == base.len:
            indeces[i] = 0
            it.coeffs[i] = base[0]
            inc indeces[i+1]
            inc i
        if indeces[^1] == base.len:
            indeces[^1] = 0
            indeces.add 1
            it.coeffs.add base[1]
        it.coeffs[i] = base[indeces[i]]