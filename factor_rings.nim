include prelude
import numbers
import sugar, macros, math
import polynomials
import typetraits

type FactorRing*[TT;M:static TT] = object #Just polynomial rings for now?
    val:TT

type Ideal*[P] = object
    generator*: P

func I*[P](p:P):Ideal[P] =
    result.generator = p
func `$`*(p:Ideal):string =
    "I(" & $p.generator & ")"


template `+`*[TT](poly:TT, M:static[Ideal[TT]]):FactorRing[TT,M] =
    result.val = poly

{.push checks: off.}



func `+++`*[TT](poly:TT, M:static TT):FactorRing[TT, M] =
    result.val = poly

func `+`*[TT, M](f,g:FactorRing[TT,M]):FactorRing[TT, M] =
    mixin `+`, `mod`
    result.val = (f.val + g.val) mod M

{.pop.}
func `$`*[TT, M](f:FactorRing[TT, M]):string =
    $f.val & " + I(" & $M & ")"

when isMainModule:
    type R = PR(ZZ,x)
    const t = x^3
    let a = x +++ t
    #let b = x+1 + I(t)
    #echo a+b
    echo typeof(I(x^2))