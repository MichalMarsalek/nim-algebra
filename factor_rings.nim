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
func `*`*[P](p:P, _:typedesc[P]):Ideal[P] =
    I(p)
func `$`*(p:Ideal):string =
    "I(" & $p.generator & ")"


func `+`*[TT](poly:TT, M:static[Ideal[TT]]):auto =
    FactorRing[TT,M.generator](val: poly)

func `+`*[TT, M](f,g:FactorRing[TT,M]):FactorRing[TT, M] =
    result.val = (f.val + g.val) mod M

func `$`*[TT, M](f:FactorRing[TT, M]):string =
    $f.val & " + I(" & $M & ")"

when isMainModule:
    type R = PR(ZZ,x)
    let b = x+1 + (x^3)*R
    echo b