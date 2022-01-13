include prelude
import numbers
import sugar, macros, math
import rings
import typetraits

type FactorRing*[M:static auto] = object #Just polynomial rings for now?
    val:typeof(M)

type Ideal*[P] = object
    generator*: P

func I*[P](p:P):Ideal[P] =
    result.generator = p
func `$`*(p:Ideal):string =
    "I(" & $p.generator & ")"

template `+`*[TT](poly:TT, M:static[Ideal[TT]]):FactorRing[M] =
    debugEcho "here"
    FactorRing[M.generator](val:poly)

func `+++`*[TT](poly:TT, M:static TT):FactorRing[M] =
    debugEcho "heree"
    FactorRing[M](val:poly)

func `+`*[TT; M:static[TT]](f,g:FactorRing[M]):FactorRing[M] =
    result.val = f.val + g.val

func `$`*[TT; M:static[TT]](f:FactorRing[M]):string =
    $f.val & " + I(" & $M & ")"

when isMainModule:
    type R = PR(ZZ,x)
    static: echo (x^3).coeffs
    let a = x +++ (x^3)
    #let b = x+1 + I(x^3)
    #echo a+b
    echo I(x^2)