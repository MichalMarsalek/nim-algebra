include prelude
import numbers
import sugar, macros, math

type ZZQ[d:static int] = tuple[x,y:ZZ]
type ZZ_i = ZZQ[-1]
type QQQ[d:static int] = tuple[x,y:QQ]
type QQ_i = QQQ[-1]

# ZZQ[D] ARITMETHICS & PRINTING
template zero*[D](R:typedesc[ZZQ[D]]):R = discard
template one*[D](R:typedesc[ZZQ[D]]):R = (x:one(ZZ), y:zero(ZZ))
func `$`*[D](a:ZZQ[D]):string =
    when D == -1:
        return replace($a.x & " + " & $a.y & "i", "+ -", "- ")
    else:
        when D > 0:
            return replace($a.x & " + " & $a.y & "sqrt" & $D, "+ -", "- ")
        else:
            return replace($a.x & " + " & $a.y & "sqrt(" & $D & ")", "+ -", "- ")

macro sqrt*(a:untyped{lit}):untyped = #TODO temporary
    quote do:
        (x:zero(typeof(`a`)), y:`a`)

func `+`*[D:static ZZ](a,b:ZZQ[D]):ZZQ[D] =
    result.x = a.x + b.x
    result.y = a.y + b.y
func `+=`*[D:static ZZ](a: var ZZQ[D], b: ZZQ[D]) =
    a = a + b
func `-`*[D:static ZZ](a,b:ZZQ[D]):ZZQ[D] =
    result.x = a.x - b.x
    result.y = a.y - b.y
func `-=`*[D:static ZZ](a: var ZZQ[D], b: ZZQ[D]) =
    a = a - b
func `-`*[D:static ZZ](a:ZZQ[D]):ZZQ[D] =
    result.x = -a.x
    result.y = -a.y
func `*`*[D:static ZZ](a,b:ZZQ[D]):ZZQ[D] =
    result.x = a.x * b.x + D * a.y * b.y
    result.y = a.y * b.x + a.x * b.y
func `*=`*[D:static ZZ](a: var ZZQ[D], b: ZZQ[D]) =
    a = a * b
    

#TODO - replace with embedings
func `+`*[D:static ZZ](a:ZZ, b:ZZQ[D]):ZZQ[D] =
    result.x = a + b.x
    result.y = b.y
func `-`*[D:static ZZ](a:ZZ, b:ZZQ[D]):ZZQ[D] =
    result.x = a - b.x
    result.y = b.y


if isMainModule:
    let a = 2 + sqrt(5)
    let b = 3 - sqrt(5)
    echo a
