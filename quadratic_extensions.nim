include prelude
import sugar, macros, math

type QuadraticExtension*[T; D:static T] = object
    x: T
    y: T

# ZZQ[D] ARITMETHICS & PRINTING
template zero*[T,D](R:typedesc[QuadraticExtension[T,D]]):R =
    result.x = T.zero
    result.y = T.zero
template one*[T,D](R:typedesc[QuadraticExtension[T,D]]):R =
    result.x = T.one
    result.y = T.zero
template sqrt*[T](a:static TT):QuadraticExtension[T,a] =
    result.x = T.zero
    result.y = T.one
    
#TODO rest of file

func `$`*[D:static ZZ](a:ZZQ[D]):string =
    when D == -1:
        return replace($a.x & " + " & $a.y & "i", "+ -", "- ")
    else:
        when D > 0:
            return replace($a.x & " + " & $a.y & "√" & $D, "+ -", "- ")
        else:
            return replace($a.x & " + " & $a.y & "√(" & $D & ")", "+ -", "- ")

func sqrt*(a:static ZZ):ZZQ[a] =
    result.x = zero(ZZ)
    result.y = one(ZZ)

func `+`*[D](a,b:ZZQ[D]):ZZQ[D] =
    result.x = a.x + b.x
    result.y = a.y + b.y
func `+=`*[D](a: var ZZQ[D], b: ZZQ[D]) =
    a = a + b
func `-`*[D](a,b:ZZQ[D]):ZZQ[D] =
    result.x = a.x - b.x
    result.y = a.y - b.y
func `-=`*[D](a: var ZZQ[D], b: ZZQ[D]) =
    a = a - b
func `-`*[D](a:ZZQ[D]):ZZQ[D] =
    result.x = -a.x
    result.y = -a.y
func `*`*[D](a,b:ZZQ[D]):ZZQ[D] =
    result.x = a.x * b.x + D * a.y * b.y
    result.y = a.y * b.x + a.x * b.y
func `*=`*[D](a: var ZZQ[D], b: ZZQ[D]) =
    a = a * b
func `^`*[D](a:ZZQ[D], exp:int):ZZQ[D] =
    binaryExponentiation(a, exp)
    

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
    echo a*b
    echo sqrt(-1)
