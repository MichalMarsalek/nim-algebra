include prelude
import sugar, macros, math

type QuadraticExtension*[T; D:static T] = object
    x: T
    y: T

type ZZQ*[D:static ZZ] = QuadraticExtension[ZZ, D]
type ZZ_i* = ZZQ[initZZ(-1)]
type QQQ*[D:static QQ] = QuadraticExtension[QQ, D]
type QQ_i* = QQQ[initFrac(initZZ(-1),initZZ(-1))]


template zero*[T,D](R:typedesc[QuadraticExtension[T,D]]):R =
    result.x = T.zero
    result.y = T.zero
template one*[T,D](R:typedesc[QuadraticExtension[T,D]]):R =
    result.x = T.one
    result.y = T.zero
template sqrt*[T](a:static[T]):QuadraticExtension[T,a] =
    result.x = T.zero
    result.y = T.one
    
func `$`*[T,D](a:QuadraticExtension[T,D]):string =
    when D == -T.one:
        return replace($a.x & " + " & $a.y & "i", "+ -", "- ")
    else:
        when len $D > 1:
            return replace($a.x & " + " & $a.y & "√(" & $D & ")", "+ -", "- ")
        else:
            return replace($a.x & " + " & $a.y & "√" & $D, "+ -", "- ")



func conjugate*[T,D](a:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    result.x = a.x
    result.y = -a.y

func norm*[T,D](a:QuadraticExtension[T,D]):T {.inline.} =
    a.x*a.x - a.y*a.y*D
func norm*(a:ZZi):ZZ {.inline.} =
    a.x*a.x + a.y*a.y
func norm*(a:QQi):QQ {.inline.} =
    a.x*a.x + a.y*a.y

func `+`*[T,D](a,b:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    result.x = a.x + b.x
    result.y = a.y + b.y
func `+=`*[T,D](a:var QuadraticExtension[T,D], b:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    a = a + b
func `-`*[T,D](a,b:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    result.x = a.x - b.x
    result.y = a.y - b.y
func `-=`*[T,D](a:var QuadraticExtension[T,D], b:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    a = a - b
func `-`*[T,D](a:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    result.x = -a.x
    result.y = -a.y
func `*`*[T,D](a,b:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    result.x = a.x * b.x + D * a.y * b.y
    result.y = a.y * b.x + a.x * b.y
func `*=`*[T,D](a:var QuadraticExtension[T,D], b:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    a = a * b
func inv*[T,D](a:QuadraticExtension[T,D]):QuadraticExtension[T,D] {.inline.} =
    let den = a.norm
    result.x = a.x / den
    result.y = -a.y / den
