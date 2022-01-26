include prelude
import sugar, macros, math

type QuadraticExtension*[T; D:static T] = object
    x: T
    y: T

template zero*[T,D](R:typedesc[QuadraticExtension[T,D]]):R =
    result.x = T.zero
    result.y = T.zero
template one*[T,D](R:typedesc[QuadraticExtension[T,D]]):R =
    result.x = T.one
    result.y = T.zero
template sqrt*[T](a:static TT):QuadraticExtension[T,a] =
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


func `+`*[T,D](a,b:QuadraticExtension[T,D]):QuadraticExtension[T,D] =
    result.x = a.x + b.x
    result.y = a.y + b.y
func `+=`*[T,D](a:var QuadraticExtension[T,D], b:QuadraticExtension[T,D]):QuadraticExtension[T,D] =
    a = a + b
func `-`*[T,D](a,b:QuadraticExtension[T,D]):QuadraticExtension[T,D]  =
    result.x = a.x - b.x
    result.y = a.y - b.y
func `-=`*[T,D](a:var QuadraticExtension[T,D], b:QuadraticExtension[T,D]):QuadraticExtension[T,D] =
    a = a - b
func `-`*[T,D](a:QuadraticExtension[T,D]):QuadraticExtension[T,D]  =
    result.x = -a.x
    result.y = -a.y
func `*`*[T,D](a,b:QuadraticExtension[T,D]):QuadraticExtension[T,D]  =
    result.x = a.x * b.x + D * a.y * b.y
    result.y = a.y * b.x + a.x * b.y
func `*=`*[T,D](a:var QuadraticExtension[T,D], b:QuadraticExtension[T,D]):QuadraticExtension[T,D] =
    a = a * b
func inv*[T,D](a:QuadraticExtension[T,D]):QuadraticExtension[T,D]  =
    let den = a.x*a.x - a.y*a.y*D
	result.x = a.x / den
	result.y = -b.x / den
