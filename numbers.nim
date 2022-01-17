include rationals, complex
import sugar
import random, algorithm

type ZZ* = int     #this is only temporary
type RR* = float   #this is only temporary
type CC* = Complex[float] #this is only temporary
type QQ* = Rational[ZZ]
type Number* = ZZ | QQ | RR | CC

type ZZMod*[M: static ZZ] = distinct ZZ

template `/`*(T:typedesc[ZZ],m:int):typedesc =
    ZZMod[m]

template zero*(_:typedesc[ZZ]):ZZ = 0
template one*(_:typedesc[ZZ]):ZZ = 1
template zero*(_:typedesc[RR]):RR = 0.0
template one*(_:typedesc[RR]):RR = 1.0
template zero*(_:typedesc[CC]):CC = Complex(0.0,0.0)
template one*(_:typedesc[CC]):CC = Complex(1.0,0.0)
template zero*(_:typedesc[QQ]):QQ = 0//1
template one*(_:typedesc[QQ]):QQ = 1//1

# ZZ
func gcd(x,y:ZZ):ZZ =
    #TODO implement binary version
    var x = abs x
    var y = abs y
    while y > 0:
        (x, y) = (y, x mod y)
    x

iterator positive*(_:typedesc[ZZ]):ZZ =
    for i in 1..<int.high:
        yield ZZ i

iterator divisors*(a:ZZ):ZZ =
    var rest:seq[ZZ]
    for i in 1..a:
        let i_squared = i*i
        if i_squared == a: yield ZZ i
        if i_squared >= a: break
        yield ZZ i
        rest.add ZZ (a div ZZ(i))
    for x in rest.reversed:
        yield x
            

# ZZ/(n)
template zero*[M](R:typedesc[ZZMod[M]]):R =
    R 0
template one*[M](R:typedesc[ZZMod[M]]):R =
    R 1

func `$`*[M](a: ZZMod[M]):string =
    $a.ZZ

func `+`*[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    ZZMod[M]((a.ZZ + b.ZZ) mod M)
func `+=`*[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a + b
func `-`*[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    ZZMod[M]((a.ZZ - b.ZZ) mod M)
func `-=`*[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a - b
func `*`[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    ZZMod[M]((a.ZZ * b.ZZ) mod M)
func `*=`*[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a * b

func inv*[M](a0: ZZMod[M]): ZZMod[M] = ##TODO make this use ZZ
    var (a, b, x0) = (a0.int, M, 0)
    var res = 1
    while a > 1:
        res = res - (a div b) * x0
        a = a mod b
        swap a, b
        swap x0, res
    if res < 0: res += M
    return ZZMod[M] res

func `^`*[M](a:ZZMod[M], exp:int): ZZMod[M] =
    func binaryExponentiation[T](value:T, exp:int):T =
        result = one(T)
        var intermediate = value
        var exp1 = exp
        while exp1 > 0:
            if exp1 mod 2 == 1:
                result *= intermediate
            intermediate *= intermediate
            exp1 = exp1 shr 1
    if exp < 0:
        binaryExponentiation(a.inv, -exp)
    else:
        binaryExponentiation(a, exp)

func `/`*[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    a * b.inv
func `/=`*[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a * b.inv

func random*[M](R:typedesc[ZZMod[M]]):R =
    const mx = M-1
    R rand(mx)

iterator items*[M](R:typedesc[ZZMod[M]]):R =
    for i in 0..<M:
        yield R i
iterator nonzero*[M](R:typedesc[ZZMod[M]]):R =
    for i in 1..<M:
        yield R i
iterator invertible*[M](R:typedesc[ZZMod[M]]):R =
    for i in 1..<M:
        if gcd(M,i) == 1:
            yield R i


when isMainModule: 
    block:
        type ZZ5 = ZZ/(5)
        let a = ZZ5 3
        #let a= 3 + 5*ZZ
        let b = ZZ5 4
        echo a + b
        echo inv a
        echo a^ -1
        echo ZZ5(4)^1001
        dump gcd(6,21)
        for i in invertible ZZ/12:
            echo i
        for i in 24.divisors:
            echo i