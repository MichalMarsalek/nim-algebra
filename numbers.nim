include rationals, complex

type ZZ = int     #this is only temporary
type RR = float   #this is only temporary
type CC = Complex[float] #this is only temporary
type QQ = Rational[ZZ]
type Number = ZZ | QQ | RR | CC

type ZZMod*[M: static ZZ] = distinct ZZ

template `/`(T:typedesc[ZZ],m:int):typedesc =
    ZZMod[m]

template zero*[M](R:typedesc[ZZMod[M]]):R =
    R 0
template one*[M](R:typedesc[ZZMod[M]]):R =
    R 1

func `$`[M](a: ZZMod[M]):string =
    $a.ZZ

func `+`[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    ZZMod[M]((a.ZZ + b.ZZ) mod M)
func `+=`[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a + b
func `-`[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    ZZMod[M]((a.ZZ - b.ZZ) mod M)
func `-=`[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a - b
func `*`[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    ZZMod[M]((a.ZZ * b.ZZ) mod M)
func `*=`[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
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

include algos
func `^`*[M](a:ZZMod[M], exp:int): ZZMod[M] =
    if exp < 0:
        binaryExponentiation(a.inv, -exp)
    else:
        binaryExponentiation(a, exp)

func `/`[M](a,b: ZZMod[M]):ZZMod[M] {.inline.} =
    a * b.inv
func `/=`[M](a: var ZZMod[M], b: ZZMod[M]) {.inline.} =
    a = a * b.inv

when isMainModule:    
    type ZZ5 = ZZ/(5)
    let a = ZZ5 3
    let b = ZZ5 4
    echo a + b
    echo inv a
    echo a^ -1
    echo ZZ5(4)^1001