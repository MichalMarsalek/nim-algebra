# ZZ/(n)
template `/`*(T:typedesc[ZZ],m:int):typedesc =
    ZZMod[m]

func `$`*[M](R:typedesc[ZZMod[M]]):string =
    "ZZ/(" & $M & ")"
template zero*[M](R:typedesc[ZZMod[M]]):R =
    R 0
template one*[M](R:typedesc[ZZMod[M]]):R =
    R 1
func card*[M](_:typedesc[ZZMod[M]]):int =
    M

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

proc random*[M](R:typedesc[ZZMod[M]]):R =
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