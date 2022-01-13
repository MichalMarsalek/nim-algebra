include prelude
import numbers
import math
import sugar, macros, rings
{.experimental: "callOperator".}

#generated using SageMath
const SMALL_BIN_IRRED_POLYS = [0'u64,0'u64,0x3'u64,0x3'u64,0x3'u64,0x5'u64,0x1b'u64,0x3'u64,0x1d'u64,0x11'u64,0x6f'u64,0x5'u64,0xeb'u64,0x1b'u64,0xa9'u64,0x35'u64,0x2d'u64,0x9'u64,0x1403'u64,0x27'u64,0x6f3'u64,0x65'u64,0x1f61'u64,0x21'u64,0x1e6a9'u64,0x145'u64,0x45d3'u64,0x16ad'u64,0x20e5'u64,0x5'u64,0x328af'u64,0x9'u64,0x8299'u64,0x3d49'u64,0x199f7'u64,0xca5'u64,0xda6163'u64,0x3f'u64,0x4727'u64,0x9ee5'u64,0xa5b12b'u64,0x9'u64,0x47141a67'u64,0x59'u64,0x10b001b'u64,0x12d841'u64,0xb24001'u64,0x21'u64,0x2821d89'u64,0x55f'u64,0x380b7755'u64,0x19241'u64,0x1ea2c493'u64,0x47'u64,0x5ea27a097'u64,0xe91'u64,0x244486b1d'u64,0x292d7f'u64,0xa7451deb'u64,0x7b'u64,0x3697464a113d'u64,0x27'u64,0x17f3f7043'u64,0x1c38b1f'u64,0x247f43cb7'u64]

type SmallBinaryField[DEG: static int,
                      MOD: static uint64,
                      V: static string="α"] =
  distinct uint64

type SmallFiniteField[P: static int,
                      DEG: static int,
                      MOD: static array[DEG,int],
                      V: static string="α"] =
  distinct array[DEG,int]

type FiniteField = SmallBinaryField | FiniteField

func `+`[DEG,MOD](a,b:SmallBinaryField[DEG,MOD]):SmallBinaryField[DEG,MOD] {.inline.} =
    SmallBinaryField[DEG,MOD] (a.uint64 xor b.uint64)
func `+=`[DEG,MOD](a:var SmallBinaryField[DEG,MOD],b:SmallBinaryField[DEG,MOD]) {.inline.} =
    a = a + b
func `-`[DEG,MOD](a,b:SmallBinaryField[DEG,MOD]):SmallBinaryField[DEG,MOD] {.inline.} =
    a + b
func `-`[DEG,MOD](a:  SmallBinaryField[DEG,MOD]):SmallBinaryField[DEG,MOD] {.inline.} =
    a
func `*`[DEG,MOD](a,b:SmallBinaryField[DEG,MOD]):SmallBinaryField[DEG,MOD] =
    const overflowCheck = 1 shl (DEG-1)
    var a = a.uint64
    var b = b.uint64
    if b > a: swap(a,b)
    while b > 0:
        if b mod 2 == 1:
            result += SmallBinaryField[DEG,MOD] a
        b = b div 2
        if a >= overflowCheck:
            a = (a xor overflowCheck) shl 1 xor MOD
        else:
            a = a shl 1
func `*=`[DEG,MOD](a:var SmallBinaryField[DEG,MOD],b:SmallBinaryField[DEG,MOD]) {.inline.} =
    a = a * b

include algos
func `^`[DEG,MOD](a:SmallBinaryField[DEG,MOD],exp:int):SmallBinaryField[DEG,MOD] =
    binaryExponentiation(a, exp)

func `/`[DEG,MOD](a,b:SmallBinaryField[DEG,MOD]):SmallBinaryField[DEG,MOD] {.inline.} =
    result = a
    var b = b
    for i in 2..DEG:
        b *= b
        result *= b
func `/=`[DEG,MOD](a:var SmallBinaryField[DEG,MOD],b:SmallBinaryField[DEG,MOD]) {.inline.} =
    a = a / b

func `$`[DEG,MOD,V](a: SmallBinaryField[DEG,MOD,V]):string =
    var parts:seq[string]
    for i in countdown(63,2):
        if ((a.uint64 shr i) and 1'u64) == 1'u64:
            parts.add V & "^" & $i
    if ((a.uint64 shr 1) and 1'u64) == 1'u64:
        parts.add V
    if (a.uint64 and 1'u64) == 1'u64:
        parts.add "1"
    if parts.len == 0: parts.add "0"
    parts.join(" + ")



type Field* = QQ | RR | CC | FiniteField

func factorPower(c:int):(int,int) =
    var p,d:int
    var car = c
    for q in 2..car:
        if q*q > car: break
        if car mod q == 0:
            p = q
            while car > 0:
                car = car div p
                inc d
            break
    return (p,d)

iterator items[DEG,MOD,V](F:typedesc[SmallBinaryField[DEG,MOD,V]]):F =
    for i in 0..<(1 shl (DEG-1)):
        yield F i

iterator nonzero[DEG,MOD,V](F:typedesc[SmallBinaryField[DEG,MOD,V]]):F =
    for i in 1..<(1 shl (DEG-1)):
        yield F i

macro GF(cardinality:typed,variable="α"):typedesc =  
    # add injecting generator
    var base = cardinality
    var exponent = quote do: 1
    if cardinality.kind == nnkInfix:
        var base = cardinality[1]
        var exponent = cardinality[2]
    result = quote do:  #replace this with a faster algo          
        const (p,d) = factorPower `base`
        when p == 2:
            SmallBinaryField[d*`exponent`, SMALL_BIN_IRRED_POLYS[d*`exponent`], `variable`]
        else:
            #type R = PR(ZZ/(p), x) circular dependency
            #R/(x^(d*`exponent`))
            int #placeholder
    echo toStrLit result

when isMainModule:
    type F = GF(16,"x")
    let a:F = F 0b111 #α^2 + α + 1
    let b:F = F 0b1011 #α^3 + α + 1
    echo a
    for e in GF(2^8,"X"):
        echo e