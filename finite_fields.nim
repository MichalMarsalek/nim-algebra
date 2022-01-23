include prelude
import numbers
import math
import sugar, macros
import algos
import random
{.experimental: "callOperator".}

#Conway polynomials:
const SMALL_BIN_IRRED_POLYS = [0'u64,0'u64,0x3'u64,0x3'u64,0x3'u64,0x5'u64,0x1b'u64,0x3'u64,0x1d'u64,0x11'u64,0x6f'u64,0x5'u64,0xeb'u64,0x1b'u64,0xa9'u64,0x35'u64,0x2d'u64,0x9'u64,0x1403'u64,0x27'u64,0x6f3'u64,0x65'u64,0x1f61'u64,0x21'u64,0x1e6a9'u64,0x145'u64,0x45d3'u64,0x16ad'u64,0x20e5'u64,0x5'u64,0x328af'u64,0x9'u64,0x8299'u64,0x3d49'u64,0x199f7'u64,0xca5'u64,0xda6163'u64,0x3f'u64,0x4727'u64,0x9ee5'u64,0xa5b12b'u64,0x9'u64,0x47141a67'u64,0x59'u64,0x10b001b'u64,0x12d841'u64,0xb24001'u64,0x21'u64,0x2821d89'u64,0x55f'u64,0x380b7755'u64,0x19241'u64,0x1ea2c493'u64,0x47'u64,0x5ea27a097'u64,0xe91'u64,0x244486b1d'u64,0x292d7f'u64,0xa7451deb'u64,0x7b'u64,0x3697464a113d'u64,0x27'u64,0x17f3f7043'u64,0x1c38b1f'u64,0x247f43cb7'u64]
include conway_polys

type BinaryField*[DEG: static int,
                      MOD: static uint64,
                      V: static string="α"] =
  distinct uint64

type GenFiniteField*[P: static int,
                      DEG: static int,
                      MOD: static array[DEG,int],
                      V: static string="α"] = object
  coeffs: array[DEG,int]

type FiniteField* = concept type T
    T is BinaryField or T is GenFiniteField #| PrimeField

#BINARY FIELD
func `$`*[DEG, MOD, V](R:typedesc[BinaryField[DEG, MOD, V]]):string =
    "GF(2^" & $DEG & (if V != "α": ", " & V else: "") & ")"

template zero*[DEG, MOD, V](R:typedesc[BinaryField[DEG, MOD, V]]):R = R 0'u64
template one*[DEG, MOD, V](R:typedesc[BinaryField[DEG, MOD, V]]):R = R 1'u64
template gen*[DEG, MOD, V](R:typedesc[BinaryField[DEG, MOD, V]]):R = R 2'u64

proc random*[DEG,MOD,V](R:typedesc[BinaryField[DEG,MOD,V]]):R =
    const mx = (1 shl DEG) - 1
    R rand(mx)

iterator items*[DEG,MOD,V](F:typedesc[BinaryField[DEG,MOD,V]]):F =
    for i in 0..<(1 shl DEG):
        yield F i

iterator nonzero*[DEG,MOD,V](F:typedesc[BinaryField[DEG,MOD,V]]):F =
    for i in 1..<(1 shl DEG):
        yield F i

func `==`*[DEG,MOD, V](a,b:BinaryField[DEG,MOD, V]):bool {.inline.} =
    a.uint64 == b.uint64
func `!=`*[DEG,MOD, V](a,b:BinaryField[DEG,MOD, V]):bool {.inline.} =
    a.uint64 != b.uint64
func `+`*[DEG,MOD, V](a,b:BinaryField[DEG,MOD, V]):BinaryField[DEG,MOD, V] {.inline.} =
    BinaryField[DEG,MOD, V] (a.uint64 xor b.uint64)
func `+=`*[DEG,MOD, V](a:var BinaryField[DEG,MOD, V],b:BinaryField[DEG,MOD, V]) {.inline.} =
    a = a + b
func `-`*[DEG,MOD, V](a,b:BinaryField[DEG,MOD, V]):BinaryField[DEG,MOD, V] {.inline.} =
    a + b
func `-=`*[DEG,MOD, V](a:var BinaryField[DEG,MOD, V],b:BinaryField[DEG,MOD, V]) {.inline.} =
    a = a - b
func `-`*[DEG,MOD, V](a:  BinaryField[DEG,MOD, V]):BinaryField[DEG,MOD, V] {.inline.} =
    a
func `*`*[DEG,MOD, V](a,b:BinaryField[DEG,MOD, V]):BinaryField[DEG,MOD, V] =
    const overflowCheck = 1 shl (DEG-1)
    var a = a.uint64
    var b = b.uint64
    if b > a: swap(a,b)
    while b > 0:
        if b mod 2 == 1:
            result += BinaryField[DEG,MOD, V] a
        b = b div 2
        if a >= overflowCheck:
            a = (a xor overflowCheck) shl 1 xor MOD
        else:
            a = a shl 1
func `*=`*[DEG,MOD, V](a:var BinaryField[DEG,MOD, V],b:BinaryField[DEG,MOD, V]) {.inline.} =
    a = a * b

func `^`*[DEG,MOD, V](a:BinaryField[DEG,MOD, V],exp:int):BinaryField[DEG,MOD, V] =
    binaryExponentiation(a, exp)

func `/`*[DEG,MOD, V](a,b:BinaryField[DEG,MOD, V]):BinaryField[DEG,MOD, V] {.inline.} =
    result = a
    var b = b
    for i in 2..DEG:
        b *= b
        result *= b
func `/=`*[DEG,MOD, V](a:var BinaryField[DEG,MOD, V],b:BinaryField[DEG,MOD, V]) {.inline.} =
    a = a / b

func `$`*[DEG,MOD,V](a: BinaryField[DEG,MOD,V]):string =
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

func trace*[DEG,MOD,V](a: BinaryField[DEG,MOD,V]):BinaryField[DEG,MOD,V] =
    var a = a
    result = a
    for i in 2..DEG:
        a = a*a
        result += a

func norm*[DEG,MOD,V](a: BinaryField[DEG,MOD,V]):BinaryField[DEG,MOD,V] =
    discard #TODO

#GENERAL FIELD

discard """
func `$`*[P, DEG, MOD, V](R:typedesc[GenFiniteField[P, DEG, MOD, V]]):string =
    "GF(" & $P & "^" & $DEG & ", " & $V & ")"
template zero*(R:typedesc[GenFiniteField]):R = discard
template one*(R:typedesc[GenFiniteField]):R = result.coeffs[0] = 1
template gen*(R:typedesc[GenFiniteField]):R = result.coeffs[1] = 1


proc random*[P,DEG,MOD,V](R:typedesc[GenFiniteField[P,DEG,MOD,V]]):R =
    for i in 0..<DEG:
        result.coeffs = random(ZZ/P)

func next*[P,DEG,MOD, V](a: GenFiniteField[P,DEG,MOD, V]):GenFiniteField[P,DEG,MOD, V] =
    result = a
    result.coeffs[0] += 1
    var i = 0
    while result.coeffs[i] == P:
        result.coeffs[i] = 0
        inc i
        result.coeffs[i] += 1

iterator items*[P,DEG,MOD, V](R:typedesc[GenFiniteField[P,DEG,MOD,V]]):R =
    var el = zero(R)
    for i in 1..R.card:
        yield el
        el = next el
iterator nonzero*[P,DEG,MOD, V](R:typedesc[GenFiniteField[P,DEG,MOD,V]]):R =
    var el = one(R)
    for i in 1..<R.card:
        yield el
        el = next el
iterator invertible*[P,DEG,MOD, V](R:typedesc[GenFiniteField[P,DEG,MOD,V]]):R =
    for e in R.nonzero:
        yield e

func `+=`[P,DEG,MOD, V](a:var GenFiniteField[P,DEG,MOD, V],b:GenFiniteField[P,DEG,MOD, V]) {.inline.} =
    for i in 0..<DEG:
        a.coeffs[i] = (a.coeffs[i] + b.coeffs[i]) mod P
        
func `+`[P,DEG,MOD, V](a,b:GenFiniteField[P,DEG,MOD, V]):GenFiniteField[P,DEG,MOD, V] {.inline.} =
    result = a
    result += b
func `-=`[P,DEG,MOD, V](a:var GenFiniteField[P,DEG,MOD, V],b:GenFiniteField[P,DEG,MOD, V]) {.inline.} =
    for i in 0..<DEG:
        a.coeffs[i] = (P + a.coeffs[i] - b.coeffs[i]) mod P
        
func `-`[P,DEG,MOD, V](a,b:GenFiniteField[P,DEG,MOD, V]):GenFiniteField[P,DEG,MOD, V] {.inline.} =
    result = a
    result += b
func `-`[P,DEG,MOD, V](a:  GenFiniteField[P,DEG,MOD, V]):GenFiniteField[P,DEG,MOD, V] {.inline.} =
    for i in 0..<DEG:
        result.coeffs[i] = (P - a.coeffs[i]) mod P


func `*`[P,DEG,MOD, V](a,b:GenFiniteField[P,DEG,MOD, V]):GenFiniteField[P,DEG,MOD, V] =
    discard #TODO
func `*=`[DEG,MOD, V](a:var GenFiniteField[DEG,MOD, V],b:GenFiniteField[DEG,MOD, V]) {.inline.} =
    a = a * b

func `^`[P,DEG,MOD, V](a:GenFiniteField[P,DEG,MOD, V],exp:int):GenFiniteField[P,DEG,MOD, V] =
    binaryExponentiation(a, exp)

func `/`[P,DEG,MOD, V](a,b:GenFiniteField[P,DEG,MOD, V]):GenFiniteField[P,DEG,MOD, V] =
    discard #TODO
func `/=`[P,DEG,MOD, V](a:var GenFiniteField[P,DEG,MOD, V],b:GenFiniteField[P,DEG,MOD, V]) {.inline.} =
    a = a / b
"""

func `$`[P,DEG,MOD,V](a: GenFiniteField[P,DEG,MOD,V]):string =
    var parts:seq[string]
    for i in countdown(DEG-1,2):
        if a.coeffs[i] > 1:
            parts.add $a.coeffs[i] & V & "^" & $i
        elif a.coeffs[i] > 0:
            parts.add V & "^" & $i
    if a.coeffs[1] > 1:
        parts.add $a.coeffs[1] & V
    elif a.coeffs[1] > 0:
        parts.add V
    if a.coeffs[0] > 0:
        parts.add $a.coeffs[0]
    if parts.len == 0: parts.add "0"
    parts.join(" + ")

type Field* = QQ | RR | CC | FiniteField

func factorPower*(c:int):(int,int) =
    var p,d:int
    var car = c
    for q in 2..car:
        if car mod q == 0:
            p = q
            while car > 1:
                car = car div p
                inc d
            break
        if q*q > car: break
    return (p,d)

macro GF*(cardinality:typed,variable="α"):typedesc =  
    # add injecting generator
    var base = cardinality
    var exponent = quote do: 1
    if cardinality.kind == nnkInfix:
        var base = cardinality[1]
        var exponent = cardinality[2]
    result = quote do:  #replace this with a faster algo          
        const (p,d) = factorPower `base`
        when p == 2:
            BinaryField[d*`exponent`, SMALL_BIN_IRRED_POLYS[d*`exponent`], `variable`]
        else:
            #type R = PR(ZZ/(p), x) circular dependency
            #R/(x^(d*`exponent`))
            int #placeholder
    #echo toStrLit result

when isMainModule:
    type F = GF(16,"α")
    echo F
    const x = F.gen
    let a:F = F 0b111 #α^2 + α + 1
    let b:F = F 0b1011 #α^3 + α + 1
    echo $a
    echo x^2 + x + F.one
    for e in GF(2^3,"X"):
        echo (e, trace e)