include prelude
include numbers
import sugar, macros, rings
{.experimental: "callOperator".}

#generated using SageMath
const SMALL_BIN_IRRED_POLYS = [0'u64,0'u64,0x3'u64,0x3'u64,0x3'u64,0x5'u64,0x1b'u64,0x3'u64,0x1d'u64,0x11'u64,0x6f'u64,0x5'u64,0xeb'u64,0x1b'u64,0xa9'u64,0x35'u64,0x2d'u64,0x9'u64,0x1403'u64,0x27'u64,0x6f3'u64,0x65'u64,0x1f61'u64,0x21'u64,0x1e6a9'u64,0x145'u64,0x45d3'u64,0x16ad'u64,0x20e5'u64,0x5'u64,0x328af'u64,0x9'u64,0x8299'u64,0x3d49'u64,0x199f7'u64,0xca5'u64,0xda6163'u64,0x3f'u64,0x4727'u64,0x9ee5'u64,0xa5b12b'u64,0x9'u64,0x47141a67'u64,0x59'u64,0x10b001b'u64,0x12d841'u64,0xb24001'u64,0x21'u64,0x2821d89'u64,0x55f'u64,0x380b7755'u64,0x19241'u64,0x1ea2c493'u64,0x47'u64,0x5ea27a097'u64,0xe91'u64,0x244486b1d'u64,0x292d7f'u64,0xa7451deb'u64,0x7b'u64,0x3697464a113d'u64,0x27'u64,0x17f3f7043'u64,0x1c38b1f'u64,0x247f43cb7'u64]

type SmallBinaryField[DEG: static int,
                      MOD: static uint64,
                      V: static string="α"] =
  distinct uint64
type FiniteField = SmallBinaryField # | GeneralFiniteField

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

import algos
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

func `$`[DEG,MOD](a: SmallBinaryField[DEG,MOD]):string =
    a.uint64.int.toBin(DEG) #temp use the polynomial repr for general output

type Field* = QQ | RR | CC | FiniteField

template GF(characteristic,degree:int,variable="α"):typedesc =
    assert characteristic == 2
    assert degree <= 64
    SmallBinaryField[degree, SMALL_BIN_IRRED_POLYS[degree]]

when isMainModule:
    type F = GF(2,4)
    let a:F = F 0b111 #α^2 + α + 1
    let b:F = F 0b1011 #α^3 + α + 1
    echo a/b