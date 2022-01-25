include prelude, complex, integers
import fractions, factor_rings, quadratic_extensions
import sugar
import algos

type RR* = float   #this is only temporary
type CC* = Complex[float] #this is only temporary

template zero*(_:typedesc[RR]):RR = 0.0
template one*(_:typedesc[RR]):RR = 1.0
template zero*(_:typedesc[CC]):CC = Complex(0.0,0.0)
template one*(_:typedesc[CC]):CC = Complex(1.0,0.0)

type QQ* = ZZ/ZZ

type ZZMod*[M: static ZZ] = ZZ/M
type intMod*[M: static int] = int/M

type ZZQ*[D:static ZZ] = QuadraticExtension[ZZ, D]
type ZZ_i* = ZZQ[initZZ(-1)]
type QQQ*[D:static QQ] = QuadraticExtension[QQ, D]
type QQ_i* = QQQ[initFrac(initZZ(-1),initZZ(-1))]

func `/`*[D](_,_:typedesc[ZZQ[D]]):typedesc =
   QQQ[D]

type Number* = int | ZZ | QQ | RR | CC | ZZi | QQi #TODO

when isMainModule: 
    block:
        type ZZ5 = ZZ/(5)
        echo ZZ5
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
        echo factor -750
        dump isPrime -7
        dump isPrime -9
        dump isPrime 1
        for p in ZZ.primes:
            echo p
            if p > 50: break
        dump phi 15
        dump toSeq (ZZ/15).invertible