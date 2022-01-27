include prelude
include complex
include integers, fractions, factor_rings
import sugar

type RR* = float   #this is only temporary
type CC* = Complex[float] #this is only temporary

template zero*(_:typedesc[RR]):RR = 0.0
template one*(_:typedesc[RR]):RR = 1.0
template zero*(_:typedesc[CC]):CC = Complex(0.0,0.0)
template one*(_:typedesc[CC]):CC = Complex(1.0,0.0)

type QQ* = ZZ/ZZ
func `$`*(_:typedesc[QQ]):string = "QQ"

include quadratic_extensions

type ZZMod*[M: static ZZ] = ZZ/M
type intMod*[M: static int] = int/M

func `/`*[D](_,x:typedesc[ZZQ[D]]):typedesc =
   QQQ[D]

type Number* = int | ZZ | QQ | RR | CC | ZZi | QQi #TODO