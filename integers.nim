import algorithm, bitops, math, tables
import factorisations
import bigints
import errors
import times

type ZZ* = BigInt
template initZZ*(x:typed):ZZ =
    initBigInt(x)
template zero*(_:typedesc[ZZ]):ZZ = 0.initZZ
template one*(_:typedesc[ZZ]):ZZ = 1.initZZ
template zero*(_:typedesc[int]):int = 0
template one*(_:typedesc[int]):int = 1
func `$`*(_:typedesc[ZZ]):string = "ZZ"

#[
func inv(x:uint64):uint64 =
    ## Given odd x returns x^(-1) mod 2^64.
    result = x
    for _ in 1..5:
        result *= (2 - result*x)
func inv*(x:int):int =
    cast[int](inv cast[uint64](x))
func `*`*(x,y:int):int =
  int(cast[uint64](x)*cast[uint64](y))]#

#TODO do binary version of gcd for ints
func divmod*(a,b:int):(int,int) =
    (a div b, a mod b)

iterator positive*(_:typedesc[ZZ]):ZZ =
    for i in 1..<int.high:
        yield i.initZZ

proc lowEratosthenes*(): seq[int] =
  var sieve = newSeq[bool](500000)
  result.add 2
  for i in countup(3,999999,2):
    if not sieve[i div 2]:
      for m in countup(i*i,999999,2*i):
        sieve[m div 2] = true
      result.add i

const lowPrimes = lowEratosthenes()

iterator eratosthenes*(low, high:int):int =
  var low = low - low mod 2
  var sieve = newSeq[bool]((high-low) div 2)
  var lastPrime = 2
  for p in lowPrimes:
    if p*p > high: break
    if p == 2: continue
    for m in countup((low.ceilDiv(p) * p)-low, high-low-1,p):
      if m mod 2 == 1:
        sieve[m div 2] = true
    for i in countup(lastPrime*lastPrime,p*p,2):
      if i >= low:
        if not sieve[(i-low) div 2]:
          yield i
    lastPrime = p
  for i in countup(lastPrime*lastPrime, high-1, 2):
    if i >= low:
      if not sieve[(i-low) div 2]:
        yield i

iterator primes*(_:typedesc[ZZ], low:int):ZZ =
  var low = low
  while true:
    for p in eratosthenes(low, low+999999):
      yield p.initZZ
    low += 1000000
iterator primes*(_:typedesc[ZZ]):ZZ =
  for p in lowPrimes:
    yield p.initZZ
  for p in ZZ.primes(lowPrimes[^1]+1):
    yield p

func isPrime*(a:ZZ):bool =
    #TODO implement more efficient version
    var a = abs a
    for p in ZZ.primes:
        if p*p > a: break
        if a mod p == ZZ.zero:
            return false
    return true

iterator divisors*(a:ZZ):ZZ =
    var a = abs a
    var rest:seq[ZZ]
    for i in ZZ.positive:
        let i_squared = i*i
        if i_squared == a: yield ZZ i
        if i_squared >= a: break
        yield ZZ i
        rest.add ZZ (a div ZZ(i))
    for x in rest.reversed:
        yield x

func factor*(x:ZZ):Factorisation[ZZ] =
  if x == ZZ.zero: raise newException(Exception, "x = 0")
  result.unit = if x < 0.initZZ: -1.initZZ else: 1.initZZ
  var x = abs x
  for d in ZZ.primes:
    if d*d > x:
      result.factors[x] = 1
      break
    var exp = 0
    while x mod d == ZZ.zero:
      inc exp
      x = x div d
    if exp > 0:
      result.factors[d] = exp

func `^`(a:ZZ, b:int):ZZ =
    a.pow b
func `^`(a,b:ZZ) =
    discard #TODO

func phi*(a:ZZ):ZZ =
    result = ZZ.one
    for p,v in factor(a):
        result *= (p-1.initBigInt)*p^(v-1)



