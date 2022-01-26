import factorisations, algorithm, bitops, math, tables
import errors
import times

type ZZ* = int     #this is only temporary
template zero*(_:typedesc[ZZ]):ZZ = 0
template one*(_:typedesc[ZZ]):ZZ = 1

func inv*(x:int):int =
    ## Returns x^(-1) mod 2^64.
	if x mod 2 == 0:
		raiseInvertError(x)
    result = x
    for _ in 1..5:
        result *= (2 - result*x)

func gcd*(x,y:ZZ):ZZ =
    #TODO implement binary version
    var x = abs x
    var y = abs y
    while y > 0:
        (x, y) = (y, x mod y)
    x

iterator positive*(_:typedesc[ZZ]):ZZ =
    for i in 1..<int.high:
        yield ZZ i

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
      yield ZZ(p)
    low += 1000000
iterator primes*(_:typedesc[ZZ]):ZZ =
  for p in lowPrimes:
    yield ZZ(p)
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
  if x == 0: raise newException(Exception, "x = 0")
  result.unit = if x < 0: -1 else: 1
  var x = abs x
  let exp2 = countTrailingZeroBits(x)
  if exp2 > 0:
    result.factors[2] = exp2
  x = x shr exp2
  for d in ZZ.primes:
    if d == 2: continue
    if d*d > x:
      result.factors[x] = 1
      break
    var exp = 0
    while x mod d == 0:
      inc exp
      x = x div d
    if exp > 0:
      result.factors[d] = exp

func phi*(a:ZZ):ZZ =
    result = ZZ.one
    for p,v in factor(a):
        result *= (p-1)*p^(v-1)



