import factorisations, algorithm, bitops, math, tables

type ZZ* = int     #this is only temporary
template zero*(_:typedesc[ZZ]):ZZ = 0
template one*(_:typedesc[ZZ]):ZZ = 1

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

iterator divisors*(a:ZZ):ZZ =
    var a = abs a
    var rest:seq[ZZ]
    for i in 1..a:
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
  for d in countup(3,x,2):
    if d*d > x:
      result.factors[x] = 1
      break
    var exp = 0
    while x mod d == 0:
      inc exp
      x = x div d
    if exp > 0:
      result.factors[d] = exp



