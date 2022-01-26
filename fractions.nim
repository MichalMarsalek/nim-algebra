import errors

type Fractions*[T] = object
    num*: T
    den*: T

func initFrac*[T](n,d:T): Fractions[T] =
    result.num = n
    result.den = d

func `/`[T](_,_:typedesc[T]):typedesc =
    when T is Fractions: #TODO replace with T.isField to include finite fields etc.
        T
    else:
        Fractions[T]

func `$`*[T](x: Fractions[T]): string =
    var d = x.den
    var n = x.num
    if ($d)[0] == '-':
        d = -d
        n = -n
    if d == T.one:
        return $n
    $n & "/" & $d

func reduce*[T](x: var Fractions[T]) {.inline.} =
    mixing gcd
    let g = gcd(x.num, x.den)
    x.num = x.num div g
    x.den = x.den div g

func `==`*[T](a,b:Fractions[T]):bool {.inline.} =
    a.num * b.den == a.den * b.num
func `<`*[T](a,b:Fractions[T]):bool {.inline.} =
    a.num * b.den < a.den * b.num
func `<=`*[T](a,b:Fractions[T]):bool {.inline.} =
    a.num * b.den <= a.den * b.num
func `!=`*[T](a,b:Fractions[T]):bool {.inline.} =
    a.num * b.den != a.den * b.num

func `+`*[T](x, y: Fractions[T]): Fractions[T] {.inline.} =
  result.num = x.num * y.den + y.num * x.den
  result.den = x.den * y.den
  reduce result
func `+=`*[T](x: var Fractions[T], y: Fractions[T]) {.inline.} =
  x.num = x.num * y.den + y.num * x.den
  x.den = x.den * y.den
  reduce x

func `-`*[T](x: Fractions[T]): Fractions[T] {.inline.} =
  result.num = -x.num
  result.den = x.den
func `-`*[T](x, y: Fractions[T]): Fractions[T] {.inline.} =
  result.num = x.num * y.den - y.num * x.den
  result.den = x.den * y.den
  reduce result
func `-=`*[T](x: var Fractions[T], y: Fractions[T]) {.inline.} =
  x.num = x.num * y.den - y.num * x.den
  x.den = x.den * y.den
  reduce x

func `*`*[T](x, y: Fractions[T]): Fractions[T] {.inline.} =
  result.num = x.num * y.num
  result.den = x.den * y.den
  reduce result
func `*=`*[T](x: var Fractions[T], y: Fractions[T]) {.inline.} =
  x.num *= y.num
  x.den *= y.den
  reduce x

func inv*[T](x: Fractions[T]): Fractions[T] {.inline.} =
  if x.num == T.zero:
    raiseInvertError(x)
  result.num = x.den
  result.den = x.num