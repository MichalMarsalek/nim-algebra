import tables, strutils, algorithm, sequtils

type Factorisation*[T] = object
  unit*: T
  factors*: OrderedTable[T,int]

func `$`*[T](x:Factorisation[T]):string =
  if x.unit == T.one:
    discard
  elif x.unit == -T.one:
    result &= "-"
  else:
    result &= $x.unit & " * "
  var parts:seq[string]
  for k,v in x.factors:
    var kk = $k
    if "+" in kk or "-" in kk:
        kk = "(" & kk & ")"
    if v == 1:
      parts.add kk
    else:
      parts.add kk & "^" & $v
  result &= parts.join(" * ")

func value*[T](x:Factorisation[T]):T =
  result = x.unit
  for k,v in x.factors:
    result *= k^v

func `*`*[T](a,b:Factorisation[T]):Factorisation[T] =
  result.unit = a.unit * b.unit
  let keys = sorted(toSeq(a.factors.keys) & toSeq(b.factors.keys))
  for k in keys:
    result.factors[k] = a.factors.getOrDefault(k) + b.factors.getOrDefault(k)

func `*`*[T](a:Factorisation[T],b:T):Factorisation[T] =
  a * b.factor
func `*`*[T](a:T,b:Factorisation[T]):Factorisation[T] =
  a.factor * b

iterator items*[T](a:Factorisation[T]): T =
    for k,v in a.factors:
        for _ in 1..v:
            yield k
iterator pairs*[T](a:Factorisation[T]): (T,int) =
    for k,v in a.factors:
        yield (k,v)