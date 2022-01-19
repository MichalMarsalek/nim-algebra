import tables, strutils, algorithm, sequtils

type
  Factorisable* = concept s
    factor(s)
  Factorisation*[T] = object
    unit*: T
    factors*: OrderedTable[T,int]

func `$`*(x:Factorisation[Factorisable]):string =
  if x.unit == 1:
    discard
  elif x.unit == -1:
    result &= "-"
  else:
    result &= $x.unit & " * "
  var parts:seq[string]
  for k,v in x.factors:
    if v == 1:
      parts.add $k
    else:
      parts.add $k & "^" & $v
  result &= parts.join(" * ")

func value*(x:Factorisation[Factorisable]):Factorisable =
  result = x.unit
  for k,v in x.factors:
    result *= k^v

func `*`*(a,b:Factorisation[Factorisable]):Factorisation[Factorisable] =
  result.unit = a.unit * b.unit
  let keys = sorted(toSeq(a.factors.keys) & toSeq(b.factors.keys))
  for k in keys:
    result.factors[k] = a.factors.getOrDefault(k) + b.factors.getOrDefault(k)

func `*`*(a:Factorisation[Factorisable],b:Factorisable):Factorisation[Factorisable] =
  a * b.factor
func `*`*(a:Factorisable,b:Factorisation[Factorisable]):Factorisation[Factorisable] =
  a.factor * b