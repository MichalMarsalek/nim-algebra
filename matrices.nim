include prelude
include numbers
import sugar, macros

type MatrixSpace[TT; row, col:static int] = distinct array[row*col, TT]
type ColVectorSpace[TT; row:static int] = MatrixSpace[TT, row, 1]
type RowVectorSpace[TT; col:static int] = MatrixSpace[TT, 1, col]


func `[]=`[TT;N,M:static int](a: var MatrixSpace[TT,N,M],n,m:int,val:TT) =
  `[]=`((array[N*M, TT])(a), n+m*N, val)

func `[]`[TT;N,M:static int](a:MatrixSpace[TT,N,M],n,m:int):TT =
  `[]`((array[N*M, TT])(a), n+m*N)

converter arrayTToVector[TT;N:static int](a:array[N,TT]):RowVectorSpace[TT,N] =
  for n in 0..<N:
    result[n,0] = a[n]

converter array2DtoMatrix[TT;N,M:static int](a:array[N,array[M,TT]]):MatrixSpace[TT,N,M] =
  for n in 0..<N:
    for m in 0..<M:
      result[n,m] = a[n][m]

func toString[TT;N,M:static int](a:MatrixSpace[TT,N,M], line1Offset = 0): string =
    when M == 1:
        return $a.T & "^T"
    var maxLengths = toSeq(0..<M).map(m => toSeq(0..<N).map(n => ($a[n,m]).len).max)
    map(toSeq(0..<N),
        n => " ".repeat(if n > 0: line1Offset else: 0) & "[" &
             map(toSeq(0..<M),
                 m => ($a[n,m]).align(maxLengths[m])
             ).join(" ") & "]"
    ).join "\n"
    
func `$`[TT;N,M:static int](a:MatrixSpace[TT,N,M]):string =
  a.toString

func T[TT;N,M:static int](a:MatrixSpace[TT,N,M]):MatrixSpace[TT,M,N] =
  for n in 0..<N:
    for m in 0..<M:
      result[m,n] = a[n,m]

func `+=`[TT,N,M](a:MatrixSpace[TT,N,M], b:var MatrixSpace[TT,N,M]) =
    discard #TODO

macro dump(a:MatrixSpace) =
    let name = $a.toStrLit & " = "
    quote do:
        echo `name` & `a`.toString(`name`.len)

template `^`(T:typedesc,k:int):typedesc =
    ColVectorSpace[T,k]
template `^`(T:typedesc,k:(int,int)):typedesc =
    MatrixSpace[T,k[0],k[1]]
    

when isMainModule:
    type M = ZZ^(3,2)
    let m:M = [[1,2],[33,4],[5,6]]
    let mT = m.T
    dump m
    dump m.T

    type V = ZZ^(1,3)
    let v:V = [1,2,3]
    let u = v.T
    dump v
    dump u