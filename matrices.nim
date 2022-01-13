include prelude
import numbers
import sugar, macros

type MatrixSpace[TT; N, M:static int] = object
    entries: array[N*M, TT]
type ColVectorSpace[TT; N:static int] = MatrixSpace[TT, N, 1]
type RowVectorSpace[TT; M:static int] = MatrixSpace[TT, 1, M]

#INDEXING
func `[]=`[TT;N,M:static int](a: var MatrixSpace[TT,N,M],n,m:int,val:TT) =
  a.entries[n+m*N] = val

func `[]`[TT;N,M:static int](a:MatrixSpace[TT,N,M],n,m:int):TT =
  a.entries[n+m*N]

#PRINTING
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
    
func `$`*[TT;N,M:static int](a:MatrixSpace[TT,N,M]):string =
  a.toString

macro dump(a:MatrixSpace) =
    let name = $a.toStrLit & " = "
    quote do:
        echo `name` & `a`.toString(`name`.len)

#TYPE CREATION & CONVERSIONS
template `^`(T:typedesc,k:int):typedesc =
    ColVectorSpace[T,k]
template `^`(T:typedesc,k:(int,int)):typedesc =
    MatrixSpace[T,k[0],k[1]]

converter arrayTToVector[TT;N:static int](a:array[N,TT]):RowVectorSpace[TT,N] =
  result.entries = a

converter array2DtoMatrix[TT;N,M:static int](a:array[N,array[M,TT]]):MatrixSpace[TT,N,M] =
  for n in 0..<N:
    for m in 0..<M:
      result[n,m] = a[n][m]

#ACCESING DIAGONAL & CREATING A DIAGONAL MATRIX
func diag*[TT,N](a: RowVectorSpace[TT,N] or ColVectorSpace[TT,N]): MatrixSpace[TT,N,N] =
    for n in 0..<N:
        result[n,n] = a[n]
func diag*[TT,N](a: MatrixSpace[TT,N,N]): RowVectorSpace[TT,N] =
    for n in 0..<N:
        result[n] = a[n,n]
func embed*[M](a: auto):M =
    for n in 0..<min(M.N,M.M):
        result[n,n] = a

template zero*[TT,N](R:typedesc[MatrixSpace[TT,N,N]]):R =
    embed[R](zero(typeof(TT)))
template one*[TT,N](R:typedesc[MatrixSpace[TT,N,N]]):R =
    embed[R](one(typeof(TT)))
    

#ARITHMETICS
func T*[TT;N,M:static int](a:MatrixSpace[TT,N,M]):MatrixSpace[TT,M,N] =
  for n in 0..<N:
    for m in 0..<M:
      result[m,n] = a[n,m]

func `+=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: MatrixSpace[TT,N,M]) =
    for i in 0..<N*M:
        a[i]+=b[i]
func `+`*[TT,N,M](a,b: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    result += b
func `-=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: MatrixSpace[TT,N,M]) =
    for i in 0..<N*M:
        a[i]-=b[i]
func `-`*[TT,N,M](a,b: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    result -= b
func `-`*[TT,N,M](a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    for i in 0..<N*M:
        result[i] = -a[i]
func `*`*[TT,N,M,K](a: MatrixSpace[TT,N,M],b: MatrixSpace[TT,M,K]):MatrixSpace[TT,N,K] =
    for n in 0..<N:
        for m in 0..<M:
            for k in 0..<K:
                result[n,k] = result[n,k] + a[n,m] * b[m,k]
func `*=`*[TT,N,M,K](a: var MatrixSpace[TT,N,M], b: MatrixSpace[TT,M,K]) =
    a = a*b

include algos
func `^`*[TT,N](a: MatrixSpace[TT,N,N], exp:int):MatrixSpace[TT,N,N] =
    a.binaryExponentiation exp  

func sum*[TT,N,M](a: MatrixSpace[TT,N,N]): TT =
    sum a.entries
func trace*[TT,N,M](a: MatrixSpace[TT,N,N]): TT =
    sum a.diag.entries

#INVERTING & SOLVING
func inv*[TT,N,M](a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    discard #TODO

func `/`*[TT,N,M,K](a: MatrixSpace[TT,N,M],b: MatrixSpace[TT,M,K]):MatrixSpace[TT,N,K] =
    a * b.inv
func `\`*[TT,N,M,K](a: MatrixSpace[TT,N,M],b: MatrixSpace[TT,M,K]):MatrixSpace[TT,N,K] =
    a.inv * b

#TODO // and \\ solution to linear system

    

when isMainModule:
    type M = ZZ^(3,2)
    let m:M = [[1,2],[33,4],[5,6]]
    dump m
    dump m.T
    dump m.T * m
    dump m * m.T
    dump (m * m.T)^2

    type V = ZZ^(1,3)
    let v:V = [1,2,3]
    dump v
    dump v.T