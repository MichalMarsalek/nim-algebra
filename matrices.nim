include prelude
import numbers
import sugar, macros
import math
#import polynomials

type MatrixSpace[TT; N, M:static int] = object
    entries: array[N*M, TT]
type ColVectorSpace[TT; N:static int] = MatrixSpace[TT, N, 1]
type RowVectorSpace[TT; M:static int] = MatrixSpace[TT, 1, M]

type AffineSpace[V] = object
    generators: seq[V]
    translation: V
    empty: bool

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

func `$`*[V](s:AffineSpace[V]):string =
  if s.empty: return "{}"
  if s.generators.len == 0:
    return "{" & $s.translation & "}"
  result = "span(" & s.generators.join(", ") & ")"
  if s.translation != zero(V):
    result &= " + " & $s.translation

#TYPE CREATION & CONVERSIONS
template `^`(T:typedesc,k:int):typedesc =
    ColVectorSpace[T,k]
template `^`(T:typedesc,k:(int,int)):typedesc =
    MatrixSpace[T,k[0],k[1]]

converter arrayToRowVector[TT;N:static int](a:array[N,TT]):RowVectorSpace[TT,N] =
  result.entries = a
converter arrayToColVector[TT;N:static int](a:array[N,TT]):ColVectorSpace[TT,N] =
  result.entries = a

converter array2DtoMatrix[TT;N,M:static int](a:array[N,array[M,TT]]):MatrixSpace[TT,N,M] =
  for n in 0..<N:
    for m in 0..<M:
      result[n,m] = a[n][m]

#ACCESING DIAGONAL & CREATING A DIAGONAL MATRIX
func diag*[TT,N](a: RowVectorSpace[TT,N] or ColVectorSpace[TT,N]): MatrixSpace[TT,N,N] =
    for n in 0..<N:
        result[n,n] = a.entries[n]
func diag*[TT,N](a: MatrixSpace[TT,N,N]): RowVectorSpace[TT,N] =
    for n in 0..<N:
        result.entries[n] = a[n,n]
func embed*[M](a: auto):M =
    for n in 0..<min(M.N,M.M):
        result[n,n] = a

func zero*[TT,N,M](R:typedesc[MatrixSpace[TT,N,M]]):R =
    discard
func one*[TT,N](R:typedesc[MatrixSpace[TT,N,N]]):R =
    embed[R](one(typeof(TT)))
    

#ARITHMETICS
func T*[TT;N,M:static int](a:MatrixSpace[TT,N,M]):MatrixSpace[TT,M,N] =
  for n in 0..<N:
    for m in 0..<M:
      result[m,n] = a[n,m]
func H*[CC;N,M](a:MatrixSpace[CC,N,M]):MatrixSpace[CC,M,N] =
  for n in 0..<N:
    for m in 0..<M:
      result[m,n] = conjugate a[n,m]

func `+=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: MatrixSpace[TT,N,M]) =
    for i in 0..<N*M:
        a.entries[i]+=b.entries[i]
func `+`*[TT,N,M](a,b: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    result += b
func `-=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: MatrixSpace[TT,N,M]) =
    for i in 0..<N*M:
        a.entries[i]-=b.entries[i]
func `-`*[TT,N,M](a,b: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    result -= b
func `-`*[TT,N,M](a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    for i in 0..<N*M:
        result.entries[i] = -a.entries[i]
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

func rowEchelon*[TT,N,M](a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M]
func det*[TT,N](a: MatrixSpace[TT,N,N]): TT =
    let ech = rowEchelon a #temporary TODO don't use division for det calc
    result = ech[0,0]
    for n in 1..<N:
        result *= ech[n,n]

#func charpoly*[TT,N](A: MatrixSpace[TT,N,N]):PolynomialRing[TT,"λ"] =
#    discard #TODO
    #type S = PolynomialRing[TT,"λ"]
    #det(embed[MatrixSpace[S,N,N]](A) - S.gen)
#func eigenvalues*[TT,N](A: MatrixSpace[TT,N,N]):seq[TT] =
#    discard #TODO
    #A.charpoly.roots

#SCALAR ARITHMETICS
func `+=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: TT) =
    for i in 0..<N*M:
        a.entries[i] += b
func `+`*[TT,N,M](a: MatrixSpace[TT,N,M],b: TT):MatrixSpace[TT,N,M] =
    result = a
    result += b
func `+`*[TT,N,M](b: TT, a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    result += b
func `-=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: TT) =
    for i in 0..<N*M:
        a.entries[i]-=b
func `-`*[TT,N,M](a: MatrixSpace[TT,N,M],b: TT):MatrixSpace[TT,N,M] =
    result = a
    result -= b
func `-`*[TT,N,M](b: TT, a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = -a
    result += b
func `*=`*[TT,N,M](a: var MatrixSpace[TT,N,M], b: TT) =
    for i in 0..<N*M:
        a.entries[i] *= b
func `*`*[TT,N,M](a: MatrixSpace[TT,N,M],b: TT):MatrixSpace[TT,N,M] =
    result = a
    result *= b
func `*`*[TT,N,M](b: TT, a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    result *= b

#NORMS
func norm*[TT,N](a: RowVectorSpace[TT,N] or ColVectorSpace[TT,N],p:static[int]=2): TT =
    when p==2 and not(TT is RR or TT is CC): assert false
    when p== -1: return max a.entries
    when p==0: return min a.entries
    when p==1: return a.entries.map(abs).sum
    when p==2: return sqrt(sum a.entries.mapIt(it*it))


#RANDOM and ENUMERATION
func random*[TT,N,M](R:typedesc[MatrixSpace[TT,N,M]]):R =
    for i in 0..<N*M:
        result.entries[i] = R.random

iterator items*[TT,N,M](R:typedesc[MatrixSpace[TT,N,M]]):R =
    discard
iterator nonzero*[TT,N,M](R:typedesc[MatrixSpace[TT,N,M]]):R =
    discard
iterator invertible*[TT,N,M](R:typedesc[MatrixSpace[TT,N,M]]):R =
    for A in R.nonzero:
        if A.isRegular:
            yield A

#SUBSPACE OPERATIONS

func span*[V](a: varargs[V]):AffineSpace[V] =
    result.generators = @a
func `+`[V](a:AffineSpace[V], b:V):AffineSpace[V] =
    result = a
    result.translation += b

#INVERTING & SOLVING
func rowEchelon*[TT,N,M](a: MatrixSpace[TT,N,M]):MatrixSpace[TT,N,M] =
    result = a
    for m in 0..<M:
        var nonzeroRow = m
        while result[nonzeroRow,m] == zero(TT) and nonzeroRow < N:
            inc nonzeroRow
        if nonzeroRow >= M:
            continue
        if nonzeroRow > m:
            for j in m..<M:
                let temp = result[nonzeroRow,j]
                result[nonzeroRow,j] = result[m,j]
                result[m,j] = temp
        for i in m+1..<N:
            let coeff = -result[i,m]/result[m,m]
            for j in m..<M:
                result[i,j] = result[i,j] + coeff * result[m,j]

func rankAtLeast*[TT,N,M](a: MatrixSpace[TT,N,M], bound:int):bool =
    var a = a
    var hope = static: min(N,M)
    var pass = 0
    for m in 0..<M:
        var nonzeroRow = m
        while a[nonzeroRow,m] == zero(TT) and nonzeroRow < N:
            inc nonzeroRow
        if nonzeroRow >= M:
            dec hope
            if hope < bound: return false
            continue
        inc pass
        if pass >= bound: return true
        if nonzeroRow > m:
            for j in m..<M:
                let temp = a[nonzeroRow,j]
                a[nonzeroRow,j] = a[m,j]
                a[m,j] = temp
        for i in m+1..<N:
            let coeff = -a[i,m]/a[m,m]
            for j in m..<M:
                a[i,j] = a[i,j] + coeff * a[m,j]
    return true

func rank*[TT,N,M](a: MatrixSpace[TT,N,M]):int =
    var a = a
    for m in 0..<M:
        var nonzeroRow = m
        while a[nonzeroRow,m] == zero(TT) and nonzeroRow < N:
            inc nonzeroRow
        if nonzeroRow >= M:
            continue
        inc result
        if nonzeroRow > m:
            for j in m..<M:
                let temp = a[nonzeroRow,j]
                a[nonzeroRow,j] = a[m,j]
                a[m,j] = temp
        for i in m+1..<N:
            let coeff = -a[i,m]/a[m,m]
            for j in m..<M:
                a[i,j] = a[i,j] + coeff * a[m,j]

func isRegular*[TT,N,M](a: MatrixSpace[TT,N,M]):bool =
    when N != M: return false
    when N == M:
        return a.rankAtLeast N
func isRegular*[TT,N,M](a: MatrixSpace[TT,N,M]):bool =
    a.isInvertible

func inv*[TT,N](a: MatrixSpace[TT,N,N]):MatrixSpace[TT,N,N] =
    discard #TODO

func `/`*[TT,N,M,K](a: MatrixSpace[TT,N,M],b: MatrixSpace[TT,M,K]):MatrixSpace[TT,N,K] =
    a * b.inv
func `\`*[TT,N,M,K](a: MatrixSpace[TT,N,M],b: MatrixSpace[TT,M,K]):MatrixSpace[TT,N,K] =
    a.inv * b

func ker*[TT,N,M](a: MatrixSpace[TT,N,M]):AffineSpace[RowVectorSpace[TT,M]] =
    discard #TODO

#TODO // and \\ solution to linear system

    

when isMainModule:
    type M = ZZ^(3,2)
    let m:M = [[1,2],[33,4],[5,6]]
    dump m
    dump m.T
    dump m.T * m
    dump m * m.T
    dump (m * m.T)^2
    let G = (m * m.T)^2
    dump G.diag.diag

    type V = ZZ^(1,3)
    let v:V = [1,2,3]
    dump v
    dump v.T
    let U = span(v, V [0,0,1]) + V [0,1,0]
    echo $U
    
    let m22:QQ^(2,2) = [[1//1,2//1],[1//1,1//1]]
    dump m22
    dump det m22
    
    dump rank m22.rowEchelon
    dump norm (RR^2) [3.0, 4.0]