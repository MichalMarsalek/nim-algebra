EMBEDDINGS = [
  #Numbers
  [int, ZZ]:[+,-,*,div,mod]
  [QQ, RR, CC]:[+,-,*,/],
  [QQQ[?D], CC],
  [ZZQ[?D], CC],
  [ZZ, ZZMod[?M]],
  [int, intMod[?M]],
  #Finite rings
  [ZZ, BinaryField[?DEG,?MOD,?V]]
  [ZZ, GenFiniteField[?P,?DEG,?MOD,?V]]
  #Polynomials
  [?D, PolynomialRing[?D]],
  [PolynomialRing[?T1],PolynomialRing[?T2]]#TODO
  #Factor rings
  [?R, FactorRing[?R,?C]]
  #Matrices
  [MatrixSpace[?T1],MatrixSpace[?T2]]#TODO
  
  
const MAX_EMBED_DEPTH = 10
#[
val.embed(T) atomic embed
val.embed(T1,T2,T3) ---> val.embed(T1).embed(T2).embed(T3)

a:T + b:S --> 
    add(a.embed(path,path,S), b)
    or
    add(a, b.embed(path,path,T))
]#