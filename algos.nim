func binaryExponentiation*[T](value:T, exp:int):T =
    result = one(T)
    var intermediate = value
    var exp1 = exp
    while exp1 > 0:
        if exp1 mod 2 == 1:
            result *= intermediate
        intermediate *= intermediate
        exp1 = exp1 shr 1