import concepts

func binaryExponentiation*(value:Ring, exp:int):typeof(value) =
    result = typeof(value).one
    var intermediate = value
    var exp1 = exp
    while exp1 > 0:
        if exp1 mod 2 == 1:
            result *= intermediate
        intermediate *= intermediate
        exp1 = exp1 shr 1