type ElementNotInvertibleError = object of CatchableError
type InfiniteCardinalityError = object of CatchableError
type UncountableCardinalityError  = object of CatchableError

template raiseInvertError*(x:auto) =
    raise newException(ElementNotInvertibleError, $x)

template raiseInfiniteError*(R:typedesc) =
    raise newException(InfiniteCardinalityError, $R)

template raiseUncountableError*(R:typedesc) =
    raise newException(UncountableCardinalityError, $R)
    