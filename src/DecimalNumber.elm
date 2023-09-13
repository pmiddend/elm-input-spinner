module DecimalNumber exposing (DecimalNumber, fromInt, gt, lt, make, truncate)

import List
import List.Extra as ListExtra


type DecimalNumber
    = DecimalNumber Int (List Int)


fromInt : Int -> DecimalNumber
fromInt x =
    DecimalNumber x []


make : Int -> List Int -> DecimalNumber
make =
    DecimalNumber


truncate : DecimalNumber -> Int
truncate (DecimalNumber d _) =
    d


listLt : List comparable -> List comparable -> Bool
listLt l0 l1 =
    List.foldr (\( l0e, l1e ) isLt -> isLt && l0e < l1e) True (ListExtra.zip l0 l1)


listGt : List comparable -> List comparable -> Bool
listGt l0 l1 =
    List.foldr (\( l0e, l1e ) isGt -> isGt && l0e > l1e) True (ListExtra.zip l0 l1)


lt : DecimalNumber -> DecimalNumber -> Bool
lt (DecimalNumber d0 c0) (DecimalNumber d1 c1) =
    d0 < d1 || d0 == d1 && listLt c0 c1


gt : DecimalNumber -> DecimalNumber -> Bool
gt (DecimalNumber d0 c0) (DecimalNumber d1 c1) =
    d0 > d1 || d0 == d1 && listGt c0 c1
