module DecimalNumber exposing
    ( DecimalNumber
    , decimals
    , decimalsCount
    , flipSign
    , forceDecimalDigitCount
    , fromInt
    , gt
    , hasDecimals
    , isNegative
    , lt
    , make
    , replaceDecimals
    , replaceInteger
    , truncate
    )

import List
import List.Extra as ListExtra
import ListUtilities exposing (rightPadList)



-- TODO: Check out https://package.elm-lang.org/packages/chain-partners/elm-bignum/latest/Decimal


type DecimalNumber
    = DecimalNumber Int (List Int)


isNegative : DecimalNumber -> Bool
isNegative (DecimalNumber n _) =
    n < 0


flipSign : DecimalNumber -> DecimalNumber
flipSign (DecimalNumber n ds) =
    DecimalNumber -n ds


forceDecimalDigitCount : DecimalNumber -> Int -> DecimalNumber
forceDecimalDigitCount (DecimalNumber n ds) desired =
    let
        dsl =
            List.length ds
    in
    if dsl < desired then
        DecimalNumber n (rightPadList ds 0 desired)

    else
        DecimalNumber n (List.take desired ds)


fromInt : Int -> DecimalNumber
fromInt x =
    DecimalNumber
        x
        []


hasDecimals : DecimalNumber -> Bool
hasDecimals (DecimalNumber _ decimalList) =
    not <| List.isEmpty decimalList


decimals : DecimalNumber -> List Int
decimals (DecimalNumber _ d) =
    d


decimalsCount : DecimalNumber -> Int
decimalsCount =
    List.length << decimals


replaceDecimals : DecimalNumber -> List Int -> DecimalNumber
replaceDecimals (DecimalNumber n d) ds =
    DecimalNumber n ds


replaceInteger : DecimalNumber -> Int -> DecimalNumber
replaceInteger (DecimalNumber n ds) newN =
    DecimalNumber newN ds


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
