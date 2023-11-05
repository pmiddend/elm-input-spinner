module DecimalNumber exposing
    ( DecimalNumber
    , add
    , decimalDigits
    , flipSign
    , fromInt
    , fromIntegralAndDecimals
    , gt
    , integralPart
    , lt
    , mul
    , subtract
    , tenToThePower
    , tenToThePowerMinus
    , toString
    )

import Char exposing (fromCode)
import Decimal
import List
import String.Extra as StringExtra


type alias DecimalNumber =
    Decimal.Decimal


integralPart : DecimalNumber -> Int
integralPart =
    Maybe.withDefault -1337 << Maybe.andThen String.toInt << List.head << String.split "." << Decimal.toString


decimalDigits : DecimalNumber -> List Char
decimalDigits =
    String.toList << StringExtra.rightOf "." << Decimal.toString


toString : DecimalNumber -> String
toString =
    Decimal.toString


lt : DecimalNumber -> DecimalNumber -> Bool
lt =
    Decimal.lt


gt : DecimalNumber -> DecimalNumber -> Bool
gt =
    Decimal.gt


add : DecimalNumber -> DecimalNumber -> DecimalNumber
add =
    Decimal.add


subtract : DecimalNumber -> DecimalNumber -> DecimalNumber
subtract =
    Decimal.sub


mul : DecimalNumber -> DecimalNumber -> DecimalNumber
mul =
    Decimal.mul


fromInt : Int -> DecimalNumber
fromInt =
    Decimal.fromInt


asciiCodeForZero : Int
asciiCodeForZero =
    48


fromIntegralAndDecimals : Int -> List Int -> DecimalNumber
fromIntegralAndDecimals integral decimals =
    Maybe.withDefault (Decimal.fromInt 0) <|
        Decimal.fromString <|
            if List.isEmpty decimals then
                String.fromInt integral

            else
                String.fromInt integral
                    ++ "."
                    ++ String.fromList (List.map (\d -> fromCode (d + asciiCodeForZero)) decimals)


tenToThePowerMinus : Int -> DecimalNumber
tenToThePowerMinus whichDigit =
    Decimal.fromIntWithExponent 1 -whichDigit


tenToThePower : Int -> DecimalNumber
tenToThePower whichDigit =
    Decimal.fromIntWithExponent 1 whichDigit


flipSign : DecimalNumber -> DecimalNumber
flipSign value =
    let
        minusOne =
            Decimal.fromInt -1
    in
    Decimal.mul
        minusOne
        value
