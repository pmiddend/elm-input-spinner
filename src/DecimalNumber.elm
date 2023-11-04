module DecimalNumber exposing (DecimalNumber, fromInt, fromIntegralAndDecimals, integralPart, toString)

import List exposing (foldl)
import Numeric.Decimal as NumericDecimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat exposing (fromIntAbs, toInt)
import Numeric.Rational as Rational


type MyDecimals
    = MyDecimals


type alias DecimalNumber =
    Decimal MyDecimals Int


type alias Precision =
    Int


integralPart : DecimalNumber -> Int
integralPart =
    Tuple.first << NumericDecimal.splitDecimal


decimalPart : DecimalNumber -> Int
decimalPart =
    Tuple.second << NumericDecimal.splitDecimal


getPrecision : DecimalNumber -> Precision
getPrecision =
    toInt << NumericDecimal.getPrecision


toString : DecimalNumber -> String
toString =
    NumericDecimal.toString


lt : DecimalNumber -> DecimalNumber -> Bool
lt x y =
    Rational.lessThan (NumericDecimal.toRational x) (NumericDecimal.toRational y)


gt : DecimalNumber -> DecimalNumber -> Bool
gt x y =
    Rational.greaterThan (NumericDecimal.toRational x) (NumericDecimal.toRational y)


fromInt : Precision -> Int -> DecimalNumber
fromInt precision number =
    NumericDecimal.fromInt RoundDown (fromIntAbs precision) number


fromIntegralAndDecimals : Precision -> Int -> List Int -> DecimalNumber
fromIntegralAndDecimals precision integral decimals =
    NumericDecimal.succeed
        RoundDown
        (fromIntAbs precision)
        (integral * 10 ^ precision + foldl (\new old -> old * 10 + new) 0 decimals)


tenToThePowerMinus : Precision -> Int -> DecimalNumber
tenToThePowerMinus precision whichDigit =
    NumericDecimal.succeed
        RoundDown
        (fromIntAbs precision)
        (10 ^ (precision - 1 - whichDigit))


flipSign : DecimalNumber -> DecimalNumber
flipSign value =
    let
        minusOne =
            NumericDecimal.fromInt RoundDown (NumericDecimal.getPrecision value) -1
    in
    NumericDecimal.multiply
        minusOne
        value
