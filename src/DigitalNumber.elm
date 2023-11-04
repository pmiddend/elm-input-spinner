module DigitalNumber exposing
    ( DecimalType
    , DigitalNumber
    , decimalDigitChars
    , decimalIntegralPart
    , decreaseDecimalDigit
    , decreaseIntegerDigit
    , decreaseSign
    , fastLog10
    , hasDecimals
    , hasSign
    , increaseDecimalDigit
    , increaseIntegerDigit
    , increaseSign
    , integerChars
    , isNegative
    , make
    , maxValueToString
    , minValueToString
    , numberOfDecimalDigits
    , numberOfIntegerDigits
    , truncatedValue
    , valueToString
    )

import Char exposing (fromCode, toCode)
import List.Extra as ListExtra
import ListUtilities exposing (leftPadList)
import Tuple


type MyDecimals
    = MyDecimals


type alias DecimalType =
    Decimal MyDecimals Int


type DigitalNumber
    = DigitalNumber
        { minValue : DecimalType
        , maxValue : DecimalType
        , value : DecimalType
        }


decimalIntegralPart : DecimalType -> Int
decimalIntegralPart =
    Tuple.first << NumericDecimal.splitDecimal


decimalPart : DecimalType -> Int
decimalPart =
    Tuple.second << NumericDecimal.splitDecimal


hasSign : DigitalNumber -> Bool
hasSign (DigitalNumber { minValue }) =
    decimalIntegralPart minValue < 0


hasDecimals : DigitalNumber -> Bool
hasDecimals (DigitalNumber { value }) =
    NumericDecimal.getPrecision value /= nat0


isNegative : DigitalNumber -> Bool
isNegative (DigitalNumber { value }) =
    decimalIntegralPart value < 0


asciiCodeForZero : Int
asciiCodeForZero =
    48


valueToString : DigitalNumber -> String
valueToString (DigitalNumber { value }) =
    NumericDecimal.toString value


minValueToString : DigitalNumber -> String
minValueToString (DigitalNumber { minValue }) =
    NumericDecimal.toString minValue


maxValueToString : DigitalNumber -> String
maxValueToString (DigitalNumber { maxValue }) =
    NumericDecimal.toString maxValue


codeToNumber : Int -> Int
codeToNumber code =
    code - asciiCodeForZero


make : DecimalType -> DecimalType -> DecimalType -> DigitalNumber
make minV maxV v =
    DigitalNumber
        { minValue = minV
        , maxValue = maxV
        , value = v
        }


decimalDigitChars : DigitalNumber -> List Char
decimalDigitChars (DigitalNumber { value }) =
    String.toList <| String.padLeft (toInt <| NumericDecimal.getPrecision value) '0' <| String.fromInt <| decimalPart value


truncatedValue : DigitalNumber -> Int
truncatedValue (DigitalNumber { value }) =
    decimalIntegralPart value


modifyValue : DigitalNumber -> (DecimalType -> DecimalType) -> DigitalNumber
modifyValue (DigitalNumber d) f =
    let
        newValue : DecimalType
        newValue =
            f d.value
    in
    DigitalNumber <|
        if decimalLt newValue d.minValue || decimalGt newValue d.maxValue then
            d

        else
            { d | value = newValue }


increaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
increaseIntegerDigit d whichDigit =
    modifyValue d
        (\value ->
            NumericDecimal.add
                value
                (NumericDecimal.fromInt RoundDown (NumericDecimal.getPrecision value) (10 ^ whichDigit))
        )


decreaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
decreaseIntegerDigit d whichDigit =
    modifyValue d
        (\value ->
            NumericDecimal.subtract
                value
                (NumericDecimal.fromInt RoundDown (NumericDecimal.getPrecision value) (10 ^ whichDigit))
        )



-- FIXME


increaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
increaseDecimalDigit d whichDigit =
    modifyValue d
        (\value ->
            let
                prec =
                    NumericDecimal.getPrecision value
            in
            NumericDecimal.add
                value
                (NumericDecimal.succeed
                    RoundDown
                    prec
                    (10 ^ (toInt prec - 1 - whichDigit))
                )
        )


decreaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDecimalDigit d whichDigit =
    modifyValue d
        (\value ->
            let
                prec =
                    NumericDecimal.getPrecision value
            in
            NumericDecimal.subtract
                value
                (NumericDecimal.succeed
                    RoundDown
                    prec
                    (10 ^ (toInt prec - 1 - whichDigit))
                )
        )


decimalValue : DigitalNumber -> DecimalType
decimalValue (DigitalNumber { value }) =
    value


increaseSign : DigitalNumber -> DigitalNumber
increaseSign (DigitalNumber d) =
    let
        minusOne =
            NumericDecimal.fromInt RoundDown (NumericDecimal.getPrecision d.value) -1

        newValue : DecimalType
        newValue =
            NumericDecimal.multiply
                minusOne
                d.value
    in
    DigitalNumber <|
        if decimalLt newValue d.minValue || decimalGt newValue d.maxValue then
            d

        else
            { d | value = newValue }


decimalLt x y =
    Rational.lessThan (NumericDecimal.toRational x) (NumericDecimal.toRational y)


decimalGt x y =
    Rational.greaterThan (NumericDecimal.toRational x) (NumericDecimal.toRational y)


decreaseSign : DigitalNumber -> DigitalNumber
decreaseSign (DigitalNumber d) =
    let
        minusOne =
            NumericDecimal.fromInt RoundDown (NumericDecimal.getPrecision d.value) -1

        newValue : DecimalType
        newValue =
            NumericDecimal.multiply
                minusOne
                d.value
    in
    DigitalNumber <|
        if decimalLt newValue d.minValue || decimalGt newValue d.maxValue then
            d

        else
            { d | value = newValue }



-- decreaseDigitHelper d =
--     if d > 0 then
--         d - 1
--     else
--         d


decreaseDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDigit (DigitalNumber d) n =
    DigitalNumber d


numberOfIntegerDigits : DigitalNumber -> Int
numberOfIntegerDigits (DigitalNumber { minValue, maxValue }) =
    fastLog10 <| max (abs (decimalIntegralPart minValue)) (abs (decimalIntegralPart maxValue))


fastLog10 : Int -> Int
fastLog10 x =
    if x < 0 then
        fastLog10 -x

    else if x < 10 then
        1

    else
        1 + fastLog10 (x // 10)


numberOfDecimalDigits : DigitalNumber -> Int
numberOfDecimalDigits (DigitalNumber { value }) =
    fastLog10 <| decimalPart <| value


integerChars : DigitalNumber -> List Char
integerChars (DigitalNumber { minValue, maxValue, value }) =
    let
        numberOfDigitsHelper : Int
        numberOfDigitsHelper =
            fastLog10 <| max (decimalIntegralPart minValue) (decimalIntegralPart maxValue)
    in
    leftPadList (String.toList <| String.fromInt <| abs <| decimalIntegralPart <| value) '0' numberOfDigitsHelper
