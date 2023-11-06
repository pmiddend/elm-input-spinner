module DigitalNumber exposing
    ( DecimalType
    , DigitalNumber
    , decimalChars
    , decimalValue
    , decreaseDecimalDigit
    , decreaseIntegerDigit
    , decreaseSign
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
    , replaceDecimalDigit
    , replaceIntegerDigit
    , truncatedValue
    , valueToString
    )

import Char exposing (toCode)
import DecimalNumber as DN
import List
import List.Extra as ListExtra
import ListUtilities exposing (leftPadList, rightPadList)


type alias DecimalType =
    DN.DecimalNumber


type DigitalNumber
    = DigitalNumber
        { minValue : DecimalType
        , maxValue : DecimalType
        , value : DecimalType
        , decimalPlaces : Int
        }


hasSign : DigitalNumber -> Bool
hasSign (DigitalNumber { minValue }) =
    DN.integralPart minValue < 0


hasDecimals : DigitalNumber -> Bool
hasDecimals (DigitalNumber { decimalPlaces }) =
    decimalPlaces > 0


isNegative : DigitalNumber -> Bool
isNegative (DigitalNumber { value }) =
    DN.integralPart value < 0


valueToString : DigitalNumber -> String
valueToString (DigitalNumber { value }) =
    DN.toString value


minValueToString : DigitalNumber -> String
minValueToString (DigitalNumber { minValue }) =
    DN.toString minValue


maxValueToString : DigitalNumber -> String
maxValueToString (DigitalNumber { maxValue }) =
    DN.toString maxValue


make : Int -> DecimalType -> DecimalType -> DecimalType -> DigitalNumber
make decimalPlaces minV maxV v =
    DigitalNumber
        { minValue = minV
        , maxValue = maxV
        , value = v
        , decimalPlaces = decimalPlaces
        }


truncatedValue : DigitalNumber -> Int
truncatedValue (DigitalNumber { value }) =
    DN.integralPart value


modifyValue : DigitalNumber -> (DecimalType -> DecimalType) -> DigitalNumber
modifyValue (DigitalNumber d) f =
    let
        newValue : DecimalType
        newValue =
            f d.value
    in
    DigitalNumber <|
        if DN.lt newValue d.minValue || DN.gt newValue d.maxValue then
            d

        else
            { d | value = newValue }


increaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
increaseIntegerDigit d whichDigit =
    modifyValue d
        (\value ->
            DN.add
                value
                (DN.tenToThePower (numberOfIntegerDigits d - whichDigit - 1))
        )


decreaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
decreaseIntegerDigit d whichDigit =
    modifyValue d (\value -> DN.subtract value (DN.tenToThePower (numberOfIntegerDigits d - whichDigit - 1)))


increaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
increaseDecimalDigit d whichDigit =
    modifyValue d
        (\value ->
            DN.add
                value
                (DN.tenToThePowerMinus (whichDigit + 1))
        )


decreaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDecimalDigit d whichDigit =
    modifyValue d
        (\value ->
            DN.subtract
                value
                (DN.tenToThePowerMinus (whichDigit + 1))
        )


decimalValue : DigitalNumber -> DecimalType
decimalValue (DigitalNumber { value }) =
    value


increaseSign : DigitalNumber -> DigitalNumber
increaseSign d =
    modifyValue d (\value -> DN.mul value (DN.fromInt -1))


decreaseSign : DigitalNumber -> DigitalNumber
decreaseSign =
    increaseSign


numberOfIntegerDigits : DigitalNumber -> Int
numberOfIntegerDigits (DigitalNumber { minValue, maxValue }) =
    fastLog10 <| max (abs (DN.integralPart minValue)) (abs (DN.integralPart maxValue))


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
    List.length <| DN.decimalDigits <| value


getIntegerDigit : Int -> DigitalNumber -> Int
getIntegerDigit whichDigit value =
    (\x -> x - 48) <| toCode <| Maybe.withDefault '0' <| ListExtra.getAt whichDigit (integerChars value)


getDecimalDigit : Int -> DigitalNumber -> Int
getDecimalDigit whichDigit value =
    (\x -> x - 48) <| toCode <| Maybe.withDefault '0' <| ListExtra.getAt whichDigit (decimalChars value)


replaceIntegerDigit : Int -> Int -> DigitalNumber -> DigitalNumber
replaceIntegerDigit whichDigit newDigit dn =
    modifyValue dn
        (\value ->
            let
                currentDigit =
                    getIntegerDigit whichDigit dn

                addition =
                    DN.mul
                        (DN.fromInt (newDigit - currentDigit))
                        (DN.tenToThePower (numberOfIntegerDigits dn - whichDigit - 1))
            in
            DN.add addition value
        )


replaceDecimalDigit : Int -> Int -> DigitalNumber -> DigitalNumber
replaceDecimalDigit whichDigit newDigit dn =
    modifyValue dn
        (\value ->
            let
                currentDigit =
                    getDecimalDigit whichDigit dn

                addition =
                    DN.mul
                        (DN.fromInt (newDigit - currentDigit))
                        (DN.tenToThePowerMinus (whichDigit + 1))
            in
            DN.add addition value
        )


integerChars : DigitalNumber -> List Char
integerChars (DigitalNumber { minValue, maxValue, value }) =
    let
        numberOfDigitsHelper : Int
        numberOfDigitsHelper =
            fastLog10 <| max (DN.integralPart minValue) (DN.integralPart maxValue)
    in
    leftPadList (String.toList <| String.fromInt <| abs <| DN.integralPart <| value) '0' numberOfDigitsHelper


decimalChars : DigitalNumber -> List Char
decimalChars (DigitalNumber { decimalPlaces, value }) =
    rightPadList (List.take decimalPlaces (DN.decimalDigits value)) '0' decimalPlaces
