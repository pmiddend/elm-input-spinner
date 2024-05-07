module DigitalNumber exposing
    ( DecimalType
    , DigitalNumber
    , decimalChars
    , decreaseDecimalDigit
    , decreaseIntegerDigit
    , decreaseSign
    , getValue
    , hasDecimals
    , hasSign
    , increaseDecimalDigit
    , increaseIntegerDigit
    , increaseSign
    , integerChars
    , isNegative
    , make
    , modifyValue
    , numberOfDecimalDigits
    , numberOfIntegerDigits
    , replaceDecimalDigit
    , replaceIntegerDigit
    , valueAsFloat
    )

{-| A "digital number" (surely a bad term) is a number where you have read/write access to its individual "digits" (decimal and integral). This module provides this data type. A digital number has a min and a max value as well (which determines the integral digits of the number), but you have to specify the number of decimal digits, since it cannot be inferred from min/max.
-}

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


{-| Retrieve just the value of the digital number
-}
getValue : DigitalNumber -> DecimalType
getValue (DigitalNumber { value }) =
    value


valueAsFloat : DigitalNumber -> Float
valueAsFloat =
    DN.toFloat << getValue


{-| Check if the number has a sign (used to determine if we display a plus/minus in front of it and making that changeable)
-}
hasSign : DigitalNumber -> Bool
hasSign (DigitalNumber { minValue }) =
    DN.integralPart minValue < 0


{-| Check if the number has decimal places at all. If not, we can omit the ".xxx" and make the input field more narrow.
-}
hasDecimals : DigitalNumber -> Bool
hasDecimals (DigitalNumber { decimalPlaces }) =
    decimalPlaces > 0


{-| Check if the number is currently negative
-}
isNegative : DigitalNumber -> Bool
isNegative (DigitalNumber { value }) =
    DN.integralPart value < 0


{-| Create a digital number (information hiding here, since we don't expose the type constructor directly)
-}
make : Int -> DecimalType -> DecimalType -> DecimalType -> DigitalNumber
make decimalPlaces minV maxV v =
    DigitalNumber
        { minValue = minV
        , maxValue = maxV
        , value = v
        , decimalPlaces = decimalPlaces
        }


{-| Helper function to modify the value of a digital number, taking care of not over/underflowing it
-}
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


{-| Increase the d'th integer digit of the number (taking care of under/overflow)
-}
increaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
increaseIntegerDigit d whichDigit =
    modifyValue d
        (\value ->
            DN.add
                value
                (DN.tenToThePower (numberOfIntegerDigits d - whichDigit - 1))
        )


{-| Decrease the d'th integer digit of the number (taking care of under/overflow)
-}
decreaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
decreaseIntegerDigit d whichDigit =
    modifyValue d (\value -> DN.subtract value (DN.tenToThePower (numberOfIntegerDigits d - whichDigit - 1)))


{-| Increase the d'th decimal digit of the number (taking care of under/overflow)
-}
increaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
increaseDecimalDigit d whichDigit =
    modifyValue d
        (\value ->
            DN.add
                value
                (DN.tenToThePowerMinus (whichDigit + 1))
        )


{-| Decrease the d'th decimal digit of the number (taking care of under/overflow)
-}
decreaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDecimalDigit d whichDigit =
    modifyValue d
        (\value ->
            DN.subtract
                value
                (DN.tenToThePowerMinus (whichDigit + 1))
        )


{-| Flip the sign (yeah, increase/decrease is weirdly named)
-}
increaseSign : DigitalNumber -> DigitalNumber
increaseSign d =
    modifyValue d (\value -> DN.mul value (DN.fromInt -1))


{-| Flip the sign (yeah, increase/decrease is weirdly named)
-}
decreaseSign : DigitalNumber -> DigitalNumber
decreaseSign =
    increaseSign


{-| Return the number of integral digits of the number (to display the correct number of digits in the UI)
-}
numberOfIntegerDigits : DigitalNumber -> Int
numberOfIntegerDigits (DigitalNumber { minValue, maxValue }) =
    fastLog10 <| max (abs (DN.integralPart minValue)) (abs (DN.integralPart maxValue))


{-| Return the number of integral digits of a number (without converting it to a string and counting characters, hence the "fast" prefix)
-}
fastLog10 : Int -> Int
fastLog10 x =
    if x < 0 then
        fastLog10 -x

    else if x < 10 then
        1

    else
        1 + fastLog10 (x // 10)


{-| Return the number of decimal digits of the number (which is fixed on construction of the digital number)
-}
numberOfDecimalDigits : DigitalNumber -> Int
numberOfDecimalDigits (DigitalNumber { decimalPlaces }) =
    decimalPlaces


{-| Return the whichDigit's integer digit of the number
-}
getIntegerDigit : Int -> DigitalNumber -> Int
getIntegerDigit whichDigit value =
    (\x -> x - 48) <| toCode <| Maybe.withDefault '0' <| ListExtra.getAt whichDigit (integerChars value)


{-| Return the whichDigit's decimal digit of the number
-}
getDecimalDigit : Int -> DigitalNumber -> Int
getDecimalDigit whichDigit value =
    (\x -> x - 48) <| toCode <| Maybe.withDefault '0' <| ListExtra.getAt whichDigit (decimalChars value)


{-| Replace the whichDigit's integer digit with "newDigit" (so converting "123" to "193" or something)
-}
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


{-| Replace the whichDigit's integer digit with "newDigit" (so converting "123.45" to "123.95" or something)
-}
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


{-| Return the integral characters for the number (as chars, so just for display)
-}
integerChars : DigitalNumber -> List Char
integerChars (DigitalNumber { minValue, maxValue, value }) =
    let
        numberOfDigitsHelper : Int
        numberOfDigitsHelper =
            fastLog10 <| max (DN.integralPart minValue) (DN.integralPart maxValue)
    in
    leftPadList (String.toList <| String.fromInt <| abs <| DN.integralPart <| value) '0' numberOfDigitsHelper


{-| Return the decimal characters for the number (as chars, so just for display)
-}
decimalChars : DigitalNumber -> List Char
decimalChars (DigitalNumber { decimalPlaces, value }) =
    rightPadList (List.take decimalPlaces (DN.decimalDigits value)) '0' decimalPlaces
