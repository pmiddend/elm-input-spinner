module DigitalNumber exposing
    ( DigitalNumber
    , decimalDigitChars
    , decreaseDecimalDigit
    , decreaseDigit
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
    , truncatedValue
    , valueToString
    )

import Char exposing (fromCode, toCode)
import DecimalNumber
import List.Extra as ListExtra
import ListUtilities exposing (leftPadList)


type DigitalNumber
    = DigitalNumber
        { minValue : DecimalNumber.DecimalNumber
        , maxValue : DecimalNumber.DecimalNumber
        , value : DecimalNumber.DecimalNumber
        }


hasSign : DigitalNumber -> Bool
hasSign (DigitalNumber { minValue }) =
    DecimalNumber.isNegative minValue


hasDecimals : DigitalNumber -> Bool
hasDecimals (DigitalNumber { value }) =
    DecimalNumber.hasDecimals value


isNegative : DigitalNumber -> Bool
isNegative (DigitalNumber { value }) =
    DecimalNumber.isNegative value


asciiCodeForZero : Int
asciiCodeForZero =
    48


valueToString : DigitalNumber -> String
valueToString (DigitalNumber { value }) =
    DecimalNumber.toString value


minValueToString : DigitalNumber -> String
minValueToString (DigitalNumber { minValue }) =
    DecimalNumber.toString minValue


maxValueToString : DigitalNumber -> String
maxValueToString (DigitalNumber { maxValue }) =
    DecimalNumber.toString maxValue



-- Here, theoretically, stuff could break. If we
-- have a string that contains things other than
-- numbers, we have values outside 0, 1 here. So
-- we could return "Maybe (List Int)", but that is
-- so cumbersome and unnecesary since we know we have a positive integer in front of us


codeToNumber : Int -> Int
codeToNumber code =
    code - asciiCodeForZero


make : DecimalNumber.DecimalNumber -> DecimalNumber.DecimalNumber -> DecimalNumber.DecimalNumber -> DigitalNumber
make minV maxV v =
    DigitalNumber
        { minValue = minV
        , maxValue = maxV
        , value = v
        }


decimalDigitChars : DigitalNumber -> List Char
decimalDigitChars (DigitalNumber { value }) =
    List.map (\x -> fromCode (x + asciiCodeForZero)) (DecimalNumber.decimals value)


truncatedValue : DigitalNumber -> Int
truncatedValue (DigitalNumber { value }) =
    DecimalNumber.truncate value


digitalNumberChangeDecimalDigit : (Int -> Int) -> DigitalNumber -> Int -> DigitalNumber
digitalNumberChangeDecimalDigit f (DigitalNumber d) n =
    let
        newDecimalDigits =
            ListExtra.updateAt n f (DecimalNumber.decimals d.value)

        newValue : DecimalNumber.DecimalNumber
        newValue =
            DecimalNumber.replaceDecimals d.value newDecimalDigits
    in
    DigitalNumber
        { d
            | value =
                if DecimalNumber.lt newValue d.minValue || DecimalNumber.gt newValue d.maxValue then
                    d.value

                else
                    newValue
        }


digitalNumberChangeDigit : (Int -> Int) -> DigitalNumber -> Int -> DigitalNumber
digitalNumberChangeDigit f (DigitalNumber d) n =
    let
        nStr : String
        nStr =
            String.fromInt (abs (DecimalNumber.truncate d.value))

        nListChar : List Char
        nListChar =
            String.toList nStr

        numberOfDigitsHelper : Int
        numberOfDigitsHelper =
            String.length <| String.fromInt <| max (DecimalNumber.truncate d.minValue) (DecimalNumber.truncate d.maxValue)

        oldDigits =
            leftPadList (List.map (codeToNumber << toCode) nListChar) 0 numberOfDigitsHelper

        newDigits =
            ListExtra.updateAt n f oldDigits

        newValue : DecimalNumber.DecimalNumber
        newValue =
            DecimalNumber.replaceInteger d.value
                ((if DecimalNumber.isNegative d.value then
                    -1

                  else
                    1
                 )
                    * List.foldl (\newDigit oldNumber -> oldNumber * 10 + newDigit) 0 newDigits
                )
    in
    DigitalNumber
        { d
            | value =
                if DecimalNumber.lt newValue d.minValue || DecimalNumber.gt newValue d.maxValue then
                    d.value

                else
                    newValue
        }


increaseDigitHelper d =
    if d < 9 then
        d + 1

    else
        d


increaseIntegerDigit : DigitalNumber -> Int -> DigitalNumber
increaseIntegerDigit =
    digitalNumberChangeDigit increaseDigitHelper


increaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
increaseDecimalDigit =
    digitalNumberChangeDecimalDigit increaseDigitHelper


decreaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDecimalDigit =
    digitalNumberChangeDecimalDigit decreaseDigitHelper


decimalValue : DigitalNumber -> DecimalNumber.DecimalNumber
decimalValue (DigitalNumber { value }) =
    value


increaseSign : DigitalNumber -> DigitalNumber
increaseSign (DigitalNumber d) =
    let
        newValue : DecimalNumber.DecimalNumber
        newValue =
            DecimalNumber.flipSign d.value

        decimalCount =
            DecimalNumber.decimalsCount d.value
    in
    DigitalNumber <|
        if DecimalNumber.lt newValue d.minValue || DecimalNumber.gt newValue d.maxValue then
            d

        else
            { d | value = newValue }


decreaseSign : DigitalNumber -> DigitalNumber
decreaseSign (DigitalNumber d) =
    let
        newValue : DecimalNumber.DecimalNumber
        newValue =
            DecimalNumber.flipSign d.value
    in
    DigitalNumber <|
        if DecimalNumber.lt newValue d.minValue || DecimalNumber.gt newValue d.maxValue then
            d

        else
            { d | value = newValue }


decreaseDigitHelper d =
    if d > 0 then
        d - 1

    else
        d


decreaseDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDigit =
    digitalNumberChangeDigit decreaseDigitHelper


numberOfIntegerDigits : DigitalNumber -> Int
numberOfIntegerDigits (DigitalNumber { minValue, maxValue }) =
    String.length <| String.fromInt <| max (abs (DecimalNumber.truncate minValue)) (abs (DecimalNumber.truncate maxValue))


numberOfDecimalDigits : DigitalNumber -> Int
numberOfDecimalDigits (DigitalNumber { value }) =
    List.length (DecimalNumber.decimals value)


integerChars : DigitalNumber -> List Char
integerChars (DigitalNumber { minValue, maxValue, value }) =
    let
        numberOfDigitsHelper : Int
        numberOfDigitsHelper =
            String.length <| String.fromInt <| max (DecimalNumber.truncate minValue) (DecimalNumber.truncate maxValue)
    in
    leftPadList (String.toList <| String.fromInt <| abs <| DecimalNumber.truncate <| value) '0' numberOfDigitsHelper
