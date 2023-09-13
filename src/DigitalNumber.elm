module DigitalNumber exposing
    ( DigitalNumber
    , decimalDigitChars
    , decreaseDecimalDigit
    , decreaseDigit
    , decreaseSign
    , increaseDecimalDigit
    , increaseDigit
    , increaseSign
    , isNegative
    , make
    , numberOfDecimalDigits
    , numberOfDigits
    , numberValue
    , toChars
    )

import Char exposing (fromCode, toCode)
import DecimalNumber
import List.Extra as ListExtra


type DigitalNumber
    = DigitalNumber
        { minValue : DecimalNumber.DecimalNumber
        , maxValue : DecimalNumber.DecimalNumber
        , negative : Bool
        , digits : List Int
        , decimalDigitsValue : List Int
        }


isNegative : DigitalNumber -> Bool
isNegative (DigitalNumber { negative }) =
    negative


asciiCodeForZero =
    48


leftPadList : List a -> a -> Int -> List a
leftPadList xs a len =
    let
        xsLen =
            List.length xs

        remainder =
            len - xsLen
    in
    if remainder > 0 then
        List.repeat remainder a ++ xs

    else
        xs



-- Here, theoretically, stuff could break. If we
-- have a string that contains things other than
-- numbers, we have values outside 0, 1 here. So
-- we could return "Maybe (List Int)", but that is
-- so cumbersome and unnecesary since we know we have a positive integer in front of us


codeToNumber : Int -> Int
codeToNumber code =
    code - asciiCodeForZero


make : Int -> Int -> Int -> List Int -> DigitalNumber
make minV maxV n decimalDigitsValue =
    let
        nStr : String
        nStr =
            String.fromInt (abs n)

        nListChar : List Char
        nListChar =
            String.toList nStr

        numberOfDigitsHelper : Int
        numberOfDigitsHelper =
            String.length <| String.fromInt <| max (abs minV) (abs maxV)
    in
    DigitalNumber
        { minValue = DecimalNumber.fromInt minV
        , maxValue = DecimalNumber.fromInt maxV
        , negative = n < 0
        , digits = leftPadList (List.map (codeToNumber << toCode) nListChar) 0 numberOfDigitsHelper
        , decimalDigitsValue = decimalDigitsValue
        }


decimalDigitChars : DigitalNumber -> List Char
decimalDigitChars (DigitalNumber { decimalDigitsValue }) =
    List.map (\x -> fromCode (x + asciiCodeForZero)) decimalDigitsValue


numberValue : DigitalNumber -> Int
numberValue (DigitalNumber { digits, negative }) =
    (if negative then
        -1

     else
        1
    )
        * List.foldl (\newDigit oldNumber -> oldNumber * 10 + newDigit) 0 digits


digitalNumberChangeDecimalDigit : (Int -> Int) -> DigitalNumber -> Int -> DigitalNumber
digitalNumberChangeDecimalDigit f (DigitalNumber d) n =
    let
        newDecimalDigits =
            ListExtra.updateAt n f d.decimalDigitsValue

        newValue : DecimalNumber.DecimalNumber
        newValue =
            DecimalNumber.make (numberValue (DigitalNumber d)) newDecimalDigits
    in
    DigitalNumber
        { d
            | decimalDigitsValue =
                if DecimalNumber.lt newValue d.minValue || DecimalNumber.gt newValue d.maxValue then
                    d.decimalDigitsValue

                else
                    newDecimalDigits
        }


digitalNumberChangeDigit : (Int -> Int) -> DigitalNumber -> Int -> DigitalNumber
digitalNumberChangeDigit f (DigitalNumber d) n =
    let
        newDigits =
            ListExtra.updateAt n f d.digits

        newValue : DecimalNumber.DecimalNumber
        newValue =
            DecimalNumber.fromInt <| numberValue (DigitalNumber { d | digits = newDigits })
    in
    DigitalNumber
        { d
            | digits =
                if DecimalNumber.lt newValue d.minValue || DecimalNumber.gt newValue d.maxValue then
                    d.digits

                else
                    newDigits
        }


increaseDigitHelper d =
    if d < 9 then
        d + 1

    else
        d


increaseDigit : DigitalNumber -> Int -> DigitalNumber
increaseDigit =
    digitalNumberChangeDigit increaseDigitHelper


increaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
increaseDecimalDigit =
    digitalNumberChangeDecimalDigit increaseDigitHelper


decreaseDecimalDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDecimalDigit =
    digitalNumberChangeDecimalDigit decreaseDigitHelper


increaseSign : DigitalNumber -> DigitalNumber
increaseSign (DigitalNumber d) =
    if d.negative then
        DigitalNumber { d | negative = False }

    else
        DigitalNumber d


decreaseSign : DigitalNumber -> DigitalNumber
decreaseSign (DigitalNumber d) =
    if d.negative then
        DigitalNumber d

    else
        DigitalNumber { d | negative = True }


decreaseDigitHelper d =
    if d > 0 then
        d - 1

    else
        d


decreaseDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDigit =
    digitalNumberChangeDigit decreaseDigitHelper


numberOfDigits : DigitalNumber -> Int
numberOfDigits (DigitalNumber { minValue, maxValue }) =
    String.length <| String.fromInt <| max (abs (DecimalNumber.truncate minValue)) (abs (DecimalNumber.truncate maxValue))


numberOfDecimalDigits : DigitalNumber -> Int
numberOfDecimalDigits (DigitalNumber { decimalDigitsValue }) =
    List.length decimalDigitsValue


toChars : DigitalNumber -> List Char
toChars (DigitalNumber { digits }) =
    List.map (\x -> fromCode (x + asciiCodeForZero)) digits
