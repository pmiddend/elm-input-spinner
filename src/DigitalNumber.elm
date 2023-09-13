module DigitalNumber exposing
    ( DigitalNumber
    , decreaseDigit
    , decreaseSign
    , increaseDigit
    , increaseSign
    , isNegative
    , make
    , numberOfDigits
    , numberValue
    , toChars
    )

import Char exposing (fromCode, toCode)
import List.Extra as ListExtra


type DigitalNumber
    = DigitalNumber { minValue : Int, maxValue : Int, negative : Bool, digits : List Int }


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


make : Int -> Int -> Int -> DigitalNumber
make minV maxV n =
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

        -- Here, theoretically, stuff could break. If we
        -- have a string that contains things other than
        -- numbers, we have values outside 0, 1 here. So
        -- we could return "Maybe (List Int)", but that is
        -- so cumbersome and unnecesary since we know we have a positive integer in front of us
        codeToNumber : Int -> Int
        codeToNumber code =
            code - asciiCodeForZero
    in
    DigitalNumber
        { minValue = minV
        , maxValue = maxV
        , negative = n < 0
        , digits = leftPadList (List.map (codeToNumber << toCode) nListChar) 0 numberOfDigitsHelper
        }


numberValue : DigitalNumber -> Int
numberValue (DigitalNumber { digits, negative }) =
    (if negative then
        -1

     else
        1
    )
        * List.foldl (\newDigit oldNumber -> oldNumber * 10 + newDigit) 0 digits


digitalNumberChangeDigit : (Int -> Int) -> DigitalNumber -> Int -> DigitalNumber
digitalNumberChangeDigit f (DigitalNumber d) n =
    let
        newDigits =
            ListExtra.updateAt n f d.digits

        newValue =
            numberValue (DigitalNumber { d | digits = newDigits })
    in
    DigitalNumber
        { d
            | digits =
                if newValue < d.minValue || newValue > d.maxValue then
                    d.digits

                else
                    newDigits
        }


increaseDigit : DigitalNumber -> Int -> DigitalNumber
increaseDigit =
    let
        increaseDigitHelper d =
            if d < 9 then
                d + 1

            else
                d
    in
    digitalNumberChangeDigit increaseDigitHelper


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


decreaseDigit : DigitalNumber -> Int -> DigitalNumber
decreaseDigit =
    let
        decreaseDigitHelper d =
            if d > 0 then
                d - 1

            else
                d
    in
    digitalNumberChangeDigit decreaseDigitHelper


numberOfDigits : DigitalNumber -> Int
numberOfDigits (DigitalNumber { minValue, maxValue }) =
    String.length <| String.fromInt <| max (abs minValue) (abs maxValue)


toChars : DigitalNumber -> List Char
toChars (DigitalNumber { digits }) =
    List.map (\x -> fromCode (x + asciiCodeForZero)) digits
