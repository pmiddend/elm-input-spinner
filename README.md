# elm-number-spinner

This Elm component allows displaying and editing *single digits* inside *decimal numbers* (positive and negative). Here's a little video on how it works:

![How it works](https://github.com/pmiddend/elm-number-spinner/raw/main/howitworks.gif)

What's there:

- Completely keyboard-driven (if it has the focus)
- Mouse clickable
- Can have upper and lower bounds

What's missing:

- Styling

## Usage

Use the usual Elm Architecture types for `Model, view, update, init, Msg` inside [NumberSpinner.NumberSpinner](http://package.elm-lang.org/packages/pmiddend/elm-input-spinner/1.0.0/NumberSpinner-NumberSpinner) to use it. Note that internally, this library uses the excellent [decimal](https://package.elm-lang.org/packages/prikhi/decimal/latest) library to represent numbers. This is somewhat opaque to you as a user, as you can use the functions from [NumberSpinner.DecimalNumber](http://package.elm-lang.org/packages/pmiddend/elm-input-spinner/1.0.0/NumberSpinner-DecimalNumber) to construct/deconstruct decimal numbers.

To get to the value of the spinner (after an update message), call a function like `NumberSpinner.valueAsFloat` and process it further.

## Example code

```elm
module Main exposing ()

import NumberSpinner.DecimalNumber as Decimal
import NumberSpinner.NumberSpinner as NumberSpinner


type alias Model =
    { spinner : NumberSpinner.Model Msg
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { spinner =
                        NumberSpinner.init
                            2
                            (Decimal.fromInt 0)
                            (Decimal.fromInt 10)
                            (Maybe.withDefault (Decimal.fromInt 0) <| Decimal.fromString "1.23")
                            SpinnerMessage
                  }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = SpinnerMessage NumberSpinner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpinnerMessage subMsg ->
            let
                ( newModel, cmd ) =
                    NumberSpinner.update subMsg spinner
            in
            ( { model | spinner = newModel }, cmd )


view : Model -> Html Msg
view model =
    NumberSpinner.view model.spinner
```
