module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { description : String
    , items : List LineItem
    , taxRate : Float
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { description = ""
      , items = [ defaultLineItem ]
      , taxRate = 0.0
      }
    , Cmd.none
    )


getTotal : Model -> Cents
getTotal model =
    case getSubtotal model.items of
        Cents cents ->
            toFloat cents * model.taxRate |> round |> Cents


type Cents
    = Cents Int


add (Cents c1) (Cents c2) =
    Cents (c1 + c2)


times (Cents c1) amount =
    Cents (c1 * amount)


displayAsDollars : Cents -> String
displayAsDollars (Cents totalCents) =
    if totalCents < 0 then
        "$0.00"

    else
        let
            dollars =
                totalCents // 100

            cents =
                remainderBy 100 totalCents

            centsPrefix =
                if cents < 10 then
                    "0"

                else
                    ""
        in
        String.concat [ "$", dollars |> String.fromInt, ".", centsPrefix, cents |> String.fromInt ]


type alias LineItem =
    { description : String
    , perUnit : Cents
    , quantity : Int
    }


defaultLineItem : LineItem
defaultLineItem =
    { description = ""
    , perUnit = Cents 0
    , quantity = 0
    }


getSubtotal : List LineItem -> Cents
getSubtotal items =
    List.foldl
        (\item acc ->
            times item.perUnit item.quantity |> add acc
        )
        (Cents 0)
        items



-- UPDATE


type Msg
    = SetInvoiceDescription String
    | AddLineItem
    | RemoveLineItem Int
    | SetTaxRate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInvoiceDescription description ->
            ( { model | description = description }, Cmd.none )

        AddLineItem ->
            ( { model | items = defaultLineItem :: model.items }, Cmd.none )

        RemoveLineItem i ->
            ( { model | items = List.take i model.items ++ List.drop (i + 1) model.items }, Cmd.none )

        SetTaxRate str ->
            ( String.toFloat str |> Maybe.map (\taxRate -> { model | taxRate = taxRate }) |> Maybe.withDefault model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Invoice Description", value model.description, onInput SetInvoiceDescription ]
            []
        , button [ onClick AddLineItem ] [ text "Add Line Item" ]
        , div [] (List.indexedMap viewLineItem model.items)
        , div []
            [ text "Subtotal:", model.items |> getSubtotal |> displayAsDollars |> text ]
        , input
            [ placeholder "Tax Rate", model.taxRate |> String.fromFloat |> value, onInput SetTaxRate ]
            []
        , div [] [ text "Total:", model |> getTotal |> displayAsDollars |> text ]
        ]


viewLineItem : Int -> LineItem -> Html Msg
viewLineItem i item =
    div []
        [ text item.description
        , item.perUnit |> displayAsDollars |> text
        , item.quantity |> String.fromInt |> text
        , button [ i |> RemoveLineItem |> onClick ] [ text "Remove Line Item" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
