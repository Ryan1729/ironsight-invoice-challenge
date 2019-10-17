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
    , taxRate : Field Float
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { description = ""
      , items = [ defaultLineItem ]
      , taxRate = fieldFromString String.toFloat "0.0"
      }
    , Cmd.none
    )


getTotal : Model -> Cents
getTotal model =
    case getSubtotal model.items of
        Cents intCents ->
            let
                cents =
                    toFloat intCents

                taxTotal =
                    (model.taxRate |> fieldWithDefault 0) * cents
            in
            taxTotal + cents |> round |> Cents


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


type Field a
    = Field (Maybe a) String


fieldToString (Field _ s) =
    s


fieldWithDefault default (Field parsed _) =
    Maybe.withDefault default parsed


fieldFromString mapper s =
    Field (mapper s) s



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
            ( { model | taxRate = fieldFromString String.toFloat str }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Invoice Description", value model.description, onInput SetInvoiceDescription ]
            []
        , button [ onClick AddLineItem ] [ text "Add Line Item" ] :: List.indexedMap viewLineItem model.items |> div []
        , div []
            [ text "Subtotal:", model.items |> getSubtotal |> displayAsDollars |> text ]
        , input
            [ placeholder "Tax Rate", model.taxRate |> fieldToString |> value, onInput SetTaxRate ]
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
