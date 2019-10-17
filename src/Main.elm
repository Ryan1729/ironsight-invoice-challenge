module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes
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


times amount (Cents c1) =
    Cents (amount * c1)


defaultDollarString =
    "$0.00"


displayAsDollars : Cents -> String
displayAsDollars (Cents totalCents) =
    if totalCents < 0 then
        defaultDollarString

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


centsFromDollarString : String -> Maybe Cents
centsFromDollarString s =
    let
        trimmed =
            if String.startsWith "$" s then
                String.dropLeft 1 s

            else
                s

        parsed =
            String.toFloat trimmed
    in
    Maybe.map
        (\dollars ->
            -- This could introduce some loss of precision, but we'll assume it's okay.
            dollars * 100 |> round |> Cents
        )
        parsed


type alias LineItem =
    { description : String
    , perUnit : Field Cents
    , quantity : Field Int
    }


defaultLineItem : LineItem
defaultLineItem =
    { description = ""
    , perUnit = fieldFromString centsFromDollarString defaultDollarString
    , quantity = fieldFromString String.toInt "0"
    }


getSubtotal : List LineItem -> Cents
getSubtotal items =
    List.foldl
        (\item acc ->
            let
                perUnit =
                    item.perUnit |> fieldWithDefault (Cents 0)

                quantity =
                    item.quantity |> fieldWithDefault 0
            in
            perUnit |> times quantity |> add acc
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
    | SetItemDescription Int String
    | SetPerUnit Int String
    | SetQuantity Int String
    | SetTaxRate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInvoiceDescription description ->
            ( { model | description = description }, Cmd.none )

        AddLineItem ->
            ( { model | items = defaultLineItem :: model.items }, Cmd.none )

        RemoveLineItem i ->
            ( { model | items = removeAt i model.items }, Cmd.none )

        SetItemDescription i str ->
            ( { model | items = mapAt (\item -> { item | description = str }) i model.items }, Cmd.none )

        SetPerUnit i str ->
            ( { model | items = mapAt (\item -> { item | perUnit = fieldFromString centsFromDollarString str }) i model.items }, Cmd.none )

        SetQuantity i str ->
            ( { model | items = mapAt (\item -> { item | quantity = fieldFromString String.toInt str }) i model.items }, Cmd.none )

        SetTaxRate str ->
            ( { model | taxRate = fieldFromString String.toFloat str }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input "Invoice Description" model.description SetInvoiceDescription
        , button [ onClick AddLineItem ] [ text "Add Line Item" ] :: List.indexedMap viewLineItem model.items |> div []
        , div []
            [ text "Subtotal:", model.items |> getSubtotal |> displayAsDollars |> text ]
        , fieldInput "Tax Rate" model.taxRate SetTaxRate
        , div [] [ text "Total:", model |> getTotal |> displayAsDollars |> text ]
        ]


viewLineItem : Int -> LineItem -> Html Msg
viewLineItem i item =
    div []
        [ input "Item Description" item.description (SetItemDescription i)
        , fieldInput "Per Unit" item.perUnit (SetPerUnit i)
        , fieldInput "Quantity" item.quantity (SetQuantity i)
        , button [ i |> RemoveLineItem |> onClick ] [ text "Remove Line Item" ]
        ]


fieldInput placeholder value msg =
    input placeholder (fieldToString value) msg


input placeholder value msg =
    Html.input [ Html.Attributes.placeholder placeholder, Html.Attributes.value value, onInput msg ] []



-- Utils


removeAt i list =
    List.take i list ++ List.drop (i + 1) list


mapAt mapper i list =
    let
        secondPart =
            List.drop i list

        mapped =
            List.head secondPart |> Maybe.map mapper
    in
    case mapped of
        Just m ->
            List.take i list ++ (m :: List.drop 1 secondPart)

        Nothing ->
            list



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
