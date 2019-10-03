module CounterAdder exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Events exposing (onClick)
import Mark



-- Entry point


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { values = Dict.empty
            }
        , view = view
        , update = update
        }



-- Model


type alias Model =
    { values : Values
    }



-- Values


type alias Values =
    Dict.Dict String Float


getValue : String -> Values -> Float
getValue name values =
    values
        |> Dict.get name
        -- Values that are not kept track of yet are assumed to be the default
        |> Maybe.withDefault 0



-- Update


type Msg
    = SetValue ( String, Float )


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetValue ( key, val ) ->
            { model
                | values =
                    Dict.insert key
                        val
                        model.values
            }



-- View


view : Model -> Html Msg
view model =
    case compileMarkup markup of
        -- The success case yields a function that takes the current `Values` dictionary
        Ok viewByData ->
            viewByData model.values
                -- Map to the program message
                |> map SetValue

        Err err ->
            text err


markup : String
markup =
    """
|> Title
    Interactive counteradder

|> Counter
    name = var1

|> Counter
    name = var2
    
|> Sum
    arg1 = var1
    arg2 = var2
"""


compileMarkup : String -> Result String (Values -> Html ( String, Float ))
compileMarkup markdownBody =
    Mark.compile
        (Mark.document
            identity
            (Mark.manyOf
                [ titleBlock
                , counterBlock
                , sumBlock
                ]
            )
        )
        markdownBody
        |> (\res ->
                case res of
                    Mark.Success blocks ->
                        Ok
                            (\data ->
                                div []
                                    (List.map
                                        -- Inject data into each block
                                        -- This makes them into regular `elm-html` nodes
                                        (\block -> block data)
                                        blocks
                                    )
                            )

                    _ ->
                        Err "Compile error"
           )



-- Markup blocks


{-| Title block, renders to h1 [][ text _ ]
-}
titleBlock : Mark.Block (Values -> Html msg)
titleBlock =
    Mark.block "Title"
        (\str _ ->
            h1 [] [ text str ]
        )
        Mark.string


{-| Counter block, renders to `var = [+] value [-]`
-}
counterBlock : Mark.Block (Values -> Html ( String, Float ))
counterBlock =
    Mark.record "Counter"
        (\name values ->
            let
                value =
                    getValue name values
            in
            div
                []
                [ text (name ++ " = ")
                , button
                    [ onClick ( name, value - 1 )
                    ]
                    [ text "-"
                    ]
                , text (String.fromFloat value)
                , button
                    [ onClick ( name, value + 1 )
                    ]
                    [ text "+"
                    ]
                ]
        )
        |> Mark.field "name" Mark.string
        |> Mark.toBlock


{-| Renders the sum of two values controlled through counters, renders to `var1 + var2 == value`
-}
sumBlock : Mark.Block (Values -> Html ( String, Float ))
sumBlock =
    Mark.record "Sum"
        (\arg1 arg2 values ->
            let
                res =
                    getValue arg1 values + getValue arg2 values
            in
            div
                []
                [ text (arg1 ++ " + " ++ arg2 ++ " == ")
                , text (String.fromFloat res)
                ]
        )
        |> Mark.field "arg1" Mark.string
        |> Mark.field "arg2" Mark.string
        |> Mark.toBlock
