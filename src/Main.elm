module Main exposing (init)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { title : String
    , h1 : String
    , sections : List Section
    , activeLink : Maybe Int
    }


type alias Section =
    { name : String
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , anchor : String
    , target : String
    , extra : Element Msg
    }


initialModel : Model
initialModel =
    { title = "Perpetually Peregrine"
    , h1 = "Web Directory"
    , sections =
        [ Section "Art"
            [ Entry 1
                "Jakub Rozalski"
                "https://jrozalski.com/"
                (row []
                    [ el [] (text "Art used in ")
                    , el [ Font.italic ] (text "Scythe")
                    , el [] (text " (board game)")
                    ]
                )
            ]
        , Section "Shopping"
            [ Entry 2
                "Higher Hacknell"
                "https://www.higherhacknell.co.uk/cat/organic-wool-and-sheepskins"
                (el [] (text "Wool and sheepskins - met the farmer in Romania at Count Kalnoky's estate"))
            , Entry 3
                "Redbubble - Appa"
                "https://www.redbubble.com/shop/appa"
                (row []
                    [ el [] (text "Appa-related merchandise (from ")
                    , el [ Font.italic ] (text "Avatar: The Last Airbender")
                    , el [] (text ")")
                    ]
                )
            , Entry 4
                "Rose Colored Gaming"
                "https://rosecoloredgaming.com/"
                (el [] (text "Display stands for consoles, controllers, cartridges"))
            ]
        , Section "Trees"
            [ Entry 5
                "Christmas Tree Farms in Germany"
                "https://www.pickyourownchristmastree.org/DUxmastrees.php"
                Element.none
            , Entry 6
                "Monumental Trees"
                "https://www.monumentaltrees.com/en/"
                Element.none
            , Entry 7
                "Monumental Trees in Bavaria"
                "https://www.monumentaltrees.com/en/records/deu/bavaria/"
                Element.none
            ]
        , Section "Video Games"
            [ Entry 8
                "Wii & Wii U Modding Guide"
                "https://sites.google.com/site/completesg/home"
                Element.none
            ]
        ]
    , activeLink = Nothing
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initialModel, Cmd.none )



-- Update


type Msg
    = HoveredLink Int
    | UnHoveredLink


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoveredLink id ->
            ( { model | activeLink = Just id }, Cmd.none )

        UnHoveredLink ->
            ( { model | activeLink = Nothing }, Cmd.none )



-- View


edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


colors =
    { primary = rgb 0.2 0.72 0.91
    , success = rgb 0.275 0.533 0.278
    , warning = rgb 0.8 0.2 0.2
    , link = rgb 0.361 0.502 0.737
    , black = rgb 0.05 0.05 0.05
    , darkgrey = rgb 0.31 0.31 0.31
    , lightgrey = rgb 0.733 0.733 0.733
    , white = rgb 0.99 0.99 0.99
    }


view : Model -> Browser.Document Msg
view model =
    Browser.Document model.title
        [ Element.layout
            [ Font.color colors.darkgrey
            , Font.family [ Font.typeface "Georgia", Font.serif ]
            , paddingEach { edges | top = 20, left = 30, right = 30 }
            , Background.color colors.white
            ]
          <|
            column [ width fill, spacing 25 ]
                (viewPageHeading model.h1
                    :: List.map (viewSection model.activeLink) model.sections
                )
        ]


viewPageHeading : String -> Element msg
viewPageHeading heading =
    el
        [ Font.size 40
        , Font.heavy
        , paddingEach { edges | bottom = 10 }
        ]
        (text heading)


viewSection : Maybe Int -> Section -> Element Msg
viewSection activeLink section =
    column [ spacing 8 ]
        (el
            [ Font.size 28
            , Font.heavy
            , paddingEach { edges | bottom = 10 }
            ]
            (text section.name)
            :: List.map (viewEntry activeLink) section.entries
        )


viewEntry : Maybe Int -> Entry -> Element Msg
viewEntry activeLink entry =
    let
        baseAttributes =
            [ Font.color colors.link
            , Events.onMouseEnter (HoveredLink entry.id)
            , Events.onMouseLeave UnHoveredLink
            ]

        attributes =
            case activeLink of
                Just id ->
                    if id == entry.id then
                        Font.underline :: baseAttributes

                    else
                        baseAttributes

                Nothing ->
                    baseAttributes
    in
    column [ paddingEach { edges | left = 40 } ]
        [ link attributes { url = entry.target, label = text entry.anchor }
        , entry.extra
        ]



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
