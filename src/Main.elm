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
    , directory : Directory
    , activeLink : Maybe String
    }


type Directory
    = Directory (List Entry)


type Entry
    = Section String (List Entry)
    | Entry String String (Maybe (Element Msg))


initialModel : Model
initialModel =
    { title = "Perpetually Peregrine"
    , h1 = "Web Directory"
    , directory =
        Directory
            [ Section "Airplanes"
                [ Entry "Lufthansa Surprise"
                    "https://www.lufthansa-surprise.com/"
                    (Just <|
                        text "Travel around Europe to a surprise destination"
                    )
                , Entry "OurAirports"
                    "https://ourairports.com/"
                    (Just <|
                        text "Information about all of the world's airports"
                    )
                ]
            , Section "Art"
                [ Entry "Jakub Rozalski"
                    "https://jrozalski.com/"
                    (Just <|
                        row []
                            [ text "Art used in "
                            , el [ Font.italic ] (text "Scythe")
                            , text " (board game)"
                            ]
                    )
                ]
            , Section "Astronomy"
                [ Entry "Heavens-Above"
                    "https://www.heavens-above.com/"
                    (Just <|
                        text "Track satellites and other objects in space"
                    )
                ]
            , Section "Coins"
                [ Entry "Swiss coin mintage figures"
                    "https://www.swissmint.ch/e/dokumentation/publikationen/liste.php"
                    Nothing
                , Entry "Swiss coins in circulation"
                    "https://www.snb.ch/en/iabout/cash/id/cash_coins#t2"
                    Nothing
                ]
            , Section "Cycling"
                [ Entry "Bavarian Network for Cyclists"
                    "http://www.bayerninfo.de/en/bike"
                    Nothing
                , Entry "EuroVelo"
                    "https://en.eurovelo.com/"
                    (Just <| text "European cycle routes")
                , Entry "Sheldon Brown"
                    "https://www.sheldonbrown.com/"
                    (Just <| text "Great technical information on bicycles")
                ]
            , Section "Dry Stone Walling"
                [ Entry "Dry Stone Walling Association"
                    "https://www.dswa.org.uk/"
                    Nothing
                , Entry "The Stone Trust"
                    "https://thestonetrust.org/"
                    (Just <|
                        text <|
                            "North American facility for dry stone walling "
                                ++ "education"
                    )
                ]
            , Section "Personal Finance"
                [ Entry "Frugalwoods"
                    "https://www.frugalwoods.com/"
                    (Just <|
                        text "Blog on financial independence and simple living"
                    )
                ]
            , Section "Reviews"
                [ Entry "Flashlight information"
                    "http://lygte-info.dk/"
                    (Just <|
                        text <|
                            "Basically the most comprehensive website on the "
                                ++ "Internet for information about "
                                ++ "flashlights, batteries, and chargers"
                    )
                ]
            , Section "Search"
                [ Entry "Wiby"
                    "https://wiby.me/"
                    (Just <| text "Search engine for classic websites")
                ]
            , Section "Shaving"
                [ Entry "Badger & Blade"
                    "https://www.badgerandblade.com/forum/forums/"
                    (Just <|
                        text <|
                            "Forum with extensive info about shaving products & methods"
                    )
                ]
            , Section "Shopping"
                [ Entry "Higher Hacknell"
                    "https://www.higherhacknell.co.uk/cat/organic-wool-and-sheepskins"
                    (Just <|
                        text <|
                            "Wool and sheepskins - met the farmer in Romania "
                                ++ "at Count Kalnoky's estate"
                    )
                , Entry "Redbubble - Appa"
                    "https://www.redbubble.com/shop/appa"
                    (Just <|
                        row []
                            [ text "Appa-related merchandise (from "
                            , el [ Font.italic ]
                                (text "Avatar: The Last Airbender")
                            , text ")"
                            ]
                    )
                , Entry "Rose Colored Gaming"
                    "https://rosecoloredgaming.com/"
                    (Just <|
                        text <|
                            "Display stands for consoles, controllers, "
                                ++ "cartridges"
                    )
                , Section "Bicycles"
                    [ Entry "Rodriguez Bicycles"
                        "https://www.rodbikes.com/"
                        (Just <| text "Custom bicycles and tandems")
                    , Entry "SOMA Fabrications"
                        "https://www.somafab.com/"
                        Nothing
                    ]
                , Section "Expensive Stuff"
                    [ Entry "Bellerby and Co Globemakers"
                        "https://bellerbyandco.com/"
                        (Just <| text "Handcrafted, personalised globes")
                    , Entry "AMG"
                        ("http://www.high-fidelity-studio.de/"
                            ++ "high-fidelitystudio/Produkte/Seiten/AMG.html"
                        )
                        (Just <| text "High-end turntables")
                    ]
                ]
            , Section "Trains"
                [ Entry "Carte du réseau ferré en France"
                    ("https://www.sncf-reseau.com/fr/carte/"
                        ++ "carte-reseau-ferre-en-france"
                    )
                    (Just <| text "French railway infrastructure map")
                , Entry "Deutsche Bahn"
                    "https://www.bahn.com/en/view/index.shtml"
                    (Just <| text "German railway operator")
                , Entry "DB Netze Fahrweg"
                    "https://geovdbn.deutschebahn.com/pgv/public/map/isr.xhtml"
                    (Just <| text "German railway infrastructure map")
                , Entry "The Man in Seat Sixty-One"
                    "https://www.seat61.com/"
                    (Just <|
                        text <|
                            "Train travel guide for Europe and the rest of "
                                ++ "the world"
                    )
                , Entry "OpenRailwayMap"
                    "https://www.openrailwaymap.org/"
                    Nothing
                , Entry "vagonWEB"
                    "https://www.vagonweb.cz/"
                    (Just <| text "Information on composition of trains")
                ]
            , Section "Trees"
                [ Entry "Christmas Tree Farms in Germany"
                    "https://www.pickyourownchristmastree.org/DUxmastrees.php"
                    Nothing
                , Entry "Monumental Trees"
                    "https://www.monumentaltrees.com/en/"
                    Nothing
                , Entry "Monumental Trees in Bavaria"
                    "https://www.monumentaltrees.com/en/records/deu/bavaria/"
                    Nothing
                , Entry "The Wood Database"
                    "https://www.wood-database.com/"
                    Nothing
                ]
            , Section "Video Games"
                [ Entry "Analogue Super Nt Firmware Updates"
                    ("https://support.analogue.co/hc/en-us/articles/"
                        ++ "360000557452-Super-Nt-Firmware-Update-v4-9"
                    )
                    Nothing
                , Entry "Wii & Wii U Modding Guide"
                    "https://sites.google.com/site/completesg/home"
                    Nothing
                , Entry "Witgui"
                    "https://desairem.com/wordpress/witgui/"
                    (Just <| text "Wii & GameCube game manager for macOS")
                ]
            , Section "Weather"
                [ Entry "Weather report"
                    "http://wttr.in/"
                    (Just <| text "Text-based local weather forecast")
                ]
            ]
    , activeLink = Nothing
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initialModel, Cmd.none )



-- Update


type Msg
    = HoveredLink String
    | UnHoveredLink


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoveredLink link ->
            ( { model | activeLink = Just link }, Cmd.none )

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
    , black = rgb 0.067 0.067 0.067
    , darkgrey = rgb 0.31 0.31 0.31
    , lightgrey = rgb 0.733 0.733 0.733
    , white = rgb 0.99 0.99 0.973
    }


view : Model -> Browser.Document Msg
view model =
    let
        sections =
            case model.directory of
                Directory dirSections ->
                    dirSections
    in
    Browser.Document model.title
        [ Element.layout
            [ Font.color colors.black
            , Font.family [ Font.typeface "Georgia", Font.serif ]
            , Background.color <| rgb 0.99 0.99 0.99
            ]
          <|
            row
                [ height fill
                , width fill
                ]
                [ column [ width <| fillPortion 1 ] []
                , column
                    [ width <| fillPortion 30
                    , paddingEach
                        { edges
                            | top = 20
                            , left = 30
                            , right = 30
                            , bottom = 20
                        }
                    , spacing 25
                    , Background.color colors.white
                    ]
                    (viewPageHeading model.h1
                        :: List.map (viewSection 1 model.activeLink) sections
                    )
                , column [ width <| fillPortion 1 ] []
                ]
        ]


viewPageHeading : String -> Element msg
viewPageHeading heading =
    el
        [ Font.size 48
        , Font.heavy
        , paddingEach { edges | bottom = 10 }
        ]
        (text heading)


viewSection : Int -> Maybe String -> Entry -> Element Msg
viewSection level activeLink entry =
    case entry of
        Section name entries ->
            column [ spacing 16 ]
                (el
                    [ Font.size (32 - (level - 1) * 4)
                    , Font.italic
                    , paddingEach
                        { edges
                            | bottom = 0
                            , left = 40 * (level - 1)
                        }
                    ]
                    (text name)
                    :: List.map (viewEntry level activeLink) entries
                )

        Entry anchor target extra ->
            Element.none


viewEntry : Int -> Maybe String -> Entry -> Element Msg
viewEntry level activeLink entry =
    case entry of
        Entry anchor target extra ->
            let
                baseAttributes =
                    [ Font.color colors.link
                    , Events.onMouseEnter (HoveredLink anchor)
                    , Events.onMouseLeave UnHoveredLink
                    ]

                attributes =
                    case activeLink of
                        Just link ->
                            if link == anchor then
                                Font.underline :: baseAttributes

                            else
                                baseAttributes

                        Nothing ->
                            baseAttributes

                description =
                    case extra of
                        Just element ->
                            paragraph [ Font.size 16 ] [ element ]

                        Nothing ->
                            Element.none
            in
            column [ paddingEach { edges | left = 40 * level }, spacing 4 ]
                [ link attributes { url = target, label = text anchor }
                , description
                ]

        Section name entries ->
            viewSection (level + 1) activeLink entry



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
