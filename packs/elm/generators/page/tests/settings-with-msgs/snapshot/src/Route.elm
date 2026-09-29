module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Home
    | SamplePage
    | Settings
    -- olifant:routes


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map SamplePage (s "sample-page")
        , Parser.map Settings (s "settings")
        -- olifant:route-parsers
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
