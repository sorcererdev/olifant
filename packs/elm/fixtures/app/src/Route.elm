module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Home
    | SamplePage
    -- olifant:routes


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map SamplePage (s "sample-page")
        -- olifant:route-parsers
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
