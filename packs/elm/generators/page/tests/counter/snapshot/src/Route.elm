module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Home
    | SamplePage
    | Counter


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map SamplePage (s "sample-page")
        , Parser.map Counter (s "counter")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
