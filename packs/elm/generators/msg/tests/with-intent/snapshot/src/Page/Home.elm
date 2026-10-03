module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, h1, text)



-- MODEL


type alias Model =
    { count : Int }


init : ( Model, Cmd Msg )
init =
    ( { count = 0 }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( { count = 0 }, Cmd.none )



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view _ =
    { title = "Home"
    , body = [ h1 [] [ text "Home" ] ]
    }
