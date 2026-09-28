module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, h1, text)



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Reset
    -- olifant:msgs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( model, Cmd.none )

        -- olifant:update-branches



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view _ =
    { title = "Home"
    , body = [ h1 [] [ text "Home" ] ]
    }
