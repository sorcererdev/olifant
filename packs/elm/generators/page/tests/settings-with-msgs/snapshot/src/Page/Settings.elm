module Page.Settings exposing (Model, Msg, init, update, view)

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
    | ToggleDarkMode
    | SetDisplayName String
    | GotArticles (List String)
    -- olifant:msgs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleDarkMode ->
            ( model, Cmd.none )

        SetDisplayName _ ->
            ( model, Cmd.none )

        GotArticles _ ->
            ( model, Cmd.none )

        -- olifant:update-branches



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view _ =
    { title = "Settings"
    , body = [ h1 [] [ text "Settings" ] ]
    }
