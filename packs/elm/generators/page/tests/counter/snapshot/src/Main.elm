module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Page.Home as Home
import Page.SamplePage as SamplePage
import Page.Counter as Counter
-- olifant:page-imports
import Route exposing (Route)
import Url exposing (Url)



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | Home Home.Model
    | SamplePage SamplePage.Model
    | Counter Counter.Model
    -- olifant:pages


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) { key = key, page = NotFound }



-- UPDATE


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotHomeMsg Home.Msg
    | GotSamplePageMsg SamplePage.Msg
    | GotCounterMsg Counter.Msg
    -- olifant:page-msgs


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Home ->
            Home.init |> updateWith Home GotHomeMsg model

        Just Route.SamplePage ->
            SamplePage.init |> updateWith SamplePage GotSamplePageMsg model

        Just Route.Counter ->
            Counter.init |> updateWith Counter GotCounterMsg model

        -- olifant:route-inits


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home |> updateWith Home GotHomeMsg model

        ( GotSamplePageMsg subMsg, SamplePage page ) ->
            SamplePage.update subMsg page |> updateWith SamplePage GotSamplePageMsg model

        ( GotCounterMsg subMsg, Counter page ) ->
            Counter.update subMsg page |> updateWith Counter GotCounterMsg model

        -- olifant:page-updates

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }, Cmd.map toMsg subCmd )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "Not found", body = [ Html.text "Not found" ] }

        Home home ->
            viewPage GotHomeMsg (Home.view home)

        SamplePage page ->
            viewPage GotSamplePageMsg (SamplePage.view page)

        Counter page ->
            viewPage GotCounterMsg (Counter.view page)

        -- olifant:page-views


viewPage : (subMsg -> Msg) -> { title : String, body : List (Html.Html subMsg) } -> Browser.Document Msg
viewPage toMsg { title, body } =
    { title = title, body = List.map (Html.map toMsg) body }



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
