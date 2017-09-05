import Html exposing (Html, a, button, code, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Task exposing (attempt, succeed)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, string, stringParam, top)



main =
  Navigation.program UrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { route : Maybe Route
  }

initModel : Maybe Route -> Model
initModel r = Model r

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initModel (Url.parseHash route location)
    , Cmd.none
    )

-- URL PARSING

type Route
  = NameOnly String
  | NameAndAlbum String String


route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map NameOnly string
    , Url.map NameAndAlbum (string </> string)
    ]

-- UPDATE


type Msg
  = UrlChange Navigation.Location

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      ( initModel (Url.parseHash route location) 
      , Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Links" ]
    , h1 [] [ text "History" ]
    , case model.route of
        Nothing -> text "No User Specified"
        Just r -> 
            case r of
                NameOnly n -> text n
                NameAndAlbum n a -> text (n ++ a)
    ]
