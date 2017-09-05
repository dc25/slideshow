import Html exposing (Html, a, button, code, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (string, Decoder)
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
  { userId : Maybe String
  }

decodeUserId : Decode.Decoder String
decodeUserId =
    Decode.at ["user", "id"] Decode.string

userIdUrl : String -> String
userIdUrl n = "https://api.flickr.com/services/rest/?&method=flickr.people.findByUserName&api_key=859b1fdf671b6419805ec3d2c7578d70&username=" ++ n ++ "&format=json&nojsoncallback=1"

initModel : Maybe Route -> (Model, Cmd Msg)
initModel r = 
  let cmd = case r of 
    Nothing -> 
      Cmd.none

    Just (NameOnly n) -> 
      Http.send SetUserId (Http.get (userIdUrl n) decodeUserId )

    Just (NameAndAlbum n a) -> 
      Http.send SetUserId (Http.get (userIdUrl n) decodeUserId )

  in (Model Nothing, cmd)

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    initModel (Url.parseHash route location)

-- URL PARSING

type Route
  = NameOnly String
  | NameAndAlbum String String


route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map NameOnly Url.string
    , Url.map NameAndAlbum (Url.string </> Url.string)
    ]

-- UPDATE

type Msg
  =   UrlChange Navigation.Location
    | SetUserId (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      initModel (Url.parseHash route location) 

    SetUserId (Ok userId) ->
      (Model (Just userId), Cmd.none)

    SetUserId (Err _) ->
      (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ case model.userId of
        Nothing -> text "No User Found"
        Just uid -> text uid

    ]
