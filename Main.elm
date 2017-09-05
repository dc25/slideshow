import Html exposing (Html, a, button, code, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (string, Decoder)
import Task exposing (attempt, succeed, andThen)
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
  { photoIds : Maybe (List String)
  }

decodeUserId : Decode.Decoder String
decodeUserId =
    Decode.at ["user", "id"] Decode.string

decodePublicPhotos : Decode.Decoder (List String)
decodePublicPhotos =
    Decode.at ["photos", "photo"] (Decode.list <| (Decode.at ["id"]) Decode.string)

userIdUrl : String -> String
userIdUrl n = "https://api.flickr.com/services/rest/?&method=flickr.people.findByUserName&api_key=859b1fdf671b6419805ec3d2c7578d70&username=" ++ n ++ "&format=json&nojsoncallback=1"

publicPhotosUrl : String -> String
publicPhotosUrl uid = "https://api.flickr.com/services/rest/?&method=flickr.people.getPublicPhotos&api_key=4ef2fe2affcdd6e13218f5ddd0e2500d&user_id=" ++ uid ++ "&format=json&nojsoncallback=1"

initModel : Maybe Route -> (Model, Cmd Msg)
initModel r = 
  let cmd = case r of 
    Nothing -> 
      Cmd.none

    Just (NameOnly n) -> 
      let req = Http.get (userIdUrl n) decodeUserId 
          userIdTask = Http.toTask req
          publicPhotosTask uid = 
              Http.toTask (Http.get (publicPhotosUrl uid) decodePublicPhotos)
      in Task.attempt SetPhotoIds (userIdTask |> (andThen publicPhotosTask ))

  in (Model Nothing, cmd)

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    initModel (Url.parseHash route location)

-- URL PARSING

type Route
  = NameOnly String


route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map NameOnly Url.string
    ]

-- UPDATE

type Msg
  =   UrlChange Navigation.Location
    | SetPhotoIds (Result Http.Error (List String))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      initModel (Url.parseHash route location) 

    SetPhotoIds (Ok photoIds) ->
      (Model (Just photoIds), Cmd.none)

    SetPhotoIds (Err _) ->
      (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ case model.photoIds of
        Nothing -> text "No User ID"
        Just pids -> div [] (List.map (\id -> div [] [text id]) pids)

    ]
