import Html exposing (Html, a, button, code, div, h1, li, text, ul)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
import List exposing (take)
import Json.Decode as DC exposing (string, Decoder)
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

type alias Scroll 
  = {left : List PhotoSpec, right : List PhotoSpec}

type alias Model =
  { photoIds : Result Http.Error Scroll
  }

decodeUserId : DC.Decoder String
decodeUserId =
    DC.at ["user", "id"] DC.string

type alias PhotoSpec = { id: String
                       , secret: String
                       , server: String
                       , farm: Int
                       }

decodePublicPhotos : DC.Decoder (List PhotoSpec)
decodePublicPhotos =
    DC.at ["photos", "photo"] 
        (DC.list <| DC.map4 PhotoSpec 
                                ((DC.at ["id"]) DC.string) 
                                ((DC.at ["secret"]) DC.string)
                                ((DC.at ["server"]) DC.string)
                                ((DC.at ["farm"]) DC.int)     
        )

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
          userPhotosTask = userIdTask |> (andThen publicPhotosTask )
      in Task.attempt SetPhotoIds userPhotosTask

  in (Model (Ok {left=[], right=[]}), cmd)

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
    | SetPhotoIds (Result Http.Error (List PhotoSpec))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      initModel (Url.parseHash route location) 

    SetPhotoIds (Ok photoIds) ->
      (Model (Ok {left=[], right=photoIds}), Cmd.none)

    SetPhotoIds (Err e) ->
      (Model (Err e), Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

photoUrl : PhotoSpec -> String
photoUrl ps = "https://farm" ++ toString ps.farm ++ ".staticflickr.com/" ++ ps.server ++ "/" ++ ps.id ++ "_" ++ ps.secret ++ ".jpg"

photoInDiv : PhotoSpec -> Html Msg
photoInDiv ps = div [style [ ("height", "100%")
                           , ("width", "100%")
                           , ("background", "url('" ++ photoUrl ps ++ "')")
                           , ("background-repeat", "no-repeat")
                           , ("background-position", "center center")
                           , ("background-color", "grey")]] []

-- https://stackoverflow.com/questions/1719452/how-to-make-a-div-always-full-screen

view : Model -> Html Msg
view model =
  div []
    [ case model.photoIds of
        Err s -> text "Http Error"
        Ok scroll -> div [ style [("height","500px"), ("width", "800px")]  ] (List.map photoInDiv <| take 1 scroll.right)

    ]
