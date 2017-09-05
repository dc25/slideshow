import Html exposing (Html, div, text)
import Html.Attributes as HA 
import Html.Events as HE 
import Http
import Svg exposing (Svg, svg, polygon)
import Svg.Attributes as SA 
import List exposing (take)
import Json.Decode as DC exposing (Decoder)
import Task exposing (andThen)
import Navigation
import UrlParser as Url 

main =
  Navigation.program UrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = \m -> Sub.none
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

decodePhotos : DC.Decoder (List PhotoSpec)
decodePhotos =
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
              Http.toTask (Http.get (publicPhotosUrl uid) decodePhotos)
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

type Direction = Left | Right

type Msg
  =   UrlChange Navigation.Location
    | SetPhotoIds (Result Http.Error (List PhotoSpec))
    | ScrollPick Direction

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      initModel (Url.parseHash route location) 

    SetPhotoIds (Ok photoIds) ->
      (Model (Ok {left=[], right=photoIds}), Cmd.none)

    SetPhotoIds (Err e) ->
      (Model (Err e), Cmd.none)

    ScrollPick dir ->
      case model.photoIds of
        Ok s -> let ns = case dir of
                           Right -> {left=List.take 1 s.right ++ s.left, right=List.drop 1 s.right}
                           Left ->  {left=List.drop 1 s.left, right=List.take 1 s.left ++ s.right}
                in (Model (Ok ns), Cmd.none)

        Err e -> (model, Cmd.none)

-- VIEW

arrow : Direction -> Svg Msg
arrow dir = polygon [ SA.points (if (dir == Left) 
                                 then "-80,-10 -80,10 -90,0" 
                                 else "80,-10 80,10 90,0")
                    , SA.style "fill: red"
                    , HE.onClick (ScrollPick dir)
                    ] 
                    []

svgArrows : (Bool, Bool) -> Html Msg
svgArrows (vl,vr) = 
    let al = if (vl) then [arrow Left] else []
        ar = if (vr) then [arrow Right] else []
    in svg [ SA.version "1.1"
           , SA.width "100%" 
           , SA.height "100%" 
           , SA.viewBox "-100 -100 200 200" 
           , SA.preserveAspectRatio "none"
           ]
           (al ++ ar)

photoUrl : PhotoSpec -> String
photoUrl ps = 
     "https://farm" ++ toString ps.farm ++ ".staticflickr.com/" 
  ++ ps.server ++ "/" 
  ++ ps.id ++ "_" ++ ps.secret ++ "_b.jpg"

photoInDiv : (Bool, Bool) -> PhotoSpec -> Html Msg
photoInDiv vis ps = div [HA.style [ ("height", "100%")
                                  , ("width", "100%")
                                  , ("background", "url('" ++ photoUrl ps ++ "') center center no-repeat grey")
                                  ]
                        ] 
                        [svgArrows vis]

view : Model -> Html Msg
view model =
  div []
      [ case model.photoIds of
          Err s -> 
              text "Http Error"

          Ok scroll -> 
              let lv = List.length (scroll.left) > 0
                  rv = List.length (scroll.right) > 1
              in div [HA.style [("height","500px"), ("width", "800px")] ] 
                     (List.map (photoInDiv (lv,rv)) <| take 1 scroll.right)
      ]
