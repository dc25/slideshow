import Html exposing (Html, div, text)
import Html.Attributes as HA 
import Html.Events as HE 
import Http
import Maybe
import Svg exposing (Svg, svg, polygon)
import Svg.Attributes as SA 
import List exposing (take)
import Json.Decode as DC exposing (Decoder)
import Task exposing (andThen)
import Navigation
import UrlParser as Url exposing ((</>))

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
                       , description: Maybe String
                       }

initPhotoSpec : String -> String -> String -> Int -> PhotoSpec
initPhotoSpec id sec ser farm = PhotoSpec id sec ser farm Nothing

decodePhotoSpecList : DC.Decoder (List PhotoSpec)
decodePhotoSpecList =
    (DC.list <| DC.map4 initPhotoSpec
                          ((DC.at ["id"]) DC.string) 
                          ((DC.at ["secret"]) DC.string)
                          ((DC.at ["server"]) DC.string)
                          ((DC.at ["farm"]) DC.int)     
    )

decodePhotos : DC.Decoder (List PhotoSpec)
decodePhotos =
  DC.at ["photos", "photo"] decodePhotoSpecList

decodeAlbumPhotos : DC.Decoder (List PhotoSpec)
decodeAlbumPhotos =
  DC.at ["photoset", "photo"] decodePhotoSpecList

decodePhotoSets : DC.Decoder (List (String, String))
decodePhotoSets =
  DC.at ["photosets", "photoset"] 
    (DC.list <| DC.map2 (,)
                  ((DC.at ["id"]) DC.string) 
                  ((DC.at ["title", "_content"]) DC.string)
    )

decodePhotoDescription : DC.Decoder String
decodePhotoDescription = 
  DC.at ["photo", "description", "_content"]  DC.string

apiKey : String
apiKey = "859b1fdf671b6419805ec3d2c7578d70"

flickrRestServices : String
flickrRestServices = "https://api.flickr.com/services/rest/?" 

noJsonCallback : String
noJsonCallback = "&format=json&nojsoncallback=1" 

userIdUrl : String -> String
userIdUrl name = 
     flickrRestServices
  ++ "&method=flickr.people.findByUserName"
  ++ "&api_key=" ++ apiKey 
  ++ "&username=" ++ name 
  ++ noJsonCallback

publicPhotosUrl : String -> String
publicPhotosUrl uid = 
     flickrRestServices
  ++ "&method=flickr.people.getPublicPhotos"
  ++ "&api_key=" ++ apiKey 
  ++ "&user_id=" ++ uid 
  ++ noJsonCallback

photoSetsUrl : String -> String
photoSetsUrl uid = 
     flickrRestServices
  ++ "&method=flickr.photosets.getList"
  ++ "&api_key=" ++ apiKey 
  ++ "&user_id=" ++ uid 
  ++ noJsonCallback

albumPhotosUrl : String -> (String, List (String, String)) -> Maybe String
albumPhotosUrl album (uid, setList) =  
  let setForAlbum = List.head <| List.filter (\(id, name) -> name == album) setList
  in case setForAlbum of
       Nothing -> Nothing
       Just (id,name) -> Just(   flickrRestServices
                              ++ "&method=flickr.photosets.getPhotos"
                              ++ "&api_key=" ++ apiKey 
                              ++ "&user_id=" ++ uid 
                              ++ "&photoset_id=" ++ id
                              ++ noJsonCallback )

photoInfoUrl : String -> String 
photoInfoUrl photoId = 
     flickrRestServices
  ++ "&method=flickr.photos.getInfo"
  ++ "&api_key=" ++ apiKey 
  ++ "&photo_id=" ++ photoId
  ++ noJsonCallback


initModel : Maybe Route -> (Model, Cmd Msg)
initModel r = 
  let cmd = case r of 
    Nothing -> 
      Cmd.none

    Just (NameOnly name) -> 
      let req = Http.get (userIdUrl name) decodeUserId 

          userIdTask = Http.toTask req

          publicPhotosTask uid = 
              Http.toTask (Http.get (publicPhotosUrl uid) decodePhotos)

          userPhotosTask = userIdTask |> (andThen publicPhotosTask )

      in Task.attempt SetPhotoIds userPhotosTask

    Just (NameAndAlbum name album) -> 
      let req = Http.get (userIdUrl name) decodeUserId 
          userIdTask = Http.toTask req

          setsTask uid = 
            Task.map (\s -> (uid,s)) <| Http.toTask (Http.get (photoSetsUrl uid) decodePhotoSets)

          albumPhotosTask sets = 
            let murl = albumPhotosUrl album sets
            in case murl of
                 Nothing -> Task.fail (Http.BadUrl <| "album not found: " ++ album)
                 Just url -> Http.toTask (Http.get url decodeAlbumPhotos)

          userPhotosTask = userIdTask |> (andThen setsTask ) |> (andThen albumPhotosTask)
      in Task.attempt SetPhotoIds userPhotosTask

  in (Model (Ok {left=[], right=[]}), cmd)

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

type Direction = Left | Right

type Msg
  =   UrlChange Navigation.Location
    | SetPhotoIds (Result Http.Error (List PhotoSpec))
    | ScrollPick Direction
    | SetCurrentDescription (Result Http.Error String)

updateDescriptionCmd : Scroll -> Cmd Msg
updateDescriptionCmd ns =
  case List.head (ns.right) of
    Nothing -> Cmd.none
    Just dp -> case (dp.description) of
                 Nothing -> Task.attempt SetCurrentDescription (Http.toTask <| Http.get (photoInfoUrl (dp.id)) decodePhotoDescription)
                 Just des -> Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      initModel (Url.parseHash route location) 

    SetPhotoIds (Ok photoIds) ->
      let scroll ={left=[], right=photoIds} 
      in (Model (Ok scroll), updateDescriptionCmd scroll)

    SetPhotoIds (Err e) ->
      (Model (Err e), Cmd.none)

    ScrollPick dir ->
      case model.photoIds of
        Ok s -> let ns = case dir of
                           Right -> 
                             {left=List.take 1 s.right ++ s.left, right=List.drop 1 s.right}
                           Left ->  
                             {left=List.drop 1 s.left, right=List.take 1 s.left ++ s.right}
                    cmd = updateDescriptionCmd ns 
                in (Model (Ok ns), cmd)

        Err e -> (Model (Err e), Cmd.none)

    SetCurrentDescription (Ok description) ->
      case model.photoIds of
        Err e -> (Model (Err e), Cmd.none)
        Ok scroll -> 
          case (List.head scroll.right) of
               Nothing -> (model, Cmd.none)
               Just ps -> 
                 let described = PhotoSpec ps.id ps.secret ps.server ps.farm (Just description)
                 in (Model (Ok {left = scroll.left, right = described :: List.drop 1 scroll.right}), Cmd.none)
          
    SetCurrentDescription (Err e) ->
      (Model (Err e), Cmd.none)

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

-- As described here: https://www.flickr.com/services/api/misc.urls.html
photoUrl : PhotoSpec -> String
photoUrl ps = 
     "https://farm" ++ toString ps.farm ++ ".staticflickr.com/" 
  ++ ps.server ++ "/" 
  ++ ps.id ++ "_" ++ ps.secret ++ "_b.jpg"

photoInDiv : (Bool, Bool) -> PhotoSpec -> Html Msg
photoInDiv vis ps = 
  div [HA.style [ ("height", "100%")
                , ("width", "100%")
                ]
      ]
      [ div [HA.style [ ("height", "90%")
                      , ("width", "100%")
                      , ("background", "url('" ++ photoUrl ps ++ "') center center no-repeat grey")
                      ]
            ]
            [svgArrows vis]

      , div [HA.style [ ("height", "10%")
                      , ("width", "100%")
                      ]
            ]
            [div [HA.style [ ("text-align", "center") ] ]
                 [text <| Maybe.withDefault "" ps.description]
            ]
      ] 

view : Model -> Html Msg
view model =
  div []
      [ case model.photoIds of
          Err s -> 
              text ("Error: " ++ (toString s))

          Ok scroll -> 
              let lv = List.length (scroll.left) > 0
                  rv = List.length (scroll.right) > 1
                  cur = List.take 1 scroll.right
              in div [HA.style [  ("overflow", "hidden"), ("height","100%"), ("width", "100%"), ("margin", "0")] ] 
                     (List.map (photoInDiv (lv,rv)) cur)
      ]
