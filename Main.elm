import Html exposing (Html, div, text)
import Html.Attributes as HA 
import Html.Events as HE 
import Http
import Maybe
import Svg exposing (Svg, svg, polygon, image)
import Svg.Attributes as SA 
import List exposing (take)
import Json.Decode as DC exposing (Decoder)
import Task exposing (andThen)
import Navigation
import UrlParser as Url exposing ((</>))

type Direction = Left | Right

type Msg
  =   UrlChange Navigation.Location
    | SetPhotos (Result Http.Error (List Photo))
    | ScrollPick Direction
    | SetDescription (Result Http.Error String)

main =
  Navigation.program UrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = \m -> Sub.none
    }

-- MODEL

type alias Model = 
  Result Http.Error { left : List Photo , right : List Photo}

decodeUser : DC.Decoder String
decodeUser =
  DC.at ["user", "id"] DC.string

type alias Photo = { id: String
                   , secret: String
                   , server: String
                   , farm: Int
                   , description: Maybe String
                   }

-- Create a Photo record from info retrieved from flickr api.
-- Defer extra api call needed for Description until photo displayed.
initPhoto : String -> String -> String -> Int -> Photo
initPhoto id sec ser farm = Photo id sec ser farm Nothing

decodePhotoList : DC.Decoder (List Photo)
decodePhotoList =
    (DC.list <| DC.map4 initPhoto
                          ((DC.at ["id"]) DC.string) 
                          ((DC.at ["secret"]) DC.string)
                          ((DC.at ["server"]) DC.string)
                          ((DC.at ["farm"]) DC.int)     
    )

-- Decode photos from "flickr.people.getPublicPhotos" request.
decodePhotos : DC.Decoder (List Photo)
decodePhotos =
  DC.at ["photos", "photo"] decodePhotoList

-- Decode photos from "flickr.photosets.getPhotos" request.
decodeAlbumPhotos : DC.Decoder (List Photo)
decodeAlbumPhotos =
  DC.at ["photoset", "photo"] decodePhotoList

-- Decode names of photosets from "flickr.photosets.getList" request.
decodePhotoSets : DC.Decoder (List (String, String))
decodePhotoSets =
  DC.at ["photosets", "photoset"] 
    (DC.list <| DC.map2 (,)
                  ((DC.at ["id"]) DC.string) 
                  ((DC.at ["title", "_content"]) DC.string)
    )

-- Decode descripion of photo from "flickr.photos.getInfo" request.
decodePhotoDescription : DC.Decoder String
decodePhotoDescription = 
  DC.at ["photo", "description", "_content"]  DC.string

-- api key from flickr.  Anyone who clones this project should
-- get their own api key.
apiKey : String
apiKey = "859b1fdf671b6419805ec3d2c7578d70"

flickrRestServices : String
flickrRestServices = "https://api.flickr.com/services/rest/?" 

noJsonCallback : String
noJsonCallback = "&format=json&nojsoncallback=1" 

userUrl : String -> String
userUrl name = 
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
photoInfoUrl photo = 
     flickrRestServices
  ++ "&method=flickr.photos.getInfo"
  ++ "&api_key=" ++ apiKey 
  ++ "&photo_id=" ++ photo
  ++ noJsonCallback

-- Cmd to get visible photo's description from flickr. 
-- Package results as SetDescription message.
setDescriptionCmd : List Photo -> Cmd Msg
setDescriptionCmd right =
  case List.head right of
    Nothing -> Cmd.none
    Just dp -> case (dp.description) of
                 Nothing -> Task.attempt SetDescription (Http.toTask <| Http.get (photoInfoUrl (dp.id)) decodePhotoDescription)
                 Just des -> Cmd.none

-- Cmd to get users public photos from flickr. 
-- Package results as SetPhotos message.
getPhotosCmd : String -> Cmd Msg
getPhotosCmd name =
  let req = Http.get (userUrl name) decodeUser 

      userTask = Http.toTask req

      publicPhotosTask uid = 
          Http.toTask (Http.get (publicPhotosUrl uid) decodePhotos)

      userPhotosTask = userTask |> (andThen publicPhotosTask )

  in Task.attempt SetPhotos userPhotosTask

-- Cmd to get public photos in named user's album from flickr. 
-- Package results as SetPhotos message.
getAlbumPhotosCmd : String -> String -> Cmd Msg
getAlbumPhotosCmd name album =
  let req = Http.get (userUrl name) decodeUser 
      userTask = Http.toTask req

      setsTask uid = 
        Task.map (\s -> (uid,s)) <| Http.toTask (Http.get (photoSetsUrl uid) decodePhotoSets)

      albumPhotosTask sets = 
        let murl = albumPhotosUrl album sets
        in case murl of
             Nothing -> Task.fail (Http.BadUrl <| "album not found: " ++ album)
             Just url -> Http.toTask (Http.get url decodeAlbumPhotos)

      userPhotosTask = userTask |> (andThen setsTask ) |> (andThen albumPhotosTask)
  in Task.attempt SetPhotos userPhotosTask

-- Initialize model based on URL 'routing' arguments.
initModel : Maybe Route -> (Model, Cmd Msg)
initModel r = 
  let cmd = case r of 
    Nothing -> 
      Cmd.none

    Just (NameOnly name) -> 
      getPhotosCmd name

    Just (NameAndAlbum name album) -> 
      getAlbumPhotosCmd name album

  in (Ok {left=[], right=[]}, cmd)

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    initModel (Url.parseHash route location)

-- URL PARSING

type Route
  = NameOnly String
  | NameAndAlbum String String

-- Parse URL 'arguments' either to just a user or a user and album.
route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map NameOnly Url.string
    , Url.map NameAndAlbum (Url.string </> Url.string)
    ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      initModel (Url.parseHash route location) 

    SetPhotos (Ok photos) ->
      (Ok {left=[], right=photos}, setDescriptionCmd photos)

    SetPhotos (Err e) ->
      (Err e, Cmd.none)

    -- A left or right arrow was picked.  Move a photo left or right.
    -- Issue command to update the description for the displayed photo.
    ScrollPick dir ->
      case model of
        Err e -> (Err e, Cmd.none)
        Ok s -> let ns = case dir of
                           Right -> 
                             { left=List.take 1 s.right ++ s.left
                             , right=List.drop 1 s.right}

                           Left ->  
                             { left=List.drop 1 s.left
                             , right=List.take 1 s.left ++ s.right}

                    cmd = setDescriptionCmd ns.right
                in (Ok ns, cmd)

    -- Update description of the currently viewed photo.
    SetDescription (Ok desc) ->
      case model of
        Err e -> (Err e, Cmd.none)
        Ok scroll -> 
          case (List.head scroll.right) of
               Nothing -> (model, Cmd.none)
               Just viewed -> 
                 let described = {viewed | description=Just desc}
                 in (Ok { scroll | right = described :: List.drop 1 scroll.right}, Cmd.none)
          
    SetDescription (Err e) ->
      (Err e, Cmd.none)

-- VIEW

-- Draw a svg "arrow" (a triangle) pointing left or right.
arrow : Direction -> Svg Msg
arrow dir = polygon [ SA.points (if (dir == Left) 
                                 then "-85,-10 -85,10 -95,0" 
                                 else "85,-10 85,10 95,0")
                    , SA.style "fill: red"
                    , HE.onClick (ScrollPick dir)
                    ] 
                    []

-- Draw an image with arrows for scrolling left or righ
imageWithArrows : Bool -> Bool -> String -> Html Msg
imageWithArrows vl vr im = 
    let al = if (vl) then [arrow Left] else []
        ar = if (vr) then [arrow Right] else []
    in svg [ SA.version "1.1"
           , SA.width "100%" 
           , SA.height "100%" 
           , SA.viewBox "-100 -60 200 120" 
           , SA.preserveAspectRatio "none"
           ]
           ( [image [ SA.xlinkHref im
                    , SA.x "-100"
                    , SA.y "-60"
                    , SA.width "200"
                    , SA.height "120"
                    ] 
                    []
             ] ++ al ++ ar
           )

-- Compute a photo URL from a Photo record.
-- per: https://www.flickr.com/services/api/misc.urls.html
photoUrl : Photo -> String
photoUrl ps = 
     "https://farm" ++ toString ps.farm ++ ".staticflickr.com/" 
  ++ ps.server ++ "/" 
  ++ ps.id ++ "_" ++ ps.secret ++ "_b.jpg"

-- show an image and description if available.
photoInDiv : Bool -> Bool -> Photo -> Html Msg
photoInDiv lv rv ps = 
  div [HA.style [ ("height", "100%")
                , ("width", "100%")
                , ("margin", "0")
                ]
      ]
      [ div [HA.style [ ("height", "90%")
                      , ("width", "100%")
                      , ("margin", "0")
                      ]
            ]
            [imageWithArrows lv rv (photoUrl ps)]

      , div [HA.style [ ("height", "10%")
                      , ("width", "100%")
                      , ("margin", "0")
                      ]
            ]
            [div [HA.style [ ("text-align", "center") ] ]
                 [text <| Maybe.withDefault "" ps.description]
            ]
      ] 

-- Draw an image or display the reason the image is not available.
view : Model -> Html Msg
view model =
  div []
      [ case model of
          Err s -> 
              text ("Error: " ++ (toString s))

          Ok scroll -> 
              let lv = List.length (scroll.left) > 0
                  rv = List.length (scroll.right) > 1
                  cur = List.take 1 scroll.right
              in div [HA.style [  ("height","100%"), ("width", "100%"), ("margin", "0")] ] 
                     (List.map (photoInDiv lv rv) cur)
      ]
