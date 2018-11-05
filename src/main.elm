import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Url.Builder as Url
import Json.Decode as Decode exposing (Decoder, int, string, float)
import Json.Decode.Pipeline exposing (required, optional, hardcoded, requiredAt)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type LoadingStatus
  = Loaded (List Image)
  | NothingLoaded
  | Loading
  | ErrorLoading Http.Error


type alias Image =
  { user : String
  , url : String
  , link : String
  , dlink : String
  }


type alias Model =
  { loadStatus : LoadingStatus
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { loadStatus = NothingLoaded }
  , Cmd.none
  )



-- UPDATE

type Msg
  = RequestRandomImages
  | RecieveImages ( Result Http.Error (List Image) )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestRandomImages ->
      ( { loadStatus = Loading }
      , getRandomImages
      )

    RecieveImages result ->
      case result of
        Ok images ->
          ( { loadStatus = Loaded images }
          , Cmd.none
          )

        Err error ->
          ( { loadStatus = ErrorLoading error }
          , Cmd.none
          )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

imageToHtml : Image -> Html msg
imageToHtml image =
  div [ class "image" ]
    [ img [ src image.url, class "imageurl" ] []
    , br [] []
    , p [] [ a [ href image.link, class "link" ] [ text image.user ] ]
    , br [] []
    , p [] [ a [ href image.dlink, class "link" ] [ text "Download" ] ]
    ]



view : Model -> Html Msg
view model =
  div []
    [ h1 [ class "header" ] [ text "Random images from Unsplash!" ]
    , button [ onClick RequestRandomImages, class "getimages" ] [ text "Request Images!" ]
    , br [] []
    , br [] []
    , case model.loadStatus of
        Loaded images ->
          div [ class "flex-container" ]
          [ div [ class "images" ] ( List.map imageToHtml images ) ]

        NothingLoaded ->
          div [] []

        Loading ->
          div [ class "loader" ]
          [ img [ src "https://community.thunkable.com/uploads/default/original/1X/a8c639f5bd67f6eecf3b7f2209c7e7fada621f9c.png" ] []
          , h1 [] [ text "Loading!" ] ]

        ErrorLoading error ->
          div [ class "errorbox" ]
          [ img [ src "http://cdn.onlinewebfonts.com/svg/img_229205.png", height 100, width 100 ] [],
            h1 []
            [ ( text ( case error of
              Http.BadUrl _ ->
                "Bad URL"
              Http.Timeout ->
                "Timeout"
              Http.NetworkError  ->
                "Network Error"
              Http.BadStatus _ ->
                "UnexpectedPayload"
              Http.BadPayload _ _ ->
                "BadResponse" ) ) ] ]
    ]

-- HTTP


getRandomImages : Cmd Msg
getRandomImages =
  Http.send RecieveImages (Http.get randomImagesUrl imagesDecoder)

unsplashClientID : String
unsplashClientID = "eae3c9c9ca41c31995d4fc91dfb687567ca076548cbfdf9cb421259339e23fad"

randomImagesUrl : String
randomImagesUrl =
  Url.crossOrigin "https://api.unsplash.com" ["photos","random"]
    [ Url.string "client_id" unsplashClientID
    , Url.int "count" 25
    ]


imagesDecoder : Decode.Decoder ( List Image )
imagesDecoder =
  Decode.list imageDecoder


imageDecoder : Decode.Decoder Image
imageDecoder =
  Decode.succeed Image
    |> requiredAt [ "user", "name" ] Decode.string
    |> requiredAt [ "urls", "small" ] Decode.string
    |> requiredAt [ "user", "links", "html" ] Decode.string
    |> requiredAt [ "links", "download" ] Decode.string
