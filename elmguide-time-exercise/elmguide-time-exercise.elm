import Html exposing (Html,div,input,text)
import Html.App as App
import Svg exposing (svg,circle,line)
import Svg.Attributes exposing (viewBox,width,cx,cy,r,fill,x1,y1,x2,y2,stroke)
import Time exposing (Time, second)

import Html.Attributes exposing (style, type')
import Html.Events exposing (onClick)



main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = {time : Time, on:Bool}


init : (Model, Cmd Msg)
init =
  (Model 0 True, Cmd.none)



-- UPDATE


type Msg
  = Tick Time | OnOff 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({model | time = newTime}, Cmd.none)
    OnOff -> 
      ({model | on = not model.on}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = if model.on then
  Time.every second Tick
  else Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model.time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    div [] [
     text "hi", text "lo",input [Html.Attributes.type' "checkbox", onClick OnOff ] [],
     svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45",  fill (if model.on then "#0B79CE" else "#7979CE") ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      ]
     ]

