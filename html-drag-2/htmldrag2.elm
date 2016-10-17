-- elm-examples/html-drag-2/htmldrag2.elm

-- this is modified from the elm guide example for mouse drags (which drags one object):
--
-- it lets the user drag several objects independently.
-- (as in the original, the objects are shown as html divs.)
-- certain lines are commented as "not fully understood", since I don't yet fully understand their code.

-- known bugs (also in original code):
-- - the text shown in the dragged objects (html divs) sometimes gets selected (by the browser) while dragging them.
-- - no provision is made for dragging out of range, scrolling, etc. I don't know whether it works properly in all such cases.

import Html exposing (Html, div, text, Attribute)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias ObjId = Int

type alias Object =
    { id : ObjId
    , position : Position
    , colorstyle : String
    , dragging : Bool
    }

type alias Model =
    { objects : List Object
    , drag : Maybe Drag
    }

type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init = ( Model [
                  Object 1 (Position   50 200)  "#8D2F3C" False
                , Object 2 (Position 200 200)  "#3C8D2F" False
                , Object 3 (Position 350 200)  "#2F3C8D" False
           ] Nothing, Cmd.none )


-- UPDATE


type Msg
    = DragStart ObjId Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({objects, drag} as model) =
  case msg of
    DragStart id xy ->
      Model (startdrag id True objects) (Just (Drag xy xy))

    DragAt xy ->
      Model objects (Maybe.map (\{start} -> Drag start xy) drag) -- not fully understood

    DragEnd _ ->
      Model (getNewObjects model) Nothing


startdrag : ObjId -> Bool -> List Object -> List Object
startdrag id on objects =
    List.map (startdragObject id on) objects

startdragObject : ObjId -> Bool ->  Object ->  Object
startdragObject id on object =
     if object.id == id then { object | dragging = on } else { object | dragging = False } -- all False unless indexed specifically
 
getNewObjects : Model -> List Object
getNewObjects {objects, drag} = 
      List.map (getNewObject drag) objects -- ### some dup code with the related view code using drag
 
getNewObject : Maybe Drag -> Object -> Object
getNewObject drag object = 
    { object | position = getPosition object drag }
 

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view {objects,drag} = 
  div [] (List.map (viewObject drag) objects)

viewObject : Maybe Drag -> Object -> Html Msg
viewObject drag object =
  let
    realPosition =
      getPosition object drag
  in
    div
      [ onMouseDown object.id
      , style
          [ "background-color" => object.colorstyle
          , "cursor" => "move"

          , "width" => "100px"
          , "height" => "100px"
          , "border-radius" => "4px"
          , "position" => "absolute"
          , "left" => px realPosition.x
          , "top" => px realPosition.y

          , "color" => "white"
          , "display" => "flex"
          , "align-items" => "center"
          , "justify-content" => "center"
          ]
      ]
      [ text ("Drag Me! (" ++ toString object.id ++ ")")
      ]


px : Int -> String
px number =
  toString number ++ "px"


getPosition : Object -> Maybe Drag -> Position
getPosition object drag =
  let position = object.position in
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      if object.dragging then
       Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)
      else
       position


onMouseDown : ObjId -> Attribute Msg
onMouseDown id =
  on "mousedown" (Json.map (DragStart id) Mouse.position) -- not fully understood ###
