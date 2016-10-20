-- elm-examples/svg-drag-1/svg-drag-1.elm

-- needed:
-- % elm-package install elm-lang/mouse
-- % elm-package install elm-lang/svg
-- % elm-make *.elm

-- to remake index.html, run % elm-make svg-drag-1.elm 

-- this is [### I mean, will be soon] modified from the prior example html-drag-2, in several ways: [planned ###]
--
-- - use svg instead of html
-- - drag by setting svg transform attribute
-- - use diff optims (keyed, lazy)
-- - more ###
--
-- - ultimately it should grow into some kind of mouse-controlled "drawing program".

-- certain lines are commented as "not fully understood", since I don't yet fully understand their code.
-- warning: I use "object" as a variable name, though there is also Html.object (which it conflicted with, before I removed implicit imports).

-- known bugs (also in original code):
-- - no provision is made for dragging out of range, scrolling, etc. I don't know whether it works properly in all such cases.

-- ### some imports might no longer be needed:

import Html exposing (Html, div, Attribute)
import Html.App as App
import Html.Attributes exposing (style)

import Svg exposing (Svg,svg,circle,g) -- bugfix: import svg from here, not from Html! I wonder why it matters. ###
import Svg.Events exposing (on)
import Svg.Attributes exposing (x,y,fontSize,cx,cy,r,fill,stroke,strokeWidth,viewBox,width)

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
                  Object 1 (Position  50 200)  "#8D2F3C" False
                , Object 2 (Position 200 200)  "#3C8D2F" False
                , Object 3 (Position 350 200)  "#2F3C8D" False
                , Object 4 (Position  -50 200)  "#8D2F3C" False
                , Object 5 (Position -200 200)  "#3C8D2F" False
                , Object 6 (Position -350 200)  "#2F3C8D" False
                , Object 7 (Position  50 -200)  "#8D2F3C" False
                , Object 8 (Position 200 -200)  "#3C8D2F" False
                , Object 9 (Position 350 -200)  "#2F3C8D" False
                , Object 10 (Position  -50 -200)  "#8D2F3C" False
                , Object 11 (Position -200 -200)  "#3C8D2F" False
                , Object 12 (Position -350 -200)  "#2F3C8D" False
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
  -- modified from https://gist.github.com/TheSeamau5/8847c0e8781a3e284d82
  let
      view' =
        -- drawText "The objects are draggable" :: -- ### zapped to see if that fixed mystery bug -- didn't
          (List.map (viewObject drag) objects)
  in
      svg
        [ 
--          style
--          [ ("border"     , "1px solid black")
--          , ("width"      , "800px")
--          , ("height"     , "600px")
--          , ("display"    , "block")
--          , ("margin"     , (toString margin) ++ "px")
--          , ("font-family", "Times, serif")
--          ],
          viewBox "0 0 600 600" -- ### will adding this help? nope... not even after commenting out style above, adding width below
          , width "600px"
        ]
        [
          circle [ cx "50", cy "50", r "45",  fill "#0B79CE" ] [] -- even this is not visible, taken directly from clock example. hmm...??? 
-- ,          g [] view' 
        ]

-- from https://gist.github.com/TheSeamau5/8847c0e8781a3e284d82
-- but it had some compile errors, and looked a bit like nonsense, so i modified it, guessing...
drawText : String -> Svg msg
drawText string =
  div
    [ x         "20"
    , y         "20"
    , fontSize  "20"
    , style
        [ ("-webkit-user-select", "none") ]
    ]
    [ Svg.text string ]

-- from https://gist.github.com/TheSeamau5/8847c0e8781a3e284d82; bks note: might mess up mouse event posns, see correctMouseEvent in there
margin : Int
margin = 8


viewObject : Maybe Drag -> Object -> Svg Msg -- ### note, compiles just as well with output type Svg Msg or Html Msg 
viewObject drag object =
  let
    p = getPosition object drag -- ### I could never get this to pass compiler when assigning directly to (x1, y1)
    radius = 20
  in
    g
      [
          onMouseDown object.id,
          style [ "cursor" => "move" ]
       ]
      [ circle
          [ cx          (toString p.x)
          , cy          (toString p.y)
          , r           (toString radius)
          , fill        "#0B79CE" -- ### replaces "rgba(255,0,0,0.1)", will that help?? nope.
   --       , stroke      "black" -- will removing these two lines help? ### nope
   --       , strokeWidth "2"
          ]
          []
      ]

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
