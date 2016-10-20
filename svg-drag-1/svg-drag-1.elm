-- elm-examples/svg-drag-1/svg-drag-1.elm

-- needed to compile, when in a fresh directory:
-- % elm-package install elm-lang/mouse
-- % elm-package install elm-lang/svg
-- % elm-make *.elm

-- to remake index.html, just run % elm-make *.elm [assuming *.elm matches only this file]

-- this is modified from the prior example html-drag-2, in several ways:
--
-- + use svg instead of html
--
-- possible future mods: ###
-- - drag by setting svg transform attribute
-- - use optims of virtual-dom diff speed (keyed, lazy)
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

import Svg exposing (Svg,svg,circle,g)
import Svg.Events exposing (on)
import Svg.Attributes exposing (x,y,fontSize,fontFamily,textAnchor,cx,cy,r,fill,stroke,strokeWidth,viewBox,width,height)

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
          drawLegendText "The objects are draggable." ::
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
          -- interactions of viewBox and (width, height) (created by the code below; by experiment, in Safari):
          --
          -- with the style above commented out, and using viewBox and (width, height) of "0 0 600 600" and (800px,600px), 
          -- the box of svg coords is centered in the width/height box, and fits it exactly in Y, 
          -- but clipping is only done by the outer box, 
          -- so the svg coords that are visible are y = 0 (top) to 600 (bot), x = -100 (left) to 700 (right)
          -- (as proven by printing them in the circles, in viewObject).
          --
          -- using same width/height box but smaller or bigger viewBox is untested. ###
          --
          -- effect on mouse event coords is untested (except scale must be 1:1 and/or a transform gets done, or dragging would not be correct).

          viewBox "0 0 600 600" -- when this is smaller than following width/height, this box is centered in the following one (by experiment)
          , width "800px"
          , height "600px" -- ### plan: eventually get border style to work ###
        ]
        [
          g [] view' 
        ]

-- from https://gist.github.com/TheSeamau5/8847c0e8781a3e284d82
-- but renamed from drawText, and modified to fix compile errors; works now; text is not selectable by browser.
drawLegendText : String -> Svg msg
drawLegendText string =
  Svg.text'
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


viewObject : Maybe Drag -> Object -> Svg Msg -- note, compiles just as well with output type Svg Msg or Html Msg 
viewObject drag object =
  let
    p = getPosition object drag -- elm syntax note: I could never get this to pass compiler when assigning directly to (x1, y1)
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
          , fill        object.colorstyle -- note: these also work here: "rgba(255,0,0,0.1)", "#0B79CE", "red"
          , stroke      "black" -- (note: stroke and strokeWidth can be left out; they outline the circle)
          , strokeWidth "2"
          ]
          []
      , Svg.text' 
          [ x (toString p.x), 
            y (toString p.y), 
            fontFamily "Verdana", 
            fontSize "12",
            textAnchor "middle" -- this centers the text horizontally. I don't know how to center it vertically. ###
          , style
              [ ("-webkit-user-select", "none") ] -- make text unselectable by browser (seems to work, though hard to test with certainty)

          ] 
          [Svg.text ("obj " ++ (toString object.id) ++ " " ++ (toString p) )]
        -- for doc of svg attrs, see https://developer.mozilla.org/en-US/docs/Web/SVG/Element/text
        -- and http://package.elm-lang.org/packages/elm-lang/svg/1.1.1/Svg-Attributes
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
