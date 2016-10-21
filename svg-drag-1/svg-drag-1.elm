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
-- - add mouse hover properties
-- - drag by setting svg transform attribute
-- - use optims of virtual-dom diff speed (keyed, lazy)
-- - more ###
--
-- - ultimately it should grow into some kind of mouse-controlled "drawing program".

-- ### KNOWN BUG (mystery):
-- - in Safari 10.0 on Mac OS 10.10.5, running via github pages, 
--   some but perhaps not all parts of the elm-coded js code can "go to sleep" (for 5-30 seconds)
--   if there is no user activity for a minute or two (or longer). 
--   The main symptom is that the display "freezes", except for mouse pointer and cursor actions -- the user can try to drag an object,
--   and the mouse pointer and cursor work correctly, but the object (an svg circle with "decorations") doesn't move. 
--   But after waiting 5-30 seconds (depending on history), the object then suddenly moves to the right place.
--     The cause must not be quite as simple as the mouse events being buffered up until the elm code runs again --
--   sometimes the object is then still being dragged (as if it didn't notice the mouseup event).
--   On the other hand, multiple drag-attempts during the freeze period do all get buffered up somehow, since upon waking,
--   the object jumps much farther than any single drag attempt should have moved it.
--   This is not perfectly explainable either by elm code being frozen, or elm code running but virtual dom output being frozen.
--     Another mystery-behavior-detail is that, after an object is erroneously dragged during mouseup, it can jump back to an earlier
--   position upon the next mousedown (at least if it starts a new object-drag, not sure about otherwise).
--
--     This should be tried in other browsers, but I didn't do that yet.
--
--     In theory it could behave differently when run from a local file, 
--   but a brief trial shows it can also happen then, and might behave the same.
--
--     At least once, the bug did NOT happen after long inactivity if I tried it right after changing from some other tab in Safari.
--   
--     As for a minimal example demoing the bug, I didn't look for one, but I find that the bug does NOT appear
--   in the elm-lang mouse drag example (a single html div) (also run from my github pages site).
--   Same for the variant of that with 3 draggable objects (all html divs).
--   The main difference between those and this one are: svg vs html, 12 vs 3 objects, and some details of drag code.


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
import Svg.Attributes exposing (x,y,dy,fontSize,fontFamily,textAnchor,cx,cy,r,fill,stroke,strokeWidth,viewBox,width,height,pointerEvents)

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
    | DragStartWhole Position


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

    DragStartWhole xy ->
      Model 
          (objects ++ [ (Object 13 xy "#3C8D2F" True) ]) -- ### bug: all new objects have same id; this means they'll drag in sync ###
          (Just (Drag xy xy))

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
          drawLegendText "The objects are draggable" "(by their big circles only)." ::
          (List.map (viewObject drag) objects)
  in
      svg
        [ 
          -- onMouseDownWhole, ### disabled, to avoid interference from bug in which mousedown on main circle also fires this event
              -- ### BUGS in onMouseDownWhole:
              -- 2 expected bugs (offset, synced drag -- both should be easy to fix); 
              -- 2 unexpected bugs (also happens after circle mousedown, and only happens then -- nothing happens from click on plain bg).
          style
          [ ("border"     , "1px solid black")
--          , ("width"      , "800px")
--          , ("height"     , "600px")
          , ("display"    , "block")
          , ("margin"     , (toString margin) ++ "px")
--          , ("font-family", "Times, serif")
          ],
          -- interactions of viewBox and (width, height) (created by the code below; by experiment, in Safari):
          --
          -- with the style above commented out, and using viewBox and (width, height) of "0 0 600 600" and (800px,600px), 
          -- the box of svg coords is centered in the width/height box, and fits it exactly in Y, 
          -- but clipping is only done by the outer box, 
          -- so the svg coords that are visible are y = 0 (top) to 600 (bot), x = -100 (left) to 700 (right)
          -- (as proven by printing them in the circles, in viewObject).
          --
          -- using same width/height box but smaller viewBox "0 0 300 300", we find that the viewBox scales up (2x) to fit,
          -- so all objects are 2x bigger (including text and coordsys), 
          -- and dragging is not compensated for this scale (as expected, since there is no obvious code to do a transform),
          -- i.e. object motions are 2x longer than mouse motions (though other than that, dragging still works correctly, as also expected).
          --
          -- using viewBox "-300 -300 300 300" we discover the latter params are w,h, not x2,y2, so all params are (xleft,ytop,w,h).
          -- using viewBox "-300 -300 600 600" we confirm this, and get back to 1:1 scale (thus correct dragging w/o transform).
          --
          -- now we'll reenable some of the style above, and see if things still work. Yes, they do, with border, display, margin.

          viewBox "-300 -300 600 600" -- this (xleft,ytop,w,h) box is scaled to fit one axis, then centered in the following one (by experiment)
          , width "800px"
          , height "600px"
        ]
        [
          g [] view' 
        ]

margin : Int
margin = 8


-- (this could be enhanced to split a single arg at newlines, or to take a list of lines)
drawLegendText : String -> String -> Svg msg
drawLegendText line1 line2 =
  Svg.text'
    [ pointerEvents "none" -- prevents typing cursor (and mousedown-capture, though this is behind all other objects so that doesn't matter)
    , x         "20"
    , y         "20"
    , fontSize  "20"
    , style
        [ ("-webkit-user-select", "none") ]
    ]
    [ Svg.tspan [x "0", dy "1.2em"] [Svg.text line1] 
    , Svg.tspan [x "0", dy "1.2em"] [Svg.text line2]
    ]


viewObject : Maybe Drag -> Object -> Svg Msg -- note, compiles just as well with output type Svg Msg or Html Msg 
viewObject drag object =
  let
    p = getPosition object drag -- elm syntax note: I could never get this to pass compiler when assigning directly to (x1, y1)
    radius = 20
    radius_small = 5
  in
    g
      [
          -- onMouseDown object.id, -- note: this works on the text and the filled circle, even if fill is entirely transparent (alpha of 0).
          -- style [ "cursor" => "move" ]
       ]
      [ circle
          [ onMouseDown object.id , style [ "cursor" => "move" ] -- putting onMouseDown here makes only the main circle work for dragging
          , cx          (toString p.x)
          , cy          (toString p.y)
          , r           (toString radius)
          , fill        "rgba(255,0,0,0)" -- note: these also work here: "rgba(255,0,0,0.1)", "#0B79CE", "red", object.colorstyle
          , stroke      "black" -- (note: stroke and strokeWidth can be left out; they outline the circle)
          , strokeWidth "2"
          ] []
      , Svg.text'
          [ pointerEvents "none" -- prevents blocking mousedown or changing to typing cursor
          , x (toString p.x), 
            y (toString p.y), 
            fontFamily "Verdana", 
            fontSize "12",
            textAnchor "middle" -- this centers the text horizontally. I don't know how to center it vertically (maybe use tspan dy??). ###
          , style
              [ ("-webkit-user-select", "none") ] -- make text unselectable by browser (seems to work, though hard to test with certainty)

          ] 
          [Svg.text ("obj " ++ (toString object.id) ++ " " ++ (toString p) )]
        -- for doc of svg attrs, see https://developer.mozilla.org/en-US/docs/Web/SVG/Element/text
        -- and http://package.elm-lang.org/packages/elm-lang/svg/1.1.1/Svg-Attributes
      , circle -- smaller, above the main circle.
          [ pointerEvents "none" -- prevents this object blocking mousedown for objects visually behind it
          , cx     (toString p.x)
          , cy     (toString (p.y - radius - radius_small - 3))
          , r      (toString radius_small)
          , fill   "rgba(0,0,0,0.25)" -- transparent gray
          , stroke "black"
          , strokeWidth "1"
          ] []
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
  on "mousedown" (Json.map (DragStart id) Mouse.position) -- not fully understood, re Json.map ###

onMouseDownWhole : Attribute Msg
onMouseDownWhole =
  on "mousedown" (Json.map DragStartWhole Mouse.position) -- not fully understood, re Json.map ###

