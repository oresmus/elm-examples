-- elm-examples/svg-drag-fancy/svg-drag-fancy.elm
-- see also README.txt, NOTES.txt
-- for elm version 0.18

-- certain lines are commented as "not fully understood", since I don't yet fully understand their code.
-- warning: I use "object" as a variable name, though there is also Html.object (which it conflicted with, before I removed implicit imports).

import Html exposing (Html,div,Attribute)
import Html.Attributes exposing (style)

import Svg exposing (Svg,svg,circle,g)
import Svg.Events exposing (on)
import Svg.Attributes exposing (x,y,dy,fontSize,fontFamily,textAnchor,cx,cy,r,fill,stroke,strokeWidth,viewBox,width,height,pointerEvents)

import Json.Decode as Json
import Mouse exposing (Position)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias ObjId = Int

type ObjectType = OT_Classic String | OT_Square Int -- to be extended ###

type alias Object =
    { id : ObjId
    , position : Position
    , objecttype : ObjectType -- replaces colorstyle -- not yet used ###
    , dragging : Bool
    }

type alias Model =
    { objects : List Object
    , drag : Maybe Drag
    , nextid : ObjId
    }

type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init = ( Model [] Nothing 1
         |> addNewObject (Position  50 200)  (OT_Classic "#8D2F3C")
         |> addNewObject (Position 200 200)  (OT_Square 10) 
         |> addNewObject (Position 350 200)  (OT_Classic "#2F3C8D") 
         |> addNewObject (Position  -50 200)  (OT_Square 20) 
         |> addNewObject (Position -200 200)  (OT_Classic "#3C8D2F") 
         |> addNewObject (Position -350 200)  (OT_Square 10) 
         |> addNewObject (Position  50 -200)  (OT_Classic "#8D2F3C") 
         |> addNewObject (Position 200 -200)  (OT_Square 15) 
         |> addNewObject (Position 350 -200)  (OT_Classic "#2F3C8D") 
         |> addNewObject (Position  -50 -200)  (OT_Square 20) 
         |> addNewObject (Position -200 -200)  (OT_Classic "#3C8D2F") 
         |> addNewObject (Position -350 -200)  (OT_Square 25) 
       , Cmd.none )


-- add a new object to the model (which is not being dragged) 

-- version which also returns new object id
addNewObject_retID : Position -> ObjectType -> Model -> ( Model , ObjId )
addNewObject_retID pos objtype ({objects, drag, nextid} as model) =
    ( Model
        (objects ++ [ (Object nextid pos objtype False) ])
        drag
        (nextid + 1) ,
      nextid
    )

-- version which only returns model
addNewObject : Position -> ObjectType -> Model -> Model
addNewObject pos objtype model =
    let 
        (newmodel, newid) = addNewObject_retID pos objtype model -- [this worked once i made sure the varnames were not being reused ###]
    in
        newmodel


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
updateHelp msg ({objects, drag, nextid} as model) =
  case msg of
    DragStart id xy ->
      Model (startdrag id True objects) (Just (Drag xy xy)) nextid

    DragAt xy ->
      Model objects (Maybe.map (\{start} -> Drag start xy) drag) nextid -- not fully understood

    DragEnd _ ->
      Model (getNewObjects model) Nothing nextid

    DragStartWhole xy ->
      let
          pos = xy
          objtype = (OT_Classic "#3C8D2F")
          (newmodel, newid) = addNewObject_retID pos objtype model
      in
          updateHelp (DragStart newid xy) newmodel


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
-- ### does always subscribing fix the freezing bug? not the basic bug, but maybe the sometimes-drags-with-mouse-up aspect of it. ###
--  case model.drag of
--    Nothing ->
--      Sub.none
--
--    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view {objects,drag} = 
  -- modified from https://gist.github.com/TheSeamau5/8847c0e8781a3e284d82
  let
      view_ =
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
          g [] view_ 
        ]

margin : Int
margin = 8


-- (this could be enhanced to split a single arg at newlines, or to take a list of lines)
drawLegendText : String -> String -> Svg msg
drawLegendText line1 line2 =
  Svg.text_
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
    pos = getPosition object drag 
  in
    case object.objecttype of
        OT_Classic _ -> view_OT_Classic pos object
        OT_Square _ -> view_OT_Square pos object

view_OT_Classic : Position -> Object -> Svg Msg -- ### defect: we mean, but can't say, "Object whose objecttype is a OT_Classic". Should we factor??
view_OT_Classic pos object =
  let
    p = pos
        -- elm syntax note [from older version of this code]: I could never get this to pass compiler when assigning directly to (x1, y1)
    radius = 20
    radius_small = 5
  in
    g
      [
          -- onMouseDown object.id, -- note: this works on the text and the filled circle, even if fill is entirely transparent (alpha of 0).
          -- style [ "cursor" => "move" ]
       ]
      -- ### doesn't yet use object.objecttype
      [ circle
          [ onMouseDown object.id , style [ "cursor" => "move" ] -- putting onMouseDown here makes only the main circle work for dragging
          , cx          (toString p.x)
          , cy          (toString p.y)
          , r           (toString radius)
          , fill        "rgba(255,0,0,0)" -- note: these also work here: "rgba(255,0,0,0.1)", "#0B79CE", "red", [obsolete] object.colorstyle
          , stroke      "black" -- (note: stroke and strokeWidth can be left out; they outline the circle)
          , strokeWidth "2"
          ] []
      , Svg.text_
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

view_OT_Square : Position -> Object -> Svg Msg
view_OT_Square pos object =
  let
    p = pos
    radius = 20
    radius_small = 5
  in
    g
      [
       ]
      -- ### not yet an actual square; doesn't yet use object.objecttype
      [ circle
          [ onMouseDown object.id , style [ "cursor" => "move" ] -- putting onMouseDown here makes only the main circle work for dragging
          , cx          (toString p.x)
          , cy          (toString p.y)
          , r           (toString radius)
          , fill        "rgba(255,0,0,0)" -- note: these also work here: "rgba(255,0,0,0.1)", "#0B79CE", "red", [obsolete] object.colorstyle
          , stroke      "black" -- (note: stroke and strokeWidth can be left out; they outline the circle)
          , strokeWidth "2"
          ] []
      , Svg.text_
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

