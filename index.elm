import Html.App as App
import Html exposing (Html, div, text)
import Html.Attributes as H
import Html.Events exposing (onClick)
import Svg exposing (svg, g, rect, text')
import Svg.Attributes as S
import Array exposing (..)

main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type Cell = Blank
type alias Model =
  { board : Array (Array (Int, Int, Cell))
  , player : List Cell
  }

size = 9
bucketSize = 7
row i = initialize size (\j -> (i, j, Blank))
board = initialize size (\i -> row i)
player = List.repeat bucketSize Blank
model : Model
model = {board = board, player = player }


-- UPDATE

type Msg = Update

update : Msg -> Model -> Model
update _ model = model


-- VIEW

dimension = 60
space = dimension + 3
boardwidth = size * space
ls = toString boardwidth
ds = toString dimension
curve = toString 10

view : Model -> Html Msg
view model =
  div [ H.style [("width", toString (boardwidth + 2*space) ++ "px")] ]
      [ boardSvgArray model.board
      , div [H.style [("text-align", "center")]] [text "Player"]
      , playerRow model.player
      ]

boardSvgArray brd =
  Svg.svg
    [ S.width ls, S.height ls, S.viewBox ("0 0 " ++ ls ++ " " ++ ls)
      , H.style [("display", "block"), ("margin", "1em auto")] ]
    (List.concat (toList (map f brd)))

f row = toList (map gg row)
gg (x,y,stt) =
  let
    clr = case stt of
      Blank -> "beige"
  in getRect (space * x + 2) (space * y + 2)

playerRow player =
  let
    ws = toString (List.length player * space + 3)
    hs = toString (dimension + 3)
  in
    Svg.svg
      [S.width ws, S.height hs, S.viewBox ("0 0 " ++ ws ++ " " ++ hs)
      , H.style [("display", "block"), ("margin", "1em auto")]]
      (List.map h (List.map2 (,) [0..bucketSize-1] player))
h (i, cell) = getRect (i*space + 2) 2
getRect x y = g [S.transform ("translate" ++ toString (x,y))]
                [svg [S.width ds, S.height ds]
                  [ rect [ S.fill "beige", S.stroke "black"
                         , S.width ds, S.height ds
                         , S.rx curve, S.ry curve ] []
                  , text' [ S.x "50%", S.y "60%"
                          , S.textAnchor "middle", S.alignmentBaseline "middle"
                          , S.fontSize "48px"] [text "Y"]
                  ]
                ]
