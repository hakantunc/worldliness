import Html.App as App
import Html exposing (Html, div, text)
import Html.Attributes as H
import Html.Events exposing (onClick)
import Svg exposing (svg, g, rect, text')
import Svg.Attributes as S
import Array exposing (..)
import String exposing (fromList)
import Char
import Random
import Task

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Cell = Blank | S Char
type alias Model =
  { board : Array (Array (Int, Int, Cell))
  , player : List Cell
  }

size = 9
bucketSize = 7
row i = initialize size (\j -> (i, j, Blank))
board = initialize size (\i -> row i)
player = List.repeat bucketSize (S ' ')
model : Model
model = {board = board, player = player }

init : (Model, Cmd Msg)
init = (model, Task.perform (always Init) (always Init) (Task.succeed 0))


-- UPDATE

type Msg = Init | Load (List Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Init ->
      ( model
      , Random.generate
          Load
          (Random.list bucketSize (Random.int (Char.toCode 'A') (Char.toCode 'Z')))
      )
    Load vals -> (updatePlayerBucket model vals, Cmd.none)

updatePlayerBucket : Model -> List Int -> Model
updatePlayerBucket model vals = {
    board = model.board,
    player = List.map (\v -> S (Char.fromCode v)) vals
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


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
    [ S.class "board", S.width ls, S.height ls, S.viewBox ("0 0 " ++ ls ++ " " ++ ls) ]
    (List.concat (toList (map f brd)))

f row = toList (map gg row)
gg (x,y,stt) =
  let
    cc = case stt of
      Blank -> ' '
      (S c)  -> c
  in getRect (space * x + 2) (space * y + 2) cc

playerRow player =
  let
    ws = toString (List.length player * space + 3)
    hs = toString (dimension + 3)
  in
    Svg.svg
      [S.class "player", S.width ws, S.height hs, S.viewBox ("0 0 " ++ ws ++ " " ++ hs) ]
      (List.map h (List.map2 (,) [0..bucketSize-1] player))
h (i, cell) =
  case cell of
    Blank -> getRect (i*space + 2) 2 ' '
    (S c) -> getRect (i*space + 2) 2 c
getRect x y c = g [S.transform ("translate" ++ toString (x,y))]
                [svg [S.width ds, S.height ds]
                  [ rect [ S.fill "beige", S.stroke "black"
                         , S.width ds, S.height ds
                         , S.rx curve, S.ry curve ] []
                  , text' [ S.x "50%", S.y "60%"
                          , S.textAnchor "middle", S.alignmentBaseline "middle"
                          , S.fontSize "48px"] [text (String.fromList [c])]
                  ]
                ]
