module Main exposing (..)

import Html exposing (Html, Attribute, div, p, text, img)
import Html.Attributes exposing (class, style, src)
import Html.Events exposing (on, keyCode)
import Keyboard

type Direction = S | E | N | W

type Sense = L | R

type Probe = Probe
             { position : (Int, Int)
             , direction : Direction
             }

type Plateau = Plateau
               { maxX : Int
               , maxY : Int
               , probes : List Probe
               }

minX : Int
minX = 1

minY : Int
minY = 1

cw : Direction -> Direction
cw direction = case direction of
                   S -> W
                   W -> N
                   N -> E
                   E -> S

ccw : Direction -> Direction
ccw direction = case direction of
                   S -> E
                   E -> N
                   N -> W
                   W -> S

turn : Sense -> Probe -> Probe
turn sense (Probe rec) =
    let
        newdir = case sense of
                     R -> cw rec.direction
                     L -> ccw rec.direction
    in
        Probe {rec | direction = newdir}

shift : Direction -> (Int, Int) -> (Int, Int)
shift dir (x, y) =
    case dir of
        S -> (x, y - 1)
        E -> (x + 1, y)
        N -> (x, y + 1)
        W -> (x - 1, y)

isInside : Plateau -> (Int, Int) -> Bool
isInside (Plateau {maxX, maxY}) (x, y) =
    case (minX <= x && x <= maxX, minY <= y && y <= maxY) of
        (True, True) -> True
        _            -> False

tryMove : Plateau -> Probe -> Maybe Probe
tryMove plateau probe =
    let
        (Probe {position, direction}) = probe
        (Plateau {maxX, maxY}) = plateau
        newPos = shift direction position
        inside = isInside plateau newPos
        newProbe = Probe {position = newPos, direction = direction}
    in
        if inside then Just newProbe else Nothing

fetchProbe : Plateau -> ProbeIndex -> Maybe Probe
fetchProbe plateau i =
    let
        (Plateau {maxX, maxY, probes}) = plateau
        go : List Probe -> ProbeIndex -> Maybe Probe
        go lst ind =
            case lst of
                [] -> Nothing
                (x::xs) ->
                    if ind == 0
                        then Just x
                        else go xs (ind - 1)
    in
        go probes i

updateProbe : Plateau -> ProbeIndex -> Probe -> Maybe Plateau
updateProbe plateau i newProbe =
    let
        (Plateau {maxX, maxY, probes}) = plateau
        go lst ind =
            if List.length lst <= ind
                then Nothing
                else
                  let
                      before = List.take ind lst
                      after  = List.drop (ind + 1) lst
                  in
                      Just (before ++ [newProbe] ++ after)
    in
        case go probes i of
            Nothing -> Nothing
            Just ps ->
                Just (Plateau {maxX = maxX, maxY = maxY, probes = ps})

-- Model

type alias Model = Plateau

init : Model
init = Plateau {maxX = 10, maxY = 10, probes = []}

-- Message

type alias ProbeIndex = Int

type Msg = TurnRight ProbeIndex
         | TurnLeft ProbeIndex
         | Move ProbeIndex
         | KeyPressed Keyboard.KeyCode

-- Views

myStyle : Model -> Attribute msg
myStyle _ =
    style
    [ ("width", "300px")
    , ("height", "300px")
    , ("background-color", "orange")
    , ("position", "relative")
    , ("top", "15px")
    , ("left", "15px")
    ]

probeView : Probe -> Html Msg
probeView probe  =
    let
        (Probe {position, direction}) = probe
        (x, y) = position
    in
        div
          [ style
            [ ("width", "15px")
            , ("height", "15px")
            , ("backgroun-color", "red")
            , ("position", "absolute")
            , ("top", toString y ++ "px")
            , ("left", toString x ++ "px")
            ]
          ]
          [
            img
            [ src "assets/WALLE.png"
            , style
              [ ("width", "15px")
              , ("height", "15px")
              ]
            ]
            []
          ]

view : Model -> Html Msg
view model =
    let
        (Plateau {maxX, maxY, probes}) = model
        probe = case List.head probes of
                    Nothing -> Probe {position = (0, 0), direction = N}
                    Just probe -> probe
    in
        div
        [ myStyle model ]
        [ probeView probe
        ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [

        ]

-- Updates

update : Msg -> Model -> Model
update msg model =
    let
        (Plateau {maxX, maxY, probes}) = model
    in
        case msg of
            TurnLeft i ->
                case fetchProbe model i of
                    Nothing -> model
                    Just probe ->

                model
            TurnRight i ->
                model
            Move i ->
                model

main = Html.beginnerProgram
       { model = init
       , view = view
       , update = update
       }
