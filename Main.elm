module Game where

import Time
import Color
import Signal
-- import Window
import Keyboard
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

{-

Model and Constants

-}

moveFactor : number
moveFactor = 3

windowDimension : (number, number')
windowDimension = (600,400)

type State
    = Start
    | Move
    | Win
    | Defeat

type alias Position = ( Int, Int )

type alias Player =
    { position:Position
    , target:Position
    , noiseMaker:Int
    }

type ZombieState
    = Idle
    | Aggressive
    | Hunting

type alias Zombie =
    { position:Position
    , target:Position
    , state:ZombieState
    }

type alias Wall =
    { x1:Int
    , y1:Int
    , x2:Int
    , y2:Int
    }

type alias NoiseGenerator =
    { position:Position
    }

type alias Game =
    ( State
    , Player
    , List Zombie
    , List Wall
    , List NoiseGenerator
    )

initialGame : Game
initialGame =
    ( Start
    , { position=(-200,0), target=(200,0), noiseMaker=3 }
    , [ { position=(100,100), target=(0,0), state=Idle }
      , { position=(100,-100), target=(0,0), state=Idle }
      , { position=(100,0), target=(0,0), state=Idle }
      , { position=(250,50), target=(0,0), state=Idle }
      ]
    , []
    , []
    )


{-

View

-}
renderPlayer : Player -> List Form
renderPlayer player =
  let (playerX,playerY) = player.position
      (targetX,targetY) = player.target
  in
    [ rect 25 25
      |> filled Color.brown
      |> move ((toFloat playerX), (toFloat playerY))
    , rect 25 25
      |> filled Color.green
      |> move ((toFloat targetX), (toFloat targetY))
    ]

renderSingleZombie : Zombie -> Form
renderSingleZombie zombie =
  let (xPos,yPos) = zombie.position
  in
    rect 20 20
      |> filled Color.red
      |> move ((toFloat xPos), (toFloat yPos))


renderZombies : List Zombie -> List Form
renderZombies zombies =
  List.map renderSingleZombie zombies


renderPlayground : (Int, Int) -> Form
renderPlayground (w, h) =
  let w' = toFloat w
      h' = toFloat h
  in
    rect w' h' |> filled Color.grey


renderMessage : (Int,Int) -> String -> Element
renderMessage (w,h) message =
  Text.fromString message
  |> centered
  |> size w h
  |> color Color.grey


draw : (Int, Int) -> Game -> Element
draw (w,h) (state,char,zombies,walls,noise) =
  case state of
    Start  -> renderMessage (w,h) "Please press space to start the game!"
    Move   -> collage w h
              (( renderPlayground (w,h)) :: ( renderPlayer char) ++ (renderZombies zombies ))
    Defeat -> renderMessage (w,h) "The zombies enjoyed your delicious brain..."
    Win    -> renderMessage (w,h) "Well done, survivor!"


{-

Controller and Update

-}

{-
toSoundHelper (state,pc,npc) =
    "sampleSound"

port handleSound : Signal String
port handleSound =
    Signal.map toSoundHelper gameState
-}
euclidianDistance : Position -> Position -> Float
euclidianDistance (p1, p2) (q1, q2) =
  (q1 - p1)^2 + (q2 - p2)^2 |> toFloat |> sqrt


winSituation : Player -> Bool
winSituation player =
  ( euclidianDistance player.position player.target ) < 20


loseSituation : Player -> List Zombie -> Bool
loseSituation player zombies =
  List.any (\zombie -> euclidianDistance player.position zombie.position < 20) zombies


winOrLose : Player -> List Zombie -> State
winOrLose player zombies =
  if winSituation player
  then Win
  else
    if loseSituation player zombies
    then Defeat
    else Move


moveCharacter : { a | x : number, y : number } -> Player -> Player
moveCharacter userInput player =
  let (xPos,yPos) = player.position
      newPos = ( xPos + userInput.x * moveFactor, yPos + userInput.y * moveFactor )
  in
    { position=newPos, noiseMaker=player.noiseMaker, target=player.target}


idleZombieMove target zombie = zombie


aggressiveZombieMove target zombie = zombie


huntingZombieMove target zombie = zombie


moveZombie : Player -> Zombie -> Zombie
moveZombie player zombie =
  case zombie.state of
    Idle       -> idleZombieMove player zombie
    Aggressive -> aggressiveZombieMove player zombie
    Hunting    -> huntingZombieMove player zombie


moveZombies : Player -> List Zombie -> List Zombie
moveZombies player zombies =
  List.map (moveZombie player) zombies


update : { a | x : number, y : number } -> Game -> Game
update userInput (state, player, zombies, walls, noise) =
    case state of
      Start  -> (Move, player, zombies, walls, noise)
      Move   ->
        ( winOrLose player zombies
        , moveCharacter userInput player
        , moveZombies player zombies
        , walls
        , noise )
      -- no transition to end
      Win    -> (Win, player, zombies, walls, noise)
      Defeat -> (Defeat, player, zombies, walls, noise)


-- Keyboard.wasd
--  ({x=0, y=0},{x=0, y=0}) (Signal.map2 (,) Keyboard.wasd Keyboard.arrows)
gameState : Signal Game
gameState = Signal.foldp update initialGame
            (Signal.sampleOn (Time.fps 60) Keyboard.arrows)


main : Signal Element
main = Signal.map2 draw (Signal.constant windowDimension) gameState
-- main = Signal.map2 draw (Signal.constant windowDimension) gameState


