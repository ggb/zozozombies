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


type Direction = North | East | South | West


type alias Player =
    { position:Position
    , target:Position
    , noiseGeneratorCount:Int
    , direction:Direction
    }

type ZombieState
    = Idle
    | Aggressive
    | Hunting

type alias Zombie =
    { position:Position
    , target:Position
    , state:ZombieState
    , direction:Direction
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
    , { position=(-200,0), target=(200,0), noiseGeneratorCount=3, direction=East }
    , [ { position=(100,100), target=(0,0), state=Idle, direction=West }
      , { position=(100,-100), target=(0,0), state=Idle, direction=West }
      , { position=(100,0), target=(0,0), state=Idle, direction=North }
      , { position=(250,50), target=(0,0), state=Idle, direction=South }
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
renderZombies =
  List.map renderSingleZombie


renderNoiseGenerator : NoiseGenerator -> Form
renderNoiseGenerator noise =
  let (xPos,yPos) = noise.position
  in
    rect 15 15
      |> filled Color.yellow
      |> move ((toFloat xPos), (toFloat yPos))


renderNoiseGenerators : List NoiseGenerator -> List Form
renderNoiseGenerators =
  List.map renderNoiseGenerator


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
              (( renderPlayground (w,h))
               :: (renderNoiseGenerators noise) ++ (renderPlayer char) ++ (renderZombies zombies))
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


directionHelper : { x:Int, y:Int } -> Direction
directionHelper userInput =
  let heading = ( userInput.x, userInput.y )
  in
    case heading of
      (-1, 0) -> West
      ( 1, 0) -> East
      ( 0,-1) -> South
      ( 0, 1) -> North


moveCharacter : { x:Int, y:Int } -> Player -> Player
moveCharacter userInput player =
  let (xPos,yPos) = player.position
      newPos = ( xPos + userInput.x * moveFactor, yPos + userInput.y * moveFactor )
      newDirection = directionHelper userInput
  in
    { position = newPos
    , noiseGeneratorCount = player.noiseGeneratorCount
    , target = player.target
    , direction = newDirection }


idleZombieMove : Zombie -> Player -> List NoiseGenerator -> Zombie
idleZombieMove zombie target noise = zombie


aggressiveZombieMove : Zombie -> Player -> List NoiseGenerator -> Zombie
aggressiveZombieMove zombie target noise = zombie


huntingZombieMove : Zombie -> Player -> List NoiseGenerator -> Zombie
huntingZombieMove zombie target noise = zombie


moveZombie : Zombie -> Player -> List NoiseGenerator -> Zombie
moveZombie zombie =
  case zombie.state of
    Idle       -> idleZombieMove zombie
    Aggressive -> aggressiveZombieMove zombie
    Hunting    -> huntingZombieMove zombie


moveZombies : Player -> List NoiseGenerator -> List Zombie -> List Zombie
moveZombies player noise zombies =
  List.map (\zombie -> moveZombie zombie player noise) zombies


isNearHelper : Position -> List NoiseGenerator -> Bool
isNearHelper playerPos noise =
  List.any
    (\noise -> euclidianDistance playerPos noise.position < 30)
    -- this is not solved  very elegant...
    ( { position=(-200,0) } :: noise )


placeNoiseGenerator : Player -> List NoiseGenerator -> List NoiseGenerator
placeNoiseGenerator player noise =
  if (player.noiseGeneratorCount > List.length noise) && not (isNearHelper player.position noise)
  then { position=player.position  } :: noise
  else noise


update : ({x:Int, y:Int},Bool) -> Game -> Game
update userInput game =
  let (state, player, zombies, walls, noise) = game
      (arrows, space) = userInput
  in
    case state of
      Start  ->
        if space
        then (Move, player, zombies, walls, noise)
        else game
      Move   ->
        ( winOrLose player zombies
        , moveCharacter arrows player
        , moveZombies player noise zombies
        , walls
        , if space
          then placeNoiseGenerator player noise
          else noise
        )
      -- no transition to end
      Win    ->
        if space
        then initialGame
        else game
      Defeat ->
        if space
        then initialGame
        else game


-- Keyboard.wasd
--  ({x=0, y=0},{x=0, y=0}) (Signal.map2 (,) Keyboard.wasd Keyboard.arrows)
gameState : Signal Game
gameState = Signal.foldp update initialGame
            (Signal.map2 (,)
                     (Signal.sampleOn (Time.fps 60) Keyboard.arrows) Keyboard.space)


main : Signal Element
main = Signal.map2 draw (Signal.constant windowDimension) gameState
-- main = Signal.map2 draw (Signal.constant windowDimension) gameState


