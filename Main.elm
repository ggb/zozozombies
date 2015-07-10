module Game where

import Time
import Color
import Signal
-- import Window
import Keyboard
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
    | End

type alias Position = ( Int, Int )

type alias Player =
    { position:Position
    , noiseMaker:Int
    }

type ZombieState
    = Idle
    | Aggressive
    | Hunting

type alias Zombie =
    { position:Position
    , state:ZombieState
    }

type alias Game =
    ( State
    , Player
    , List Zombie
    )

initialGame : Game
initialGame =
    ( Start
    , { position=(-200,0), noiseMaker=3 }
    , [ { position=(100,100), state=Idle }
      , { position=(100,-100), state=Idle }
      , { position=(100,0), state=Idle }
      , { position=(250,50), state=Idle }
      ]
    )


{-

View

-}
renderPlayer : Player -> Form
renderPlayer player =
  let (xPos,yPos) = player.position
  in
    rect 25 25
      |> filled Color.brown
      |> move ((toFloat xPos), (toFloat yPos))


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


draw : (Int, Int) -> Game -> Element
draw (w,h) (state,char,zombies) =
  collage w h
    ( renderPlayground (w,h) :: ( renderPlayer char :: renderZombies zombies ))


{-

Controller and Update

-}


toSoundHelper (state,pc,npc) =
    "sampleSound"

port handleSound : Signal String
port handleSound =
    Signal.map toSoundHelper gameState


moveCharacter player userInput =
  let (xPos,yPos) = player.position
      newPos = ( xPos + userInput.x * moveFactor, yPos + userInput.y * moveFactor )
  in
    { position=newPos, noiseMaker=player.noiseMaker}


update userInput (state, player, zombies) = 
    case state of
      Start -> (Move, player, zombies)
      Move  -> (Move, (moveCharacter player userInput), zombies)
      -- no transition to end
      End   -> (End, player, zombies)


-- Keyboard.wasd
--  ({x=0, y=0},{x=0, y=0}) (Signal.map2 (,) Keyboard.wasd Keyboard.arrows)
gameState = Signal.foldp update initialGame
            (Signal.sampleOn (Time.fps 60) Keyboard.arrows)


main : Signal Element
main = Signal.map2 draw (Signal.constant windowDimension) gameState
-- main = Signal.map2 draw (Signal.constant windowDimension) gameState


