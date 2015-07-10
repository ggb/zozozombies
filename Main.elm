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
    , { position=(0,0), noiseMaker=3 }
    , []
    )


{-

View

-}

draw (w,h) (state,char,zombies) =
  let (xPos,yPos) = char.position
  in
    collage w h
    [ rect (toFloat w) (toFloat h) |> filled Color.grey
    , rect 25 25
        |> filled Color.brown
        |> move ((toFloat xPos), (toFloat yPos))
    ]


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


