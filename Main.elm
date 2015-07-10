module Game where

import Time
import Color
import Signal
import Window
import Keyboard
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

{-

Model and Constants

-}

moveFactor = 3
windowDimension = (600,400)

type State
    = Start
    | Move
    | End

type alias Character =
    { xPos:Int
    , yPos:Int
    }

initialChara =
    { xPos=0
    , yPos=0
    }

type alias Game =
    ( State
    , Character
    )

initialGame =
    ( Start
    , initialChara
    )


{-

View

-}

draw (w,h) (state,char) =
    collage w h
    [ rect (toFloat w) (toFloat h) |> filled Color.blue
    , rect 50 50
        |> filled Color.green
        |> move ((toFloat char.xPos), (toFloat char.yPos))
    ]


{-

Controller and Update

-}


toSoundHelper (state,rest) =
    if rest.xPos > 0 then "rightSound" else "leftSound"
    {-
    case state of
      Start -> "startSound"
      Move  -> "moveSound"
      End   -> "pauseSound"
    -}


port handleSound : Signal String
port handleSound =
    Signal.map toSoundHelper gameState


moveCharacter player userInput =
    { xPos = player.xPos + userInput.x * moveFactor
    , yPos = player.yPos + userInput.y * moveFactor
    }


update userInput (state, player) = 
    case state of
      Start -> (Move, player)
      Move  -> (Move, (moveCharacter player userInput))
      -- no transition to end
      End   -> (End, player)


-- Keyboard.wasd
--  ({x=0, y=0},{x=0, y=0}) (Signal.map2 (,) Keyboard.wasd Keyboard.arrows)
gameState = Signal.foldp update initialGame
            (Signal.sampleOn (Time.fps 60) Keyboard.arrows)


main : Signal Element
main = Signal.map2 draw Window.dimensions gameState
-- main = Signal.map2 draw (Signal.constant windowDimension) gameState


