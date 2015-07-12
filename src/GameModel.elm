module GameModel where

import Random exposing (Seed)

{-

Model and Constants

-}

moveFactor : Int
moveFactor = 3

zombieMoveFactor : Int
zombieMoveFactor = 1

zombieFastMoveFactor : Int
zombieFastMoveFactor = 3

windowDimension : (Int,Int)
windowDimension = (800,600)

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
    , imgNumber:Int
    }

type ZombieState
    = Idle
    | Aggressive
    | Hunting

type ZombieType
    = Brainy
    | Frankenstein

type alias Zombie =
    { position:Position
    , target:Position
    , state:ZombieState
    , direction:Direction
    , imgNumber:Int
    , zombieType:ZombieType
    }

type alias Wall =
    { start:Position
    , end:Position
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
    , Seed
    )

initialGame : Game
initialGame =
    ( Start
    , { position=(-300,200), target=(250,-100), noiseGeneratorCount=3, direction=East, imgNumber=0 }
    , [ { position=(150,100), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Frankenstein }
      , { position=(150,-100), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Frankenstein }
      , { position=(150,0), target=(0,0), state=Idle, direction=North, imgNumber=0, zombieType=Brainy }
      , { position=(250,50), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Brainy }
      , { position=(0,50), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Brainy }
      , { position=(0,100), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Frankenstein }
      , { position=(50,-150), target=(0,0), state=Idle, direction=East, imgNumber=0, zombieType=Frankenstein }
      , { position=(-50,-250), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Brainy }
      , { position=(-250,-250), target=(0,0), state=Idle, direction=North, imgNumber=0, zombieType=Brainy }
      , { position=(-250,-150), target=(0,0), state=Idle, direction=North, imgNumber=0, zombieType=Frankenstein }
      , { position=(250,250), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Brainy }
      , { position=(0,0), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Frankenstein }
      , { position=(250,250), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Brainy }
      , { position=(50,50), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Frankenstein }
      , { position=(200,150), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Brainy }
      , { position=(-50,-50), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Frankenstein }
      ]
    , [
        -- outer frame
        { start=(405,-300), end=(405,300) }
      , { start=(-405,-300), end=(-405,300) }
      , { start=(-400,305), end=(400,305) }
      , { start=(-400,-305), end=(400,-305)}
        -- walls, right room
      , { start=(100,200), end=(400,200) }
      , { start=(100,205), end=(100,150) }
      , { start=(100,-300), end=(100,80)}
        -- left room
      , { start=(-100,-100), end=(-100,300)}
      , { start=(-100,-300), end=(-100,-200)}
      ]
    , []
    , Random.initialSeed 42
    )
