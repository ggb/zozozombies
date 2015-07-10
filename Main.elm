module Game where

import Time
import Color
import Signal
import Random exposing (Generator,Seed)
import Keyboard
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

{-

Model and Constants

-}

moveFactor : Int
moveFactor = 3

zombieMoveFactor : Int
zombieMoveFactor = 1

windowDimension : (Int,Int)
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
    , Seed
    )

initialGame : Game
initialGame =
    ( Start
    , { position=(-200,0), target=(200,0), noiseGeneratorCount=3, direction=East, imgNumber=0 }
    , [ { position=(100,100), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Frankenstein }
      , { position=(100,-100), target=(0,0), state=Idle, direction=West, imgNumber=0, zombieType=Frankenstein }
      , { position=(100,0), target=(0,0), state=Idle, direction=North, imgNumber=0, zombieType=Brainy }
      , { position=(250,50), target=(0,0), state=Idle, direction=South, imgNumber=0, zombieType=Brainy }
      ]
    , []
    , []
    , Random.initialSeed 42
    )


{-

View

-}
renderImage : Position -> Direction -> Int -> String -> Form
renderImage position direction imgNumber name =
  let (xPos,yPos) = position
      path = "res/img/" ++ name ++ (toString <| imgNumber + 1) ++ ".png"
      img = toForm (image 25 25 path) |> move ((toFloat xPos), (toFloat yPos))
  in
    case direction of
      North -> img
      West  -> rotate (degrees 90) img
      South -> rotate (degrees 180) img
      East  -> rotate (degrees 270) img


renderZombieImage : Zombie -> Form
renderZombieImage zombie =
  let name = if zombie.zombieType == Brainy then "zombi1_" else "zombi2_"
  in
    renderImage zombie.position zombie.direction zombie.imgNumber name


renderPlayer : Player -> List Form
renderPlayer player =
  let (targetX,targetY) = player.target
  in
    [ renderImage player.position player.direction player.imgNumber "erdling_"
    , rect 25 25
      |> filled Color.green
      |> move ((toFloat targetX), (toFloat targetY))
    ]

renderZombies : List Zombie -> List Form
renderZombies =
  List.map renderZombieImage


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
draw (w,h) (state,char,zombies,walls,noise,seed) =
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


directionHelper : { x:Int, y:Int } -> Direction -> Direction
directionHelper userInput oldDirection =
  let heading = ( userInput.x, userInput.y )
  in
    case heading of
      (-1, 0) -> West
      ( 1, 0) -> East
      ( 0,-1) -> South
      ( 0, 1) -> North
      -- why is this necessary?
      ( _, _) -> oldDirection


moveCharacter : { x:Int, y:Int } -> Player -> Player
moveCharacter userInput player =
  let (xPos,yPos) = player.position
      newPos = ( xPos + userInput.x * moveFactor, yPos + userInput.y * moveFactor )
      newDirection = directionHelper userInput player.direction
      newImgNumber = (player.imgNumber + 1) % 6
  in
    if userInput == { x=0, y=0 }
    then player
    else { player | position <- newPos
                  , direction <- newDirection
                  , imgNumber <- newImgNumber }


updateZombieState : Zombie -> Player -> List NoiseGenerator -> ZombieState
updateZombieState zombie player noise =
  let zPos = zombie.position
      pPos = player.position
      distance = euclidianDistance zPos pPos
  in
    if | distance < 125 -> Hunting
       | distance < 200 -> Aggressive
       | otherwise      -> Idle


determineDirection : Zombie -> Position -> Direction
determineDirection zombie object =
  let (zX, zY) = zombie.position
      (oX, oY) = object
      ( x,  y) = (oX - zX, oY - zY)
      (absX, absY) = (abs x, abs y)
  in
    if | absX >= absY && x <  0 -> West
       | absX >= absY && x >= 0 -> East
       | absX <  absY && x <  0 -> South
       | absX <  absY && x >= 0 -> North


zombieMoveHelper : (Int,Int) -> Int -> Direction -> (Int,Int)
zombieMoveHelper (posX,posY) mFactor heading =
  case heading of
    West  -> ( posX - mFactor, posY )
    East  -> ( posX + mFactor, posY )
    North -> ( posX, posY + mFactor )
    South -> ( posX, posY - mFactor )


idleZombieMove : Zombie -> Player -> List NoiseGenerator -> Direction -> Zombie
idleZombieMove zombie target noise heading =
  let newPos   = zombieMoveHelper zombie.position zombieMoveFactor heading
      newState = updateZombieState zombie target noise
      newImgNumber = (zombie.imgNumber + 1) % 6
  in
    { zombie | position  <- newPos
             , direction <- heading
             , state     <- newState
             , imgNumber <- newImgNumber}


aggressiveZombieMove : Zombie -> Player -> List NoiseGenerator -> Direction -> Zombie
aggressiveZombieMove zombie target noise heading =
  let newDir = determineDirection zombie target.position
      newPos  = zombieMoveHelper zombie.position zombieMoveFactor newDir
      newState = updateZombieState zombie target noise
      newImgNumber = (zombie.imgNumber + 1) % 6
  in
    { zombie | position  <- newPos
             , state     <- newState
             , imgNumber <- newImgNumber
             , direction <- newDir }


huntingZombieMove : Zombie -> Player -> List NoiseGenerator -> Direction -> Zombie
huntingZombieMove zombie target noise heading =
  let newDir = determineDirection zombie target.position
      newPos = zombieMoveHelper zombie.position 3 newDir
      newState = updateZombieState zombie target noise
      newImgNumber = (zombie.imgNumber + 1) % 6
  in
    { zombie | position  <- newPos
             , state     <- newState
             , imgNumber <- newImgNumber
             , direction <- newDir }


moveZombie : Zombie -> Player -> List NoiseGenerator -> Direction -> Zombie
moveZombie zombie =
  case zombie.state of
    Idle       -> idleZombieMove zombie
    Aggressive -> aggressiveZombieMove zombie
    Hunting    -> huntingZombieMove zombie


moveZombies : Player -> List NoiseGenerator -> List Direction -> List Zombie -> List Zombie
moveZombies player noise headings zombies =
  List.map ( \(zombie, heading) -> moveZombie zombie player noise heading)
  <| List.map2 (,)  zombies headings


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


randomHeading : Generator Int
randomHeading = Random.int 0 3


randomChangeDir : Generator Int
randomChangeDir = Random.int 0 9


randomHeadings : Int -> Generator (List Int)
randomHeadings count = Random.list count randomHeading


randomDirections : Int -> Generator (List Int)
randomDirections count = Random.list count randomChangeDir


headingHelper : (Int,Int,Zombie) -> Direction
headingHelper (randNum, randChange, zombie) =
  if randChange == 0
  then
    case randNum of
      0 -> North
      1 -> East
      2 -> South
      3 -> West
  else
    zombie.direction


createHeadings : Seed -> List Zombie -> (Seed,List Direction)
createHeadings seed zombies =
  let (randList, listSeed) = Random.generate (List.length zombies |> randomHeadings) seed
      (randChangeDir, dirSeed) = Random.generate (List.length zombies |> randomDirections) listSeed
  in
    ( dirSeed, List.map headingHelper <| List.map3 (,,) randList randChangeDir zombies )


update : ({x:Int, y:Int},Bool) -> Game -> Game
update userInput game =
  let (state, player, zombies, walls, noise, seed) = game
      (arrows, space) = userInput
      (nextSeed, headings) = createHeadings seed zombies
  in
    case state of
      Start  ->
        if space
        then (Move, player, zombies, walls, noise, nextSeed)
        else game
      Move   ->
        ( winOrLose player zombies
        , moveCharacter arrows player
        , moveZombies player noise headings zombies
        , walls
        , if space
          then placeNoiseGenerator player noise
          else noise
        , nextSeed
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


gameState : Signal Game
gameState = Signal.foldp update initialGame
            (Signal.map2 (,)
                     (Signal.sampleOn (Time.fps 60) Keyboard.arrows) Keyboard.space)


main : Signal Element
main = Signal.map2 draw (Signal.constant windowDimension) gameState


