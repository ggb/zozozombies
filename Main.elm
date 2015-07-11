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


{-

View

-}
renderImage : Position -> Direction -> Int -> String -> Form
renderImage (xPos,yPos) direction imgNumber name =
  let path = "res/img/" ++ name ++ (toString <| imgNumber + 1) ++ ".png"
      img = toForm (image 25 25 path) |> move ((toFloat xPos), (toFloat yPos))
  in
    case direction of
      North -> img
      West  -> rotate (degrees 90) img
      South -> rotate (degrees 180) img
      East  -> rotate (degrees 270) img


renderStaticImage : Position -> (Int, Int) -> String -> Form
renderStaticImage (xPos, yPos) (dimX, dimY) name =
  let path = "res/img/" ++ name ++ ".png"
  in
    toForm (image dimX dimY path) |> move ((toFloat xPos), (toFloat yPos))


renderZombieImage : Zombie -> Form
renderZombieImage zombie =
  let name = if zombie.zombieType == Brainy then "zombi1_" else "zombi2_"
  in
    renderImage zombie.position zombie.direction zombie.imgNumber name


renderPlayer : Player -> List Form
renderPlayer player =
    [ renderImage player.position player.direction player.imgNumber "erdling_"
    , renderStaticImage player.target (60,20) "kettensaege"
    ]

renderZombies : List Zombie -> List Form
renderZombies =
  List.map renderZombieImage


renderWall : Wall -> Form
renderWall wall =
  let (x1, y1) = wall.start
      (x2, y2) = wall.end
      x = if x1 == x2 then 0 else x2 - x1
      y = if y1 == y2 then 0 else y2 - y1
      (d1, d2) = ( if x == 0 then 10 else x, if y == 0 then 10 else y)
      (m1, m2) = ( (x2 + x1) / 2, (y2 + y1) / 2 )
  in
    rect (toFloat d1) (toFloat d2)
      |> filled Color.darkGrey
      |> move (m1, m2)


renderWalls : List Wall -> List Form
renderWalls =
  List.map renderWall


renderNoiseGenerator : NoiseGenerator -> Form
renderNoiseGenerator noise =
  renderStaticImage noise.position (50,25) "tröte"

renderNoiseGenerators : List NoiseGenerator -> List Form
renderNoiseGenerators =
  List.map renderNoiseGenerator


renderPlayground : (Int, Int) -> Form
renderPlayground (w, h) =
  renderStaticImage (0,0) (800,600) "zombie_spielfeld2"


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
               :: (renderNoiseGenerators noise)
               ++ (renderWalls walls)
               ++ (renderPlayer char)
               ++ (renderZombies zombies))
    Defeat -> renderMessage (w,h) "The zombies enjoyed your delicious brain..."
    Win    -> renderMessage (w,h) "Well done, survivor!"


{-

Controller and Update

-}
toSoundHelper : Game -> String
toSoundHelper (state,player,zombies,noise,walls,seed) =
   if | state == Move   -> "backgroundSound"
      | state == Defeat -> "defeat"
      | state == Win    -> "chainsaw"
      | otherwise       -> ""

port handleSound : Signal String
port handleSound =
    Signal.map toSoundHelper gameState


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


isWallCollision : Position -> Wall -> Bool
isWallCollision pos wall =
  let (x1,y1) = wall.start
      (x2,y2) = wall.end
      (o1,o2) = pos
      targetP = if x1 == x2 && (y1 - 10 <= o2 && o2 <= y2 + 10)
                then Just (x1, o2)
                else if y1 == y2  && (x1 - 10 <= o1 && o1 <= x2 + 10)
                     then Just (o1, y1)
                     else Nothing
  in
    case targetP of
      Just point -> euclidianDistance pos point < 10
      Nothing    -> False


detectWallCollision : Position -> List Wall -> Bool
detectWallCollision pos =
  List.any (isWallCollision pos)


directionHelper : { x:Int, y:Int } -> Direction -> Direction
directionHelper userInput oldDirection =
  let heading = ( userInput.x, userInput.y )
  in
    case heading of
      (-1, 0) -> West
      ( 1, 0) -> East
      ( 0,-1) -> South
      ( 0, 1) -> North
      ( _, _) -> oldDirection


moveCharacter : { x:Int, y:Int } -> Player -> List Wall -> Player
moveCharacter userInput player walls =
  let (xPos,yPos) = player.position
      newPos = ( xPos + userInput.x * moveFactor, yPos + userInput.y * moveFactor )
      collision = detectWallCollision newPos walls
      newDirection = directionHelper userInput player.direction
      newImgNumber = (player.imgNumber + 1) % 6
  in
    if userInput == { x=0, y=0 } || collision
    then player
    else { player | position <- newPos
                  , direction <- newDirection
                  , imgNumber <- newImgNumber }


updateZombieState : Zombie -> Position -> ZombieState
updateZombieState zombie target =
  let zPos = zombie.position
      pPos = target
      distance = euclidianDistance zPos pPos
  in
    if | distance < 200 -> Hunting
       | distance < 300 -> Aggressive
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
       | absX <  absY && y <  0 -> South
       | absX <  absY && y >= 0 -> North


zombieMoveHelper : (Int,Int) -> Int -> Direction -> (Int,Int)
zombieMoveHelper (posX,posY) mFactor heading =
  case heading of
    West  -> ( posX - mFactor, posY )
    East  -> ( posX + mFactor, posY )
    North -> ( posX, posY + mFactor )
    South -> ( posX, posY - mFactor )

getTarget : Zombie -> List Position -> Position
getTarget zombie targets =
  List.map (euclidianDistance zombie.target) targets
  |> List.map2 (,) targets
  |> List.sortBy (\(element, distance) -> distance)
  |> List.map (\(element, distance) -> element)
  |> List.head
  |> Maybe.withDefault (0,0)


otherZombieHelper : Zombie -> Zombie -> Bool
otherZombieHelper fst scd =
  if fst.position == scd.position
  then False
  else euclidianDistance fst.position scd.position < 10


isOtherZombieNear : Zombie -> List Zombie -> Bool
isOtherZombieNear zombie =
  List.any (\other -> otherZombieHelper zombie other)


moveZombie : Zombie -> List Zombie -> Player -> List NoiseGenerator -> Direction -> List Wall -> Zombie
moveZombie zombie zombies player noise heading walls =
  let target = getTarget zombie <| ( player.position :: List.map (\ele -> ele.position) noise )
      speed = if zombie.state == Hunting
              then 3
              else zombieMoveFactor
      newDir = if zombie.state == Idle
               then heading
               else determineDirection zombie target
      newPos = zombieMoveHelper zombie.position speed heading
      collision = detectWallCollision newPos walls
      zombieCollision = isOtherZombieNear zombie zombies
      newState = if collision || zombieCollision
                 then Idle
                 else updateZombieState zombie target
      newImgNumber = (zombie.imgNumber + 1) % 6
  in
    { zombie | position  <- if collision then zombie.position else newPos
             , direction <- newDir
             , state     <- newState
             , imgNumber <- newImgNumber}


moveZombies : Player -> List NoiseGenerator -> List Direction -> List Wall ->  List Zombie -> List Zombie
moveZombies player noise headings walls  zombies =
  List.map ( \(zombie, heading) -> moveZombie zombie zombies player noise heading walls)
  <| List.map2 (,)  zombies headings


isNearHelper : Position -> List NoiseGenerator -> Bool
isNearHelper playerPos noise =
  List.any
    (\noise -> euclidianDistance playerPos noise.position < 30)
    -- this is not solved  very elegant...
    ( { position=(-300,200) } :: noise )


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
        , moveCharacter arrows player walls
        , moveZombies player noise headings walls zombies
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
                     (Signal.sampleOn (Time.fps 40) Keyboard.arrows) Keyboard.space)


main : Signal Element
main = Signal.map2 draw (Signal.constant windowDimension) gameState


