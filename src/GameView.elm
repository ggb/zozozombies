module GameView where

import GameModel exposing (..)
import Graphics.Collage exposing (Form,toForm,move,rotate,rect,filled,collage)
import Graphics.Element exposing (Element,image,centered,size,color)
import Color
import Text

{-

View

-}
renderImage : Position -> Direction -> Int -> String -> Form
renderImage (xPos,yPos) direction imgNumber name =
  let path = "res/img/" ++ name ++ (toString <| imgNumber + 1) ++ ".png"
      img = toForm (image 35 35 path) |> move ((toFloat xPos), (toFloat yPos))
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

