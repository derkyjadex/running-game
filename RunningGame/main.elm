import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage, Form, rect, filled, moveX, move, scale, group)
import Color exposing (red, green)
import Signal exposing (constant, Signal, merge)
import Time exposing (fps)
import Keyboard exposing (space)
import List exposing (map)

type alias Player = {
    x : Float,
    y : Float,
    dx : Float,
    dy : Float
}

type Event
    = Update Float
    | Jump
    | NoEvent

gravity : Float
gravity = -0.00005

jumpVelocity : Float
jumpVelocity = 0.02 


terrain : Int -> Maybe Int
terrain _ = Just 3

emptyForm : Form
emptyForm = filled green <| rect 0.0 0.0 -- NOOOOOOOO

renderTerrainSegment : Int -> Form
renderTerrainSegment x = case terrain x of
    Just y -> move ((toFloat x), 0.5 * (toFloat y)) <| filled green <| rect 1.0 (toFloat y)
    Nothing -> emptyForm

renderTerrain : (Int, Int) -> Form
renderTerrain (min, max) =
    group <| map renderTerrainSegment [min..max]

renderPlayer : Player -> Form
renderPlayer p =
    rect 1.0 2.0
    |> filled red
    |> move (p.x, p.y + 1)

canJump : Player -> Bool
canJump p =
    let ix = floor p.x
        ground = Maybe.map toFloat <| terrain ix
    in case ground of
        Just g -> p.y <= g
        Nothing -> False

clip : Player -> Player
clip p =
    let ix = floor p.x
        ground = Maybe.map toFloat <| terrain ix
    in case ground of
        Just g -> if p.y < g
                     then {p | y <- g}
                     else p
        Nothing -> p

updatePlayer : Event -> Player -> Player
updatePlayer e p = case e of
    Update dt ->
        let dy' = p.dy + (gravity * dt)
            y' = p.y + (dy' * dt)
         in clip { p | dy <- dy', y <- y' }
    Jump -> if canJump p
               then { p | dy <- jumpVelocity }
               else p
    _ -> p

jumpEvents : Signal Event
jumpEvents = Signal.map (\x -> if x then Jump else NoEvent) space

updateEvents : Signal Event
updateEvents = Signal.map Update (fps 60)

events : Signal Event
events =
    merge jumpEvents updateEvents

initialPlayer : Player
initialPlayer = { x = 0, y = 3, dx = 0, dy = 0 }

player : Signal Player
player = Signal.foldp updatePlayer initialPlayer events


main : Signal Element
main = Signal.map
    (\ player -> collage 600 600 [
        move (0, -300) <| scale 80 <| group [
            renderTerrain (0, 10),
            renderPlayer player
            ]
        ]
    )
    player

