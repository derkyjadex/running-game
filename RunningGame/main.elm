import Graphics
import Graphics.Collage (Collage, Form, rectangle)
import Signal (constant)

type alias Player = {
    x : Float,
    y : Float,
    dx : Float,
    dy : Float,
}

data Event
    = Update Float
    | Jump
    | NoEvent


-- (x, y) -> bool
terrain : Int -> Maybe Int
terrain _ = Just 3

renderTerrainSegment : Int -> Form
renderTerrainSegment x = case terrain x of 
    Just y = moveX x <| rectangle 1.0 y
    Nothing = rectangle 0.0 0.0  -- NOOOOOOOO

renderTerrain : (Int, Int) -> Form
renderTerrain (min, max) = moveX (0.5 * (toFloat min)) (toFloat max)

renderPlayer : Player -> Form
renderPlayer p -> 

updatePlayer : Player 



events = Signal Event


player : Signal (Float, Float)
player = constant (4, 4)




main : Element
main 





