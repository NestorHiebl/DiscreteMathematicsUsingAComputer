import Stdm

data Colour = Red | Blue | Green | Orange | Yellow | Violet
    deriving (Eq, Show)

colourComplement :: Digraph Colour
colourComplement =
    ([Red, Blue, Green, Orange, Yellow, Violet],
     [
        (Red, Green),
        (Green, Red),
        (Blue, Orange),
        (Orange, Blue),
        (Yellow, Violet),
        (Violet, Yellow)
     ])

