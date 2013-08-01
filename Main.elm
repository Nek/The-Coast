data Tile = Grass | Sand | Rock | Water

colorOf tile =
    case tile of 
        Grass -> rgba 0 255 0 1
        Sand -> rgba 255 255 0 1
        Rock -> rgba 125 125 125 1
        Water -> rgba 125 125 255 1

level = [
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass,    
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass,
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass,
    Grass, Grass, Grass, Rock, Grass, Grass, Grass, Grass,
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass,
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass,
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass,
    Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass
    ]

makeShape : Tile -> Int -> Form
makeShape tile coord =
    let x = coord `mod` 8 in 
    let y = coord `div` 8 in
    filled (colorOf tile) (rect 32 32) |> move (x * 32 - 240 
                                       |> toFloat, y * (0-32) + 160  
                                       |> toFloat)

main = collage 480 320 
    (zipWith makeShape level [0..63])