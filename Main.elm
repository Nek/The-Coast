import Random

data Tile = Grass | Sand | Rock | Water

colorOf tile =
    case tile of 
        Grass -> rgba 0 255 0 1
        Sand -> rgba 255 255 0 1
        Rock -> rgba 125 125 125 1
        Water -> rgba 125 125 255 1

tileWidth = 16
tileHeight = 16
sceneWidth = 480
sceneHeight = 320
levelWidth = 12
levelHeight = 8


makeTile n = head (drop n [Grass, Rock, Sand, Water])

makeShape : Int -> Int -> Tile -> Form
makeShape x y tile=
    let tw = tileWidth in
    let th = tileHeight in
    let w = sceneWidth `div` 2 in
    let h = sceneHeight `div` 2 in  
    filled (colorOf <| tile) (rect tw th) |> move (x * tw - 0 
                                       |> toFloat, y * (0-th) + 0 
                                       |> toFloat)

makeBlock x y = 
    ((makeShape x y) <~ (makeTile <~ Random.range 0 2 (constant Nothing)))

makeNthBlock n = 
    let x = n `mod` levelWidth in
    let y = n `div` levelWidth in
    makeBlock x y

makeCollage : [Form] -> Element
makeCollage fs =
    collage sceneWidth sceneHeight fs

makeBlocksList = combine (map makeNthBlock [0..(levelWidth * levelHeight - 1)])

main = makeCollage <~ makeBlocksList