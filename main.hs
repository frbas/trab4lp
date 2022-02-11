data Final = Inter { mini :: Int
                    ,maxi :: Int
                    } deriving (Show)

type Inicial = (Int, Int, Bool, Bool)

processa :: Inicial -> Final
processa (x, y, xc, yc) 
    | xc == False && yc == False = Inter (x+1) (y-1)
    | xc == False && yc == True = Inter (x+1) y
    | xc == True && yc == False = Inter x (y-1)
    | xc == True && yc == True = Inter x y

lisinter :: Final -> [Int]
lisinter (Inter mini maxi)
    | mini == maxi = [mini]
    | mini < maxi = mini : lisinter (Inter (mini+1) maxi)

contem :: Eq a => a -> [a] -> Bool
contem _ [] = False
contem a (x:xs) = a == x || contem a xs

intercepta :: Eq a => [a] -> [a] -> Bool
intercepta _ [] = False
intercepta [] _ = False
intercepta (x:xs) (y:ys) =  if contem x (y:ys)
                                then True
                                else intercepta xs ys

media :: Final -> Int
media (Inter mini maxi) = (div (mini+maxi) 2)

produto :: Final -> Final -> Final
produto (Inter mini maxi) (Inter mini' maxi') = (Inter (mini*mini') (maxi*maxi'))

redup :: Eq a => [a] -> [a]
redup (x:xs)
    | contem x xs = redup xs 
    | otherwise = x : redup xs

uniao :: [Int] -> [Int] -> [Int]
uniao a b
    | intercepta a b == True = let dup = a ++ b in redup dup
    | otherwise = a ++ b
  
main :: IO ()
main =  do
    putStrLn "Digite o menor numero do intervalo, entao o maior, em seguida se o menor pertence ao intervalo e entao se o maior pertence"
    a <- readLn :: IO Inicial
    b <- readLn :: IO Inicial
    putStrLn "Digite um valor"
    num <- readLn :: IO Int
    let (Inter mini1 maxi1) = processa a
    let v1 = lisinter (Inter mini1 maxi1)
    if contem num v1
        then putStrLn "Contem"
        else putStrLn "Nao contem"
    let (Inter mini2 maxi2) = processa b
    let v2 = lisinter (Inter mini2 maxi2)
    if intercepta v1 v2
        then putStrLn "Intercepta"
        else putStrLn "Nao intercepta"
    putStrLn "Media:"
    putStrLn (show (media (Inter mini1 maxi1)))
    putStrLn "Produto:"
    putStrLn (show (produto (Inter mini1 maxi1) (Inter mini2 maxi2)))
    putStrLn "Uniao:"
    putStrLn (show (uniao v1 v2))
