import Data.Char
import System.IO

--fjerner elementer fra listen som er lik det gitte argumentet
remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove x (y : ys)
  | x == y = remove x ys
  | otherwise = y : remove x ys

-- Padd with spaces
paddSpaces :: Int -> Int -> String
paddSpaces n a = concat (replicate (n - a) " ")

-- ringer representert som liste
ringLst :: Int -> [Int] -> [String]
ringLst _ [] = []
ringLst n (x : xs) = (paddSpaces n x ++ concat (replicate x "* ") ++ paddSpaces n x) : ringLst n xs

--Tegner bretter i terminalen
drawBoard :: [[Int]] -> IO ()
drawBoard xs =
  do
    -- Finne makshøyden
    let height = head (xs !! 3) + 1
        tårnA = xs !! 0
        tårnB = xs !! 1
        tårnC = xs !! 2
    -- Lage ringer i en liste som er fylt ut med tomme rader til og med maks
    let t1 = paddVertical height (if isEmpty tårnA then ringLst height [] else ringLst height (remove 0 tårnA))
    let t2 = paddVertical height (if isEmpty tårnB then ringLst height [] else ringLst height (remove 0 tårnB))
    let t3 = paddVertical height (if isEmpty tårnC then ringLst height [] else ringLst height (remove 0 tårnC))

    -- Sette sammen alle trærne til en stor liste
    let t = [t1 !! row ++ t2 !! row ++ t3 !! row | row <- [0 .. (height -1)]]

    -- Printe ut hovedlisten linje for linje
    mapM_ putStrLn t

-- tegner stolper vertical der det ikke er ringer
paddVertical :: Int -> [String] -> [String]
paddVertical max ringer = rows ++ ringer
  where
    n = max - length ringer
    rowSize = length ringer
    rows = replicate n (paddSpaces max 1 ++ "| " ++ paddSpaces max 1)

-- Sjekker om stolpen er tom. Dvs om listen er fylt med 0-er
isEmpty :: [Int] -> Bool
isEmpty [] = False
isEmpty xs = foldr (\x -> (&&) (x == 0)) True xs

-- Spillet er ferdig når de to første stolpene er tomme
finished :: [[Int]] -> Bool
finished xs = isEmpty (head xs) && isEmpty (xs !! 1)

--takes a board and a pole to move from and to
-- gives true if it is a valid move, else false
valid :: [[Int]] -> Int -> Int -> Bool
valid board from to
  --Kan ikke flytte fra en tom stålpe
  | isEmpty (board !! from) = False
  -- det er lovt å flytte til en tom stolpe
  | isEmpty (board !! to) = True
  --ringen du prøver å flytte er større en den som ligger der du flytter til og stolpene må være lovlige--
  | validRings board from to && validStolpe from to = True
  -- Ellers er det lovelig
  | otherwise = False

validRings :: [[Int]] -> Int -> Int -> Bool
validRings board from to
  | øversteRing fromStolpe n > øversteRing toStolpe n = False
  | otherwise = True
  where
    n = head (board !! 3) -- antall ringer i spillet
    fromStolpe = board !! from -- Stolpen vi flytter fra
    toStolpe = board !! to -- stolpen vi flytter til

-- sjekker om stolpene brukeren har oppgit er lovlige stolper--
validStolpe :: Int -> Int -> Bool
validStolpe from to
  | from == to = False
  | from `notElem` [0, 1, 2] = False
  | to `notElem` [0, 1, 2] = False
  | otherwise = True

--setter bytter elementet på en gitt posisjon med et anet element
swap :: [Int] -> Int -> Int -> [Int]
swap xs i num = pre ++ [num] ++ tail rest
  where
    (pre, rest) = splitAt i xs

-- Setter inn i på første forekomst av 0
swapZero :: [Int] -> Int -> [Int]
swapZero [] i = []
swapZero (x : xs) i = if x /= 0 then x : swapZero xs i else i : xs

-- Gir brettet etter et trekk er gjort
move :: [[Int]] -> Int -> Int -> [[Int]]
move board from to
  --finner rekkefølgen av de oppdaterte stolpene/tårnene
  | from == 0 && to == 1 = [tårnA, tårnB, tårnC, board !! 3]
  | from == 0 && to == 2 = [tårnA, tårnC, tårnB, board !! 3]
  | from == 1 && to == 0 = [tårnB, tårnA, tårnC, board !! 3]
  | from == 1 && to == 2 = [tårnC, tårnA, tårnB, board !! 3]
  | from == 2 && to == 0 = [tårnB, tårnC, tårnA, board !! 3]
  | from == 2 && to == 1 = [tårnC, tårnB, tårnA, board !! 3]
  where
    fromStolpe = board !! from -- Listen vi skal flytte fra
    toStolpe = board !! to -- Listen vi skal flytte til
    n = head (board !! 3) -- antall ringer i spill

    -- Indexen til den øverste ringen på stolpen på plass from
    fromIndex = indexØversteRing fromStolpe 0 n
    -- Indexen til den øveste ringen på stolpen på plass to, er stolpen tom er det siste element i listen (nederst på stolpen) som gjelder
    toIndex = indexØversteRing toStolpe 0 n
    -- Ringen som ligger på stolpen from
    fromRing = øversteRing (board !! from) n

    -- Tårnet etter øverste ringen fra orginal stolpen (from) er fjernet
    tårnA = swap fromStolpe fromIndex 0
    -- Tårnet etter at ringen fra orginal stolpen er lagt på ny stolpen (to)
    tårnB = reverse (swapZero (reverse toStolpe) fromRing)
    -- Det resterende tåret
    tårnC = board !! restStolpe from to

--Finner stolpen hvor ingen ringer blir flyttet på
restStolpe :: Int -> Int -> Int
restStolpe from to
  | from + to == 1 = 2
  | from + to == 2 = 1
  | otherwise = 0

-- tar in et tårn, en index counter, antall disker i spill
-- Finner den indezen på den øverste ringen, er stilpen tom er det den bakerste indexen
indexØversteRing :: [Int] -> Int -> Int -> Int
indexØversteRing xs y n
  | isEmpty xs = length xs
  | head xs == 0 && y <= n -1 = indexØversteRing (tail xs) (y + 1) n
  | otherwise = y

-- tar inn et tårn og antall disker i spill
-- gir ringen (størrelsen)
øversteRing :: [Int] -> Int -> Int
øversteRing [] _ = 0
øversteRing (x : xs) n
  | isEmpty xs = 0
  | x == 0 = øversteRing xs n
  | otherwise = x

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  s <- getLine
  newline
  if not (null s) && all isDigit s
    then return (read s)
    else do
      putStrLn "ERROR: Invalid number"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

undoLog :: [[[Int]]] -> Int -> [[[Int]]]
undoLog log z
  | z > length log = reverse $ drop ((length log) -1) (reverse log)
  | otherwise = reverse (drop z (reverse log))

clearTerminal :: IO ()
clearTerminal = putStr "\ESC[2J"

-- Lager startbrettet hvor de 3 første lisene er stolpene og den sise listen holder på antall ringer i spillet
initialBoard :: Int -> [[Int]]
initialBoard n = [[1 .. n], replicate n 0, replicate n 0, [n]]

{-
-- et testboar
testBoard ::  [[Int]]
testBoard = [[0,0,3,4],[0,0,0,2],[0,0,0,1],[4]]
-}

-- lager et nytt spill med initialBoard, legger dette bretter til listen av brett, moves tellern til 0, og det er ingen beskjed og gi enda.
newGame :: Int -> IO ()
newGame n = play (initialBoard n) [initialBoard n] 0 ""

main = do
  putStrLn "Start a game with: b <numOfRings>, or quit with q"
  input <- getLine
  case words input of
    "b" : [n] ->
      if foldr (\x y -> y && isDigit x) True n
        then newGame (read n :: Int)
        else do
          clearTerminal
          putStrLn $ "ERROR:  '" ++ n ++ "' is Not a number"
          main
    ["q"] -> putStrLn "Thanks for Playing !"
    _ -> do
      clearTerminal
      putStrLn "Unrecognized command"
      main

play :: [[Int]] -> [[[Int]]] -> Int -> String -> IO () -- ett brett, liste med brett, en moves teller, beskjed
play board log mvs msg = do
  newline
  clearTerminal
  drawBoard board
  if finished board
    then do
      newline
      putStrLn " !! You win !!"
    else do
      newline
      putStrLn msg
      putStrLn ("Number of moves: " ++ show mvs)
      putStrLn "Make your move, exapmle 1 3 : "
      inn <- getLine
      case words inn of
        "b" : [n] -> if foldr (\x y -> y && isDigit x) True n then newGame (read n :: Int) else play board log mvs "ERROR: Not a number"
        "z" : [n] -> do
          play (last newLog) newLog (if z > length newLog then 0 else mvs - z) ""
          where
            newLog = undoLog log (z -1)
            z = (read n :: Int)
        f : t : _ ->
          if all isDigit f && all isDigit t && validStolpe from to
            then
              ( if valid board from to
                  then play (move board from to) (log ++ [board]) (mvs + 1) ""
                  else do
                    newline
                    play board log mvs "ERROR: Invalid move"
              )
            else
              ( do
                  play board log mvs "Move must be integers between 1 and 3, and not the same"
              )
          where
            from = (read f :: Int) -1
            to = (read t :: Int) -1
        ["q"] -> putStrLn "Thanks for Playing !"
        _ -> do
          play board log mvs "Unrecognized command"
