import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

winCount :: Int
winCount = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any (hasWinCount p) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

hasWinCount :: Player -> [Player] -> Bool
hasWinCount p xs = any (all (== p)) (windows winCount xs)

windows :: Int -> [a] -> [[a]]
windows n xs | length xs < n = []
             | otherwise     = take n xs : windows n (tail xs)

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1, 1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g   = putStrLn "It's a draw!\n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                   [] -> do putStrLn "ERROR: Invalid move"
                            run' g p
                   [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g     = []
  | full g    = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player, Int)
minimax (Node g [])
  | wins O g  = Node (g, O, 0) []
  | wins X g  = Node (g, X, 0) []
  | otherwise = Node (g, B, maxBound) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps, minDepth) ts'
  | turn g == X = Node (g, maximum ps, minDepth) ts'
  where
    ts' = map minimax ts
    ps  = [p | Node (_, p, _) _ <- ts']
    ds  = [d | Node (_, p, d) _ <- ts', p == turn g]
    minDepth = if null ds then maxBound else minimum ds + 1

bestmove :: Tree (Grid, Player, Int) -> Grid
bestmove tree = head [g' | Node (g', p', d') _ <- ts, p' == best, d' == bestDepth]
               where
                  Node (_, best, _) ts = tree
                  bestDepth = minimum [d | Node (_, p', d) _ <- ts, p' == best]

evaluate :: Grid -> Player
evaluate g
  | wins O g  = O
  | wins X g  = X
  | otherwise = B

alphabeta :: Grid -> Player -> Int -> Player -> Player -> (Player, Int)
alphabeta g p d alpha beta
  | d == 0 || won g || full g = (evaluate g, 0)
  | p == O = let (result, depth) = alphabetaMin (moves g p) (next p) (d-1) alpha beta maxBound
             in (result, depth + 1)
  | p == X = let (result, depth) = alphabetaMax (moves g p) (next p) (d-1) alpha beta maxBound
             in (result, depth + 1)

alphabetaMin :: [Grid] -> Player -> Int -> Player -> Player -> Int -> (Player, Int)
alphabetaMin [] _ _ _ _ bestDepth = (X, bestDepth)
alphabetaMin (g:gs) p d alpha beta bestDepth
  | alpha >= beta = (beta, bestDepth)
  | otherwise = let (val, depth) = alphabeta g p d alpha beta
                in if val >= beta then (beta, bestDepth)
                   else let newAlpha = if val < alpha then val else alpha
                            (restVal, restDepth) = alphabetaMin gs p d newAlpha beta (min bestDepth depth)
                        in if val < restVal then (val, depth)
                           else if val == restVal && depth < restDepth then (val, depth)
                           else (restVal, restDepth)

alphabetaMax :: [Grid] -> Player -> Int -> Player -> Player -> Int -> (Player, Int)
alphabetaMax [] _ _ _ _ bestDepth = (O, bestDepth)
alphabetaMax (g:gs) p d alpha beta bestDepth
  | alpha >= beta = (alpha, bestDepth)
  | otherwise = let (val, depth) = alphabeta g p d alpha beta
                in if val <= alpha then (alpha, bestDepth)
                   else let newBeta = if val > beta then val else beta
                            (restVal, restDepth) = alphabetaMax gs p d alpha newBeta (min bestDepth depth)
                        in if val > restVal then (val, depth)
                           else if val == restVal && depth < restDepth then (val, depth)
                           else (restVal, restDepth)

bestmoveAlphaBeta :: Grid -> Player -> Grid
bestmoveAlphaBeta g p = head [g' | (g', val, d) <- candidates, val == best, d == bestDepth]
  where
    candidates = [(g', val, d) | g' <- moves g p, let (val, d) = alphabeta g' (next p) (depth-1) O X]
    (best, bestDepth) = if p == X then maximum [(val, d) | (_, val, d) <- candidates]
                                  else minimum [(val, d) | (_, val, d) <- candidates]

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          startPlayer <- chooseSide
          play empty startPlayer

chooseSide :: IO Player
chooseSide = do putStrLn "Choose your side:"
                putStrLn "1. First player (O)"
                putStrLn "2. Second player (X)"
                choice <- getNat "Enter 1 or 2: "
                if choice == 1 then return O
                else if choice == 2 then return X
                else do putStrLn "ERROR: Please enter 1 or 2"
                        chooseSide

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play' g p
                    [g'] -> play g' (next p)
  | p == X   = do putStr "Player X is thinking... "
                  (play $! (bestmove tree)) (next p)
                  where tree = minimax (prune depth (gametree g p))

playAlphaBeta :: Grid -> Player -> IO ()
playAlphaBeta g p = do cls
                       goto (1, 1)
                       putGrid g
                       playAlphaBeta' g p

playAlphaBeta' :: Grid -> Player -> IO ()
playAlphaBeta' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             playAlphaBeta' g p
                    [g'] -> playAlphaBeta g' (next p)
  | p == X   = do putStr "Player X is thinking... "
                  (playAlphaBeta $! (bestmoveAlphaBeta g p)) (next p)