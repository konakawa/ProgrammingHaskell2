{-# LANGUAGE CPP #-}
#include "base.hs"

import System.Random

bestmoveRandom :: Grid -> Player -> IO Grid
bestmoveRandom g p = do i <- randomRIO (0, length bestMoves - 1)
                        return (bestMoves !! i)
                        where
                          tree = prune depth (gametree g p)
                          Node (_, best) ts = minimax tree
                          bestMoves = [g' | Node (g', p') _ <- ts, p' == best]