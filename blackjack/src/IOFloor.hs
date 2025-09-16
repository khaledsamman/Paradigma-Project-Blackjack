{-# LANGUAGE OverloadedStrings #-}


module IOFloor where

import System.Random
import Data.List (sortOn)
import Cards

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    gen <- newStdGen
    return $ map snd $ sortOn fst $ zip (randoms gen :: [Int]) deck

