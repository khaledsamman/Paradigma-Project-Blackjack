{-# LANGUAGE OverloadedStrings #-}

module BlackjackSim 
  ( handTotal, isBust, dealerShouldStand, chooseOne, DealerResult(..), dealerResultDist
  ) where

import Cards

handTotal :: Hand -> Int
handTotal = handValue

-- Purely check if a hand is bust
isBust :: Hand -> Bool
isBust h = handTotal h > 21

-- Dealer stands on 17+ ( handValue already handles Aces)
dealerShouldStand :: Hand -> Bool
dealerShouldStand h = handTotal h >= 17


-- https://stackoverflow.com/questions/2225774/haskell-pattern-matching-what-is-it
-- https://en.wikibooks.org/wiki/Haskell/Pattern_matching
-- https://wiki.haskell.org/index.php?title=Lazy_pattern_match

chooseOne :: [a] -> [(a, [a])]
chooseOne [] = []
chooseOne (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- chooseOne xs ]


data DealerResult
  = DealerBust
  | DealerTotal Int
  deriving (Eq, Ord, Show)

  -- combine identical outcomes by summing their probabilities
combine :: [(DealerResult, Double)] -> [(DealerResult, Double)]
combine = foldr insert []
  where
    insert (k, p) acc = case lookup k acc of
      Nothing -> (k, p) : acc
      Just q -> (k, p + q) : filter ((/= k) . fst) acc

dealerResultDist :: Hand -> Deck -> [(DealerResult, Double)]
dealerResultDist h d
  | isBust h = [(DealerBust, 1.0)]
  | dealerShouldStand h = [(DealerTotal (handTotal h), 1.0)]
  | null d = [(DealerTotal (handTotal h), 1.0)] -- No more cards to draw
  | otherwise = combine
     [  (r, p * fromIntegral(length d))
      | (c, d') <- chooseOne d
      , (r, p) <- dealerResultDist (h ++ [c]) d' --Recursie!!! :)
     ]
