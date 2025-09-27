{-# LANGUAGE OverloadedStrings #-}

module BlackjackSim 
  ( handTotal, isBust, dealerShouldStand, chooseOne, DealerResult(..), dealerResultDist, evStand, evHit, decideHitOrStand, evHitDepth
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
     [  (r, p / fromIntegral(length d))
      | (c, d') <- chooseOne d
      , (r, p) <- dealerResultDist (h ++ [c]) d' --Recursie!!! :)
     ]

     
evStand :: Hand -> Hand -> Deck -> Double
evStand pHand dHand deck =
  let pTotal = handValue pHand
      dist   = dealerResultDist dHand deck  -- [(DealerResult, Probability)]
      score (DealerBust, prob)              =  1.0 * prob        
      score (DealerTotal dTotal, prob)
        | dTotal <  pTotal                  =  1.0 * prob
        | dTotal == pTotal                  =  0.0 * prob
        | otherwise                         = -1.0 * prob
  in  sum (map score dist)

evHitDepth :: Int -> Hand -> Hand -> Deck -> Double
evHitDepth 0 pHand dHand deck = evStand pHand dHand deck
evHitDepth k pHand dHand deck =
  let branches = chooseOne deck 
      n        = fromIntegral (length branches) :: Double
      evFor (c, deck') =
        let p' = pHand ++ [c]
        in  if handValue p' > 21
              then -1.0
              else max (evStand p' dHand deck') -- stand after this hit
                       (evHitDepth (k-1) p' dHand deck') -- or hit again
  in  if null branches
        then evStand pHand dHand deck -- no cards = stand
        else sum (map evFor branches) / n

evHit :: Hand -> Hand -> Deck -> Double
evHit = evHitDepth 1 


decideHitOrStand :: Hand -> Hand -> Deck -> (String, Double, Double)
decideHitOrStand pHand dHand deck =
  let s = evStand pHand dHand deck
      h = evHit   pHand dHand deck
  in if h > s then ("hit", h, s) else ("stand", s, h)