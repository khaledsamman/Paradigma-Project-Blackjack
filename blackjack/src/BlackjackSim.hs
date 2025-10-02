{-# LANGUAGE OverloadedStrings #-}

module BlackjackSim 
  ( handTotal, isBust, dealerShouldStand, chooseOne, DealerResult(..), dealerResultDist, evStand, evHit, decideHitOrStand, evHitDepth
  ) where

import Cards

handTotal :: Hand -> Int
handTotal = handValue

-- checkt alleen als hand > 21 
isBust :: Hand -> Bool
isBust h = handTotal h > 21

-- dealer stopt bij 17+
dealerShouldStand :: Hand -> Bool
dealerShouldStand h = handTotal h >= 17


-- https://stackoverflow.com/questions/2225774/haskell-pattern-matching-what-is-it
-- https://en.wikibooks.org/wiki/Haskell/Pattern_matching
-- https://wiki.haskell.org/index.php?title=Lazy_pattern_match

-- pak elke kaart een voor een uit de lijst
chooseOne :: [a] -> [(a, [a])]
chooseOne [] = []
chooseOne (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- chooseOne xs ]

-- uitkomst can dealer
data DealerResult
  = DealerBust
  | DealerTotal Int
  deriving (Eq, Ord, Show)

  -- voeg gelijke uitkomsten samen door hun kan op te tellen
combine :: [(DealerResult, Double)] -> [(DealerResult, Double)]
combine = foldr insert []
  where
    insert (k, p) acc = case lookup k acc of
      Nothing -> (k, p) : acc
      Just q -> (k, p + q) : filter ((/= k) . fst) acc

-- kans van dealer
-- stel dealerHand = 16 (bijvoorbeeld 7 en 9) en er zijn nog 2 kaarten in het deck 2 en 10. dealer moet nog een kaart trekken omdat het nog geen 17 is. dit betekent dat we in het recursief geval zitten.
-- chooseOne [2,10] -> (2,[10]) of (10,[2]). elke keuze heeft 1/2 kans. nu komen de branches.
-- brancch 1: als dealer 2 trekt dHand = 18 = stand  [(DealerTotal 18, 1)] met 1/2 wegen =[(DealerTotal 18, 0.5 )] 
-- branch 2: als dealer 10 trekt dHand = 26 = bust [(DealerBust, 1)] met 1/2 wegen =[(DealerBust, 0.5 )] 
-- combineer [(DealerTotal 18, 0.5 ),(DealerBust, 0.5 )] som = 1
-- per kaart is de kans 1 / length dat
-- roep recursief dealerResultDist aan met de nieuwe hand (h ++ [c]) en nieuwe deck

dealerResultDist :: Hand -> Deck -> [(DealerResult, Double)]
dealerResultDist h d
  | isBust h = [(DealerBust, 1.0)] -- dealer is bust 
  | dealerShouldStand h = [(DealerTotal (handTotal h), 1.0)] -- dealer heeft 17+ >21
  | null d = [(DealerTotal (handTotal h), 1.0)] -- geen kaarten meer. deck is leeg
  | otherwise = combine
     [  (r, p / fromIntegral(length d))
      | (c, d') <- chooseOne d
      , (r, p) <- dealerResultDist (h ++ [c]) d' --recursie! :
     ]


-- verwachte waarde als de speler stopt. +1 bij winst, 0 bij push, -1 bij verlies.
evStand :: Hand -> Hand -> Deck -> Double
evStand pHand dHand deck =
  let pTotal = handValue pHand
      dist   = dealerResultDist dHand deck  -- [(DealerResult, Probability)]
      score (DealerBust, prob)              =  1.0 * prob        
      score (DealerTotal dTotal, prob)
        | dTotal <  pTotal                  =  1.0 * prob -- win +1
        | dTotal == pTotal                  =  0.0 * prob -- gelijk = 0
        | otherwise                         = -1.0 * prob -- verlies -1
  in  sum (map score dist)


-- ev als de speler een kaart pakt, met een maximale diepte k. k = 0 betekent: we stoppen met vooruitkijken en doen alsof we stand. anders, voor elke mnogelijke kaart die je kan trekken, bust = -1.
-- als het geen bust is kies beste, nu stoppen of hitten met k -1. neem het gemiddelde van alle mogelijkheden
-- zonder k groeit het enorm. zie verslag voor tabel

evHitDepth :: Int -> Hand -> Hand -> Deck -> Double
evHitDepth 0 pHand dHand deck = evStand pHand dHand deck
evHitDepth k pHand dHand deck =
  let branches = chooseOne deck 
      n        = fromIntegral (length branches) :: Double
      -- ev van een specifieke branch, trek kaart c en beoordeel
      evFor (c, deck') = 
        let p' = pHand ++ [c]
        in  if handValue p' > 21
              then -1.0
              else max (evStand p' dHand deck') -- stand after this hit
                       (evHitDepth (k-1) p' dHand deck') -- or hit again
  in  if null branches
        then evStand pHand dHand deck -- no cards = stand
        else sum (map evFor branches) / n

-- hoever je wil vooruitkijken
evHit :: Hand -> Hand -> Deck -> Double
evHit = evHitDepth 1 -- ev van een extra kaart k=1

-- geef advies en beiden ev terug
decideHitOrStand :: Hand -> Hand -> Deck -> (String, Double, Double)
decideHitOrStand pHand dHand deck =
  let s = evStand pHand dHand deck
      h = evHit   pHand dHand deck
  in if h > s then ("hit", h, s) else ("stand", s, h)