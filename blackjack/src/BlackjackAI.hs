{-# LANGUAGE OverloadedStrings #-}

module BlackjackAI 
    (hiLoCard, runningCount, remainingDecks, trueCount) where

import Cards 


--https://www.blackjackapprenticeship.com/how-to-count-cards/


-- hiLoCard geeft elke kaart een waarde volgens het Hi-Lo systeem:
-- Lage kaarten (2 t/m 6) = +1
-- Middel kaarten (7,8,9) = 0
-- Hoge kaarten (10, J, Q, K, A) = -1

hiLoCard :: Card -> Int
hiLoCard (Card v _) = case v of
    Two   -> 1
    Three -> 1
    Four  -> 1
    Five  -> 1
    Six   -> 1
    Seven -> 0
    Eight -> 0
    Nine  -> 0
    Ten   -> -1
    Jack  -> -1
    Queen -> -1
    King  -> -1
    Ace   -> -1



-- telt alle hi lo waardes van de gespeelde kaarten bij elkaar op. dus hoe meer hoge kaarten al weg zijn, hoe hoger de count.
runningCount :: [Card] -> Int
runningCount = sum . map hiLoCard


-- berekent hoeveel decks er nog in het spel zitten. een deck heeft 52 kaarten, dus length deck / 52.
remainingDecks :: Deck -> Double
remainingDecks d =
  let rc = max 1 (length d) -- zorgen dat je niet door 0 deelt
  in fromIntegral rc / 52.0

-- trueCount is de echte telling: runningCount gedeeld door aantal resterende decks.

trueCount :: [Card] -> Deck -> Double
trueCount seen d = fromIntegral (runningCount seen) / remainingDecks d

-- als de true count hoog genoeg is (>= 2), heb je voordeel en zet je hoog in.
shouldBetHigh :: [Card] -> Deck -> Bool
shouldBetHigh seen d = trueCount seen d >= 2.0

-- anders zet je laag in
shouldBetLow :: [Card] -> Deck -> Bool
shouldBetLow seen d = trueCount seen d < 2.0

-- hoog = 100 | laag = 10
shouldBet :: [Card] -> Deck -> Int
shouldBet seen d = if shouldBetHigh seen d then 100 else 10
