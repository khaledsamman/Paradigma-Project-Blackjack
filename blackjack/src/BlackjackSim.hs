{-# LANGUAGE OverloadedStrings #-}

module BlackjackSim 
  ( handTotal
  , isBust
  , dealerShouldStand
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
