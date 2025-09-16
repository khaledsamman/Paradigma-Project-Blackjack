{-# LANGUAGE OverloadedStrings #-}

module GameFloor where

import Control.Monad.State
import Cards
import IOFloor (getPlayerBet)


--monad transformer stack
type Game = StateT GameState IO


--initial game state
initialState :: GameState
initialState = GameState {
    player = Player [] 0 1000,
    dealer = Dealer [],
    deck = fullDeck
}


--computation in game

playerBetAction :: Game ()
playerBetAction = do
  bet <- liftIO getPlayerBet
  p0  <- gets player
  let p1 = makeBet bet p0
  modify (\s -> s { player = p1 })