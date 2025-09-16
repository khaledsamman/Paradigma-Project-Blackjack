{-# LANGUAGE OverloadedStrings #-}
module GameFloor where

import Control.Monad (when)
import Control.Monad.State
import Data.Char (toLower)
import Cards
import IOFloor (getPlayerBet, shuffleDeck)
import Control.Monad.IO.Class (MonadIO(liftIO))

-- monad transformer stack
type Game = StateT GameState IO

-- initial game state
initialState :: GameState
initialState = GameState
  { player = Player [] 0 1000
  , dealer = Dealer []
  , deck   = fullDeck
  }

-- prompt the user for a bet and update state
playerBetAction :: Game ()
playerBetAction = do
  bet <- liftIO getPlayerBet
  p0  <- gets player
  let p1 = makeBet bet p0
  modify (\s -> s { player = p1 })

-- deal to player
dealCardsToPlayer :: Int -> Game ()
dealCardsToPlayer n = do
  s <- get
  let (newDeck, drawn) = dealCards n (deck s)
      p' = (player s) { playerHand = playerHand (player s) ++ drawn }
  put s { deck = newDeck, player = p' }

-- deal to dealer
dealCardsToDealer :: Int -> Game ()
dealCardsToDealer n = do
  s <- get
  let (newDeck, drawn) = dealCards n (deck s)
      d' = (dealer s) { dealerHand = dealerHand (dealer s) ++ drawn }
  put s { deck = newDeck, dealer = d' }

-- show helpers
showPlayerState :: Game ()
showPlayerState = do
  p <- gets player
  let v = handValue (playerHand p)
  liftIO $ putStrLn $ "Player: " ++ show (playerHand p) ++ "  (value " ++ show v ++ ")"
  liftIO $ putStrLn $ "Bet: " ++ show (playerBet p) ++ "   Money: " ++ show (playerMoney p)

showDealerHand :: Game ()
showDealerHand = do
  d <- gets dealer
  let v = handValue (dealerHand d)
  liftIO $ putStrLn $ "Dealer: " ++ show (dealerHand d) ++ "  (value " ++ show v ++ ")"

-- player's turn: loop until stand or bust
playerAction :: Game ()
playerAction = do
  showDealerHand
  showPlayerState
  p <- gets player
  let pv = handValue (playerHand p)
  if pv > 21
    then liftIO $ putStrLn "Player busts! Dealer wins :("
    else do
      act <- liftIO $ do
        putStr "Choose action: (h)it or (s)tand > "
        getLine
      case map toLower act of
        "h" -> dealCardsToPlayer 1 >> playerAction
        "s" -> liftIO $ putStrLn "Player stands."
        _   -> liftIO (putStrLn "Invalid input.") >> playerAction

-- dealer AI: hit until 17+, then settle outcome vs player's value
dealerAction :: Int -> Game ()
dealerAction playerValue = do
  d <- gets dealer
  let dv = handValue (dealerHand d)
  if dv < 17
    then do
      dealCardsToDealer 1
      showDealerHand
      dealerAction playerValue
    else do
      showDealerHand
      liftIO $ putStrLn $ "Dealer stands with value: " ++ show dv
      if dv > 21 || dv < playerValue
        then do
          liftIO $ putStrLn "Player wins!"
          p <- gets player
          -- simple 1:1 payout (win back bet + same amount). Change to *3 if you wish.
          let winAmt = playerBet p * 2
          modify (\s -> s { player = payout winAmt p })
          showPlayerState
        else if dv == playerValue
          then do
            liftIO $ putStrLn "Push (tie). Bet returned."
            p <- gets player
            modify (\s -> s { player = payout (playerBet p) p })
            showPlayerState
          else do
            liftIO $ putStrLn "Dealer wins."
            p <- gets player
            -- player already paid bet when placing it; reset bet to 0
            modify (\s -> s { player = payout 0 p })
            showPlayerState

-- one complete round
roundOnce :: Game ()
roundOnce = do
  -- shuffle fresh deck
  d0 <- liftIO (shuffleDeck fullDeck)
  modify (\s -> s { deck = d0, player = (player s) { playerHand = [], playerBet = 0 }
                  , dealer = (dealer s) { dealerHand = [] } })

  playerBetAction
  dealCardsToPlayer 2
  dealCardsToDealer 2

  -- player's turn
  playerAction

  -- if player didn't bust, let dealer play and settle
  p <- gets player
  let pv = handValue (playerHand p)
  when (pv <= 21) (dealerAction pv)

-- loop after each round until player quits

gameLoop :: Game ()
gameLoop = do
    roundOnce
    liftIO $ putStr "Play another round? (y/n) > "
    ans <- liftIO getLine
    when (map toLower ans == "y") gameLoop


-- runner to start a round from the console
gameMain :: IO ()
gameMain = evalStateT gameLoop initialState
