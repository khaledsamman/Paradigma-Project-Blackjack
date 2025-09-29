{-# LANGUAGE OverloadedStrings #-}
module GameFloor where

import Control.Monad (when)
import Control.Monad.State
import Data.Char (toLower)
import Cards
import IOFloor (getPlayerBet, shuffleDeck)
import Control.Monad.IO.Class (MonadIO(liftIO))

import BlackjackAI (runningCount, trueCount)
import BlackjackSim (decideHitOrStand)

-- pretty print a hand without extra imports
showHandSimple :: Hand -> String
showHandSimple []     = "[]"
showHandSimple [c]    = "[" ++ show c ++ "]"
showHandSimple (c:cs) = "[" ++ show c ++ concatMap (\x -> "," ++ show x) cs ++ "]"


showDealerHidden :: Hand -> String
showDealerHidden []       = "[]"
showDealerHidden [c]      = "[" ++ show c ++ "]"
showDealerHidden (c:cs) = "[" ++ show c ++ ",?]"


-- monad transformer stack
type Game = StateT GameState IO

-- initial game state
initialState :: GameState
initialState = GameState
  { player = Player [] 0 1000
  , dealer = Dealer []
  , deck   = multiDeck 1
  }

initialDeckSize :: Int
initialDeckSize = length (deck initialState)

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

-- (kept in case you want the old views elsewhere)
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
  s0 <- get
  let pH0 = playerHand (player s0)
      pv  = handValue pH0
  if pv > 21
    then liftIO $ putStrLn "Player busts! Dealer wins :("
    else do
      s <- get
      let pHand    = playerHand (player s)
          dHand    = dealerHand (dealer s)
          bankroll = playerMoney (player s)   
          bet      = playerBet   (player s)   
          remDeck  = deck s

          seen     = pHand ++ take 1 dHand
          rc       = runningCount seen
          tc       = trueCount seen remDeck
          (adv, evA, evB) = decideHitOrStand pHand (take 1 dHand) remDeck

      liftIO $ do
        putStrLn ""
        putStrLn "=============================="
        putStrLn "         CURRENT HAND         "
        putStrLn "=============================="
        putStrLn $ "Dealer: " ++ showDealerHidden dHand ++ "  (value ?)"
        putStrLn $ "Player: " ++ showHandSimple pHand ++ "  (value " ++ show (handValue pHand) ++ ")"
        putStrLn $ "Bet: " ++ show bet ++ "   Bankroll: " ++ show bankroll
        putStrLn ""
        putStrLn "--- COUNT INFO ---"
        putStrLn $ "Running Count: " ++ show rc ++ "   True Count: " ++ take 5 (show tc)
        putStrLn "--- AI ADVICE ---"
        case adv of
          "hit" ->
            putStrLn $ "HIT   (EV(hit)=" ++ take 5 (show evA) ++ ", EV(stand)=" ++ take 5 (show evB) ++ ")"
          "stand" ->
            putStrLn $ "STAND (EV(stand)=" ++ take 5 (show evA) ++ ", EV(hit)=" ++ take 5 (show evB) ++ ")"
        putStrLn ""

      act <- liftIO $ do
        putStrLn "=============================="
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
            modify (\s -> s { player = payout 0 p })
            showPlayerState

-- one complete round
roundOnce :: Game ()
roundOnce = do
  -- if you want to keep a continuous shoe, reshuffle only when low; otherwise skip
  -- (left as-is: you now keep the same multiDeck across rounds)
  -- modify to clear hands/bets only:
  modify (\s -> s { player = (player s) { playerHand = [], playerBet = 0 }
                  , dealer = (dealer s) { dealerHand = [] } })

  s <- get
  let cardsLeft    = length (deck s)
      fractionLeft = fromIntegral cardsLeft / fromIntegral initialDeckSize
  when (fractionLeft < 0.25) $ do
    liftIO $ putStrLn "Reshuffling the deck..."
    shuffled <- liftIO $ shuffleDeck (multiDeck 1)
    modify (\st -> st { deck = shuffled })

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
  liftIO $ putStrLn "Play another round? (y/n) > "
  ans <- liftIO getLine
  when (map toLower ans == "y") gameLoop

-- runner to start a round from the console
gameMain :: IO ()
gameMain = do
  putStrLn "Welcome to Haskell Blackjack!"
  putStrLn "Shuffling deck..."
  shuffledDeck <- shuffleDeck (deck initialState)
  let initialStateShuffled = initialState { deck = shuffledDeck }
  evalStateT gameLoop initialStateShuffled

