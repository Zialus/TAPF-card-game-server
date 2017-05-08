{-# LANGUAGE RecordWildCards #-}

module Game.Funcs where

import           Control.Monad
import           Control.Monad.ST

import           Data.Array.ST
import           Data.List        (delete, elemIndex, find, insert)
import           Data.Maybe
import           Data.STRef

import           System.Random

import           Game.Data
import           Game.Types

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray' n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
        where
            n = length xs
            newArray' :: Int -> [a] -> ST s (STArray s Int a)
            newArray' n' =  newListArray (1,n')


giveCardsToPlayer :: Deck -> Player -> Int -> (Deck,Player)
giveCardsToPlayer deck player@Player{..} cardAmount = (exitDeck,exitPlayer)
                                where playerHandAfter = take cardAmount deck
                                      exitDeck = drop cardAmount deck
                                      exitState = state {gameHand = playerHandAfter}
                                      exitPlayer = player {state = exitState }

giveCardsToPlayers :: DeckState -> [Player] -> Int -> (DeckState,[Player])
giveCardsToPlayers (deckcards,deckstate) playerList amountOfCards = (exitState,exitPlayerList)
                            where amount = length playerList
                                  (exitState, exitPlayerList) = ( (deckcards,deckstate), playerList )    -- iterateNTimes amount giveCardsToPlayer amountOfCards
                                  result = giveCardsToPlayer deckcards (head playerList) amountOfCards

distributeCards :: GameState -> GameState
distributeCards game@GameState{..} = exitState
                    where howManyCardsTemp = lookup numPlayers amountToDistributeMap
                          howManyCards = fromMaybe (error "Invalid number of players") howManyCardsTemp
                          shuffledDeck = shuffleDeck deckState
                          (deckAfterRemovingCards,playerList) = giveCardsToPlayers shuffledDeck players howManyCards
                          exitState = game {deckState = deckAfterRemovingCards, players = playerList}

amountToDistributeMap :: [(Int,Int)]
amountToDistributeMap = [(2,10),(3,9),(4,8),(5,7)]

-- removes from a deck the number of puddings played in a round
removePuddings :: Deck -> Int -> Deck
removePuddings deck nPuddings = newDeck
                where newDeck = iterateNTimes nPuddings (delete Pudding) deck

calculatePuddings :: GameState -> Int
calculatePuddings = undefined

deckForNextRound :: GameState -> DeckState
deckForNextRound gs@GameState{..} = newDeck
                            where (deck,state) = deckState
                                  puddingAmount = calculatePuddings gs
                                  deckWithLessPudding = removePuddings deck puddingAmount
                                  newDeck = shuffleDeck (deckWithLessPudding,state)

nextRound :: GameState -> GameState
nextRound gs = nextRoundGameState
        where nextRoundN = roundN gs + 1
              nextDeck = deckForNextRound gs
              nextRoundGameState = gs {roundN = nextRoundN, deckState = nextDeck}

iterateNTimes :: Int -> (a -> a) -> a -> a
iterateNTimes n f x = iterate f x !! n

shuffleDeck :: DeckState -> DeckState
shuffleDeck (deck,gen) = shuffle' deck gen

takeCardFromPlayer :: Card -> Player -> Player
takeCardFromPlayer card player@Player{..} = player {state = newState}
    where
        newHand = takeCardFromHand (gameHand state) card
        newState = state {gameHand = newHand}

takeCardFromHand :: [Card] -> Card -> [Card]
takeCardFromHand gameHand card = delete card gameHand

checkValidMove :: Player -> Move -> GameState -> Bool
checkValidMove player move game = undefined

applyMoveToPlayer :: Move -> Player -> Player
applyMoveToPlayer (PlayCard card) player                    = takeCardFromPlayer card player

applyMoveToPlayer (SpecialMoveChopStick card1 card2) player = bringChopstickBack playerAfter
                                            where playerAfter = applyMoveToPlayer (PlayCard card1)
                                                              $ applyMoveToPlayer (PlayCard card2) player

bringChopstickBack :: Player -> Player
bringChopstickBack Player{..} = Player {pid = pid, state = newState}
                    where
                        newHand = insert Chopsticks (gameHand state)
                        newCardsOnTable = delete Chopsticks (cardsOnTable state)
                        newState = state {cardsOnTable = newCardsOnTable, gameHand = newHand}


whoseTurn :: Player -> [Player] -> Player
whoseTurn currentPlayer playerList = nextPlayer
    where
        maybePlayerIndex = elemIndex currentPlayer playerList
        playerIndex = fromMaybe (error "couldn't find the index of the player") maybePlayerIndex
        numberOfPlayers = length playerList
        nextPlayerIndex = (playerIndex + 1) `mod` numberOfPlayers
        nextPlayer = playerList !! nextPlayerIndex



applyMoveToGame :: Player -> Move -> GameState -> GameState
applyMoveToGame p mv gs@GameState{..} = newGameState
    where
        thisPlayer = find (==p) players-- get the player from the list
        foundThisPlayer = fromMaybe (error "couldn't find the player in the list") thisPlayer
        tempPlayerList = delete foundThisPlayer players -- remove the player from the list
        newPlayer = applyMoveToPlayer mv foundThisPlayer -- update player by applying the changes generated by game move
        newPlayers = insert newPlayer tempPlayerList  -- put him back in the list
        newGameState = gs {players = newPlayers}
