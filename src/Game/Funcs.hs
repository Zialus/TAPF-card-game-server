{-# LANGUAGE RecordWildCards #-}

module Game.Funcs where

import           Control.Monad
import           Control.Monad.ST

import           Data.Array.ST
import           Data.List        (delete, elemIndex, find, insert)
import           Data.Maybe       (fromMaybe)
import           Data.STRef

import           System.Random

import           Game.Data
import           Game.Types

iterateNTimes :: Int -> (a -> a) -> a -> a
iterateNTimes n f x = iterate f x !! n

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

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
        where
            playerHandAfter = take cardAmount deck
            exitDeck = drop cardAmount deck
            exitState = state {gameHand = playerHandAfter}
            exitPlayer = player {state = exitState }

auxToIterate :: (Int, Int, [Player], DeckState) -> (Int,Int,[Player],DeckState)
auxToIterate (cardsAmount, indexOfPlayer, playerList, (deckcards,deckstate)) = (cardsAmount,nextIndex,exitPlayerList,exitDeckState)
        where
            playerToReceiveCards = playerList !! indexOfPlayer -- get the player from the list
            tempPlayerList = delete playerToReceiveCards playerList -- remove the players from the list
            (deckAfter,playerAfter) = giveCardsToPlayer deckcards playerToReceiveCards cardsAmount -- update the player and deck
            exitPlayerList = insert playerAfter tempPlayerList -- put the player back in the list
            exitDeckState = (deckAfter,deckstate)
            nextIndex = indexOfPlayer + 1

giveCardsToPlayers :: DeckState -> [Player] -> Int -> (DeckState,[Player])
giveCardsToPlayers (deckcards,deckstate) playerList amountOfCards = (exitState,exitPlayerList)
        where
            amount = length playerList
            (_,_, exitPlayerList,exitState) = iterateNTimes amount auxToIterate (amountOfCards, 0, playerList, (deckcards,deckstate))


distributeCards :: GameState -> GameState
distributeCards game@GameState{..} = exitState
        where
            howManyCardsTemp = lookup numPlayers amountToDistributeMap
            howManyCards = fromMaybe (error "Invalid number of players") howManyCardsTemp
            shuffledDeck = shuffleDeck deckState
            (deckAfterRemovingCards,playerList) = giveCardsToPlayers shuffledDeck players howManyCards
            exitState = game {deckState = deckAfterRemovingCards, players = playerList}


removePuddings :: Deck -> Int -> Deck
removePuddings deck nPuddings = newDeck
        where
            newDeck = iterateNTimes nPuddings (delete Pudding) deck


findPuddings :: Player -> Int
findPuddings Player{..} = count Pudding (cardsOnTable state)


calculatePuddings :: GameState -> Int
calculatePuddings GameState{..} = amountOfPuddings
        where
            puddingsPerPlayer = map findPuddings players
            amountOfPuddings = sum puddingsPerPlayer

deckForNextRound :: GameState -> DeckState
deckForNextRound gs@GameState{..} = newDeck
        where
            (deck,state) = (allcardsDeck,state) -- put all cards back in the deck
            puddingAmount = calculatePuddings gs -- find out how many pudding cards where played in the previous round
            deckWithLessPudding = removePuddings deck puddingAmount -- remove the pudding cards
            newDeck = shuffleDeck (deckWithLessPudding,state) -- deck is ready to be suffled and sent back for the next round


cleanPlayer :: Player -> Player
cleanPlayer player@Player{..} = newPlayer
        where
            newPlayerState = state {gameHand = []}
            newPlayer = player {state = newPlayerState}

nextRound :: GameState -> GameState
nextRound gs@GameState{..} = nextRoundGameState
        where
            nextRoundN = roundN + 1 -- increase the roundNumber
            nextDeck = deckForNextRound gs -- do the necessary changes on the game deck
            nextPlayers = map cleanPlayer players -- clean Players Game Hands
            nextRoundGameState = gs {roundN = nextRoundN, deckState = nextDeck, players = nextPlayers}


shuffleDeck :: DeckState -> DeckState
shuffleDeck (deck,gen) = shuffle' deck gen


bringChopstickBack :: Player -> Player
bringChopstickBack player@Player{..} = player {state = newState}
        where
            newHand = insert Chopsticks (gameHand state)
            newCardsOnTable = delete Chopsticks (cardsOnTable state)
            newState = state {cardsOnTable = newCardsOnTable, gameHand = newHand}

takeCardFromHand :: [Card] -> Card -> [Card]
takeCardFromHand gameHand card = delete card gameHand


takeCardFromPlayer :: Card -> Player -> (Card,Player)
takeCardFromPlayer card player@Player{..} = (card, player {state = newState})
        where
            newHand = takeCardFromHand (gameHand state) card
            newState = state {gameHand = newHand}

putCardOnTable :: (Card,Player) -> Player
putCardOnTable (card,player@Player{..}) = player {state = newState}
        where
            newCardsOnTable = insert card (cardsOnTable state)
            newState = state {cardsOnTable = newCardsOnTable}


playCardFromPlayer :: Card -> Player -> Player
playCardFromPlayer card player = putCardOnTable $ takeCardFromPlayer card player

applyMoveToPlayer :: Move -> Player -> Player
applyMoveToPlayer (PlayCard card) player                    = playCardFromPlayer card player
applyMoveToPlayer (SpecialMoveChopStick card1 card2) player = bringChopstickBack playerAfter
        where
            playerAfter = applyMoveToPlayer (PlayCard card1) $ applyMoveToPlayer (PlayCard card2) player


applyMoveToGame :: Player -> Move -> GameState -> GameState
applyMoveToGame p mv gs@GameState{..} = newGameState
        where
            thisPlayer = find (==p) players -- get the player from the list
            foundThisPlayer = fromMaybe (error "couldn't find the player in the list") thisPlayer
            tempPlayerList = delete foundThisPlayer players -- remove the player from the list
            newPlayer = applyMoveToPlayer mv foundThisPlayer -- update player by applying the changes generated by game move
            newPlayers = insert newPlayer tempPlayerList  -- put him back in the list
            newGameState = gs {players = newPlayers}


playerHasCardToPlay :: Player -> Card -> Bool
playerHasCardToPlay Player{..} card =
    case search of
        Nothing -> False
        Just _ -> True
    where playerHand = gameHand state
          search = find (==card) playerHand

correctTurn :: Player -> Int -> Bool
correctTurn Player{..} inputTurn = inputTurn == turn state

checkValidMove :: Player -> Move -> GameState -> Bool
-- checkValidMove player move game = coise
checkValidMove = undefined

whoseTurn :: Player -> [Player] -> Player
whoseTurn currentPlayer playerList = nextPlayer
        where
            maybePlayerIndex = elemIndex currentPlayer playerList
            playerIndex = fromMaybe (error "couldn't find the index of the player") maybePlayerIndex
            numberOfPlayers = length playerList
            nextPlayerIndex = (playerIndex + 1) `mod` numberOfPlayers
            nextPlayer = playerList !! nextPlayerIndex
