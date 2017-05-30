{-# LANGUAGE RecordWildCards #-}

module Game.Funcs where

import           Data.List  (delete, elemIndex, find, insert)
import           Data.Maybe (fromMaybe)

import           Game.Data
import           Game.Types
import           Lib


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


-- auxGiveCards :: Int -> [Player] -> [Deck] -> Deck -> ([Player],Deck)
-- auxGiveCards cardsAmount playerList dividedCards deckcards = (exitPlayerList,exitDeckState)
--         where
--           num_players = length playerList
--           exitDeckState = drop (cardsAmount * num_players) deckcards
--           exitPlayerList = playerList ----------- WRONG!!!!!!
--
-- giveCardsToPlayers :: DeckState -> [Player] -> Int -> (DeckState,[Player])
-- giveCardsToPlayers (deckcards,deckstate) playerList amountOfCards = (exitState,exitPlayerList)
--         where
--             dividedDeck = splitEvery amountOfCards deckcards
--             (exitPlayerList,deckAfter) = auxGiveCards amountOfCards playerList dividedDeck deckcards
--             exitState = (deckAfter,deckstate)

distributeCards :: GameState -> GameState
distributeCards game@GameState{..} = exitState
        where
            howManyCardsTemp = lookup numPlayers amountToDistributeMap
            howManyCards = fromMaybe (error "Invalid number of players") howManyCardsTemp
            shuffledDeck = shuffleDeck deckState
            (deckAfterRemovingCards,playerList) = giveCardsToPlayers shuffledDeck players howManyCards
            exitState = game {deckState = deckAfterRemovingCards, players = playerList}


startGame :: GameState -> GameState
startGame = distributeCards


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
            nextRoundN = roundG + 1 -- increase the roundNumber
            nextDeck = deckForNextRound gs -- do the necessary changes on the game deck
            nextPlayers = map cleanPlayer players -- clean Players Game Hands
            nextRoundGameState = gs {roundG = nextRoundN, deckState = nextDeck, players = nextPlayers}


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


-- check if the player has a card you are expecting
playerHasCardToPlay :: Player -> Card -> Bool
playerHasCardToPlay Player{..} card =
    case search of
        Nothing -> False
        Just _  -> True
    where playerHand = gameHand state
          search = find (==card) playerHand


tableHasChopSticks :: Deck -> Bool
tableHasChopSticks = elem Chopsticks

-- check if Player Turn is the one you are expecting
correctTurn :: Player -> Int -> Bool
correctTurn Player{..} inputTurn = inputTurn == turnP state


correctRound :: Player -> Int -> Bool
correctRound Player{..} inputRound = inputRound == roundP state


-- check if a play is valid by checking if a move is valid, and if the round/turn number is valid
checkValidPlay :: Move -> Player -> GameState -> Bool
checkValidPlay move player game = condition1 && condition2 && condition3
    where
      condition1 = checkValidMove move player
      condition2 = correctRound player $ roundG game
      condition3 = correctTurn player $ turnG game


-- check if a move is valid by calling the correct move-checker depending on move type
checkValidMove :: Move -> Player -> Bool
checkValidMove move player =
    case move of
        SpecialMoveChopStick card1 card2 -> checkSpecialMoveValidity card1 card2 player
        PlayCard card -> checkRegularMoveValidy card player


checkSpecialMoveValidity :: Card -> Card -> Player -> Bool
checkSpecialMoveValidity card1 card2 player = condition1 && condition2 && condition3
    where
      condition1 = playerHasCardToPlay player card1
      condition2 = playerHasCardToPlay player card2
      condition3 = tableHasChopSticks $ cardsOnTable $ state player

checkRegularMoveValidy ::  Card -> Player -> Bool
checkRegularMoveValidy card player = condition
    where
      condition = playerHasCardToPlay player card


whoGetsNextTurn :: Player -> [Player] -> Player
whoGetsNextTurn currentPlayer playerList = nextPlayer
        where
            maybePlayerIndex = elemIndex currentPlayer playerList
            playerIndex = fromMaybe (error "couldn't find the index of the player") maybePlayerIndex
            numberOfPlayers = length playerList
            nextPlayerIndex = (playerIndex + 1) `mod` numberOfPlayers
            nextPlayer = playerList !! nextPlayerIndex


calculateScore :: GameState -> GameState
calculateScore = undefined


howManyCardsOnThisPlayerHand :: Player -> Int
howManyCardsOnThisPlayerHand player = length $ gameHand $ state player
