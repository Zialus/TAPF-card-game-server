{-# LANGUAGE RecordWildCards #-}

module Game.Funcs where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List        (delete, elemIndex, find, insert)
import           Data.Maybe
import           Data.STRef
import           Game.Types
import           System.Random

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
                                where currentPlayerHand = gameHand state
                                      playerHandAfter = take cardAmount deck
                                      exitDeck = drop cardAmount deck
                                    --   updateState _state = _state {gameHand = playerHandAfter}
                                    --   exitState = updateState state
                                      exitState = state {gameHand = playerHandAfter}
                                      exitPlayer = player {state = exitState }

giveCardsToPlayers :: DeckState -> [Player] -> Int -> (DeckState,[Player])
giveCardsToPlayers (deckcards,deckstate) playerList amountOfCards = (exitState,exitPlayerList)
                            where amount = length playerList
                                  (exitState, exitPlayerList) = ( (deckcards,deckstate), playerList )    -- iterateNTimes amount giveCardsToPlayer amountOfCards
                                  result = giveCardsToPlayer deckcards (head playerList) amountOfCards

distributeCards :: GameState -> GameState
distributeCards game@GameState{..} = exitState
                    where howManyCards = lookup numPlayers amountToDistributeMap
                          shuffledDeck = shuffleDeck deckState
                          (deckAfterRemovingCards,playerList) = giveCardsToPlayers deckState players (fromMaybe (error "Invalid number of players") howManyCards)
                        --   updateState _state = _state {deckState = deckAfterRemovingCards}
                        --   exitState = updateState game
                          exitState = game {deckState = deckAfterRemovingCards, players = playerList}

amountToDistributeMap :: [(Int,Int)]
amountToDistributeMap = [(2,10),(3,9),(4,8),(5,7)]

-- removes from a deck the number of puddings played in a round
deckForNextRound :: DeckState -> Int -> DeckState
deckForNextRound (deck,state) nPuddings = newDeckShuffled
                where newDeck = iterateNTimes nPuddings (delete Pudding) deck
                      newDeckShuffled = shuffleDeck (newDeck,state)

iterateNTimes :: Int -> (a -> a) -> a -> a
iterateNTimes n f x = iterate f x !! n

shuffleDeck :: DeckState -> DeckState
shuffleDeck (deck,gen) = shuffle' deck gen


allcardsDeck :: [Card]
allcardsDeck =     replicate 14  Tempura  ++
                   replicate 14  Sashimi  ++
                   replicate 14  Dumpling  ++
                   replicate 12  TwoMaki  ++
                   replicate 8   ThreeMaki  ++
                   replicate 6   OneMaki  ++
                   replicate 10  SalmonNigiri  ++
                   replicate 5   SquidNigiri  ++
                   replicate 5   EggNigiri  ++
                   replicate 10  Pudding  ++
                   replicate 6   Wasabi  ++
                   replicate 4   Chopsticks


takeCardFromPlayer :: Card -> Player -> Player
takeCardFromPlayer card player@Player{..} = player {state = newState}
    where
        newHand = takeCardFromHand (gameHand state) card
        -- update _state = _state { gameHand = newHand}
        -- newState = update state
        newState = state {gameHand = newHand}
        -- newState = PlayerState { gameHand = newHand
        --                        , cardsOnTable = cardsOnTable state
        --                        , wasabi = wasabi state
        --                        , turn = turn state
        --                        }

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
                        -- updateState _state = _state { cardsOnTable = newCardsOnTable, gameHand = newHand}
                        -- newState = updateState state
                        newState = state {cardsOnTable = newCardsOnTable, gameHand = newHand}
                        -- newState = PlayerState { gameHand = newHand
                        --                        , cardsOnTable = newCardsOnTable
                        --                        , wasabi = wasabi state
                        --                        , turn = turn state
                        --                        }

applyMoveToGame :: Int -> Move -> GameState -> GameState
applyMoveToGame playerIndex mv game = newGameState
        where
            thisPlayer = players game !! playerIndex -- get the player from the list
            tempPlayerList = delete thisPlayer (players game) -- remove the player from the list
            newPlayer = applyMoveToPlayer mv thisPlayer
            newPlayers = insert newPlayer tempPlayerList  -- put him back in the list
            -- updateState _game = _game {players = newPlayers}
            -- newGameState = updateState game
            newGameState = game {players = newPlayers}
            -- newGameState = GameState { roundN     = roundN game
            --                          , numPlayers = numPlayers game
            --                          , players    = newPlayers
            --                          , sessionID  = sessionID game
            --                          }

                                     --thisPlayer = find (==p) (Game.players game)
                                     --newPlayers :: [Player]
                                     --newPlayers = maybe thisPlayer [p]
                                     -- case thisPlayer of
                                     --     Nothing -> newPlayers = [thisPlayer]
                                     --     Just aPlayer -> newPlayers = [aPlayer]
                                     -- NOT FINISHED!!!!!


-- elemItSelf :: Eq a => a -> [a] -> Maybe a
-- elemItSelf x [] = Nothing
-- elemItSelf x (y:ys) | x==y = Just x
--                     | x/=y = elemItself x ys
