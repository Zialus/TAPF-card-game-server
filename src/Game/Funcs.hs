{-# LANGUAGE RecordWildCards #-}

module Game.Funcs where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List        (delete, elemIndex, find, insert)
import           Data.STRef
import           System.Random
import           Game.Types

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
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
            newArray :: Int -> [a] -> ST s (STArray s Int a)
            newArray n xs =  newListArray (1,n) xs


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
takeCardFromPlayer card Player{..} =
    Player { id = id , state = newState}
    where
        newHand = takeCardFromHand (gameHand state) card
        newState = PlayerState { gameHand = newHand
                               , cardsOnTable = cardsOnTable state
                               , wasabi = wasabi state
                               , turn = turn state
                               }

takeCardFromHand :: [Card] -> Card -> [Card]
takeCardFromHand gameHand card = delete card gameHand

checkValidMove :: Player -> Move -> GameState -> Bool
checkValidMove player move game = undefined

applyMoveToPlayer :: Move -> Player -> Player
applyMoveToPlayer (PlayCard card) player      = takeCardFromPlayer card player

applyMoveToPlayer (SpecialMoveChopStick card1 card2) player = bringChopstickBack playerAfter
                                            where playerAfter = applyMoveToPlayer (PlayCard card1)
                                                              $ applyMoveToPlayer (PlayCard card2) player

bringChopstickBack :: Player -> Player
bringChopstickBack Player{..} = Player {id = id, state = newState}
                    where
                        newHand = insert Chopsticks (gameHand state)
                        newCardsOnTable = delete Chopsticks (cardsOnTable state)
                        updateState _state = _state { cardsOnTable = newCardsOnTable}
                        newState = updateState state
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
            newGameState = GameState { roundN     = roundN game
                                     , numPlayers = numPlayers game
                                     , players    = newPlayers
                                     , sessionID  = sessionID game
                                     }

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
