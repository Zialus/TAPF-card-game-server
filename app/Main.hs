{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GHC.Generics

import           Web.Spock
import           Web.Spock.Config

import           Control.Concurrent.MVar
import           Control.Monad.Trans

import           Data.Aeson              (FromJSON, ToJSON, eitherDecode,
                                          encode)
import           Data.IORef
import           Data.List               (deleteBy, find, insert, insertBy)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Ord                (comparing)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T

import           System.Random

import           Game.Data
import           Game.Funcs
import           Game.Types

import           Lib

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type SessionID = Int

data GameServer = GameServer
    { gameList                :: MVar [(SessionID, GameState)]
    , playersOnline           :: [Player]
    , numberOfPlayersOnServer :: IORef Int
    }

-- | initialize the server state
newServer :: IO GameServer
newServer = do
    games <- newMVar []
    number <- newIORef 0
    return GameServer {gameList = games, playersOnline = [], numberOfPlayersOnServer = number}

newGameState :: Int -> StdGen -> Player -> GameState
newGameState numP seed initialPlayer = newGameRoom
        where newGameRoom = GameState { deckState = (allcardsDeck,seed)
                                      , roundN = 0
                                      , numPlayers = numP
                                      , players = [initialPlayer]
                                      }
newPlayerState :: PlayerState
newPlayerState = PlayerState { gameHand = []
                             , cardsOnTable = []
                             , wasabi = False
                             , turn = 0
                             }

main :: IO ()
main = do
        ref <- newIORef 0
        spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
        runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
        gameServer <- liftIO newServer
        seedForDeck <- liftIO getStdGen
        let initialDeck = (allcardsDeck,seedForDeck)
        currentDeck <- liftIO $ newMVar initialDeck
        get root $
            text "Hello World!"

        get ("hello" <//> var) $ \name -> do
                (DummyAppState ref) <- getState
                visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
                text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

        get "shuffle" $ do
            deck <- liftIO $ takeMVar currentDeck
            let (currentBoard, currentState) = shuffleDeck deck
            liftIO $ putMVar currentDeck (currentBoard, currentState)
            text  ("Current state of the Deck: " <> T.pack ( show currentBoard ))

        post "create" $ do
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String CreateGameInfo
            liftIO $ print bodyDecoded
            case bodyDecoded of
                Left err       -> text $ T.pack err
                Right gameInfo -> do
                          listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)

                          liftIO $ print listOfGamesInServer

                          let newSessionID = if null listOfGamesInServer
                                             then 1
                                             else fst (Prelude.head listOfGamesInServer) + 1
                          seed <- liftIO getStdGen
                          let how_many_players = howManyPlayers gameInfo
                          let playerID = userIDCreating gameInfo
                          let player = Player {pid = playerID, pname = "LOOOOOL", state = newPlayerState} ---- NEEDS TO BE FIXED!!!! ------
                          let newGame = newGameState how_many_players seed player
                          let serverPlusNewGame = insertBy (comparing fst) (newSessionID,newGame) listOfGamesInServer
                          liftIO $ putMVar (gameList gameServer) serverPlusNewGame
                          text "New Game was created"

        post "join" $ do
          boodyOfRequest <- body
          liftIO $ print boodyOfRequest
          let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String JoinGameInfo
          liftIO $ print bodyDecoded
          case bodyDecoded of
              Left err       -> text $ T.pack err
              Right gameInfo -> do
                        listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)

                        let playerID = userIDJoinning gameInfo

                        let fakePlayer = Player {pid= playerID, pname = undefined, state =  undefined}
                        let thisPlayer = find (==fakePlayer) (playersOnline gameServer) -- get the player from the list
                        let foundThisPlayer = fromMaybe (error "couldn't find the player in the online list") thisPlayer

                        let roomID = roomIDtoJoin gameInfo

                        liftIO $ print ("looooool 0:" <> show listOfGamesInServer)


                        let gameToJoin = findBy roomID listOfGamesInServer

                        liftIO $ print ("looooool 1:" <> show gameToJoin)

                        let serverListTmp = deleteBy ( equalling fst ) (roomID,undefined) listOfGamesInServer

                        let (_gameID,game_state) = gameToJoin

                        let updatedListOfPlayers = insert foundThisPlayer (players game_state)

                        let game_state_updated = game_state { players = updatedListOfPlayers }
                        let gameToJoinUpdated = (_gameID,game_state_updated)

                        liftIO $ print ("looooool 2:" <> show gameToJoinUpdated)

                        let serverPlusUpdatedGame = insertBy (comparing fst) gameToJoinUpdated serverListTmp

                        liftIO $ putMVar (gameList gameServer) serverPlusUpdatedGame
                        text "You've joined the game"

        post "/login" $ do
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String UserLoginInfo
            case bodyDecoded of
                Left err    -> text $ T.pack err
                Right texto -> do
                    text $ cs $ encode texto
            -- t <- jsonBody
            -- case t of
            --     Nothing -> return ()
            --     Just a -> liftIO $ print a
            --
            -- text ("done" <> T.pack ( show t ) )


data UserLoginInfo = UserLoginInfo { userIDLogin       :: Int
                                   , userNameLogin     :: T.Text
                                   , userPasswordLogin :: T.Text
                                   } deriving (Show,Generic)

data JoinGameInfo = JoinGameInfo { userIDJoinning :: Int
                                 , roomIDtoJoin   :: Int
                                 } deriving (Show,Generic)


data CreateGameInfo = CreateGameInfo { userIDCreating :: Int
                                     , howManyPlayers :: Int
                                     } deriving (Show,Generic)

instance FromJSON JoinGameInfo
instance ToJSON JoinGameInfo

instance FromJSON UserLoginInfo
instance ToJSON UserLoginInfo

instance FromJSON CreateGameInfo
instance ToJSON CreateGameInfo
