{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GHC.Generics

import           Web.Spock
import           Web.Spock.Config

import           Control.Concurrent.MVar
import           Control.Monad.Trans

import           Data.Aeson              (FromJSON, ToJSON, eitherDecode)
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
data MyAppState = DummyAppState GameServer

type SessionID = Int

data GameServer = GameServer
    { gameList      :: MVar [(SessionID, GameState)]
    , playersOnline :: MVar [Player]
    , playerCounter :: IORef Int
    }

-- | initialize the server state
newServer :: IO GameServer
newServer = do
    _games <- newMVar []
    _players <- newMVar []
    _number <- newIORef 0
    return GameServer {gameList = _games, playersOnline = _players, playerCounter = _number}

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
        -- ref <- newIORef 0
        gameServer <- liftIO newServer
        spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState gameServer)
        runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
        seedForDeck <- liftIO getStdGen
        let initialDeck = (allcardsDeck,seedForDeck)
        currentDeck <- liftIO $ newMVar initialDeck
        get root $
            text "Hello World!"

        -- get ("hello" <//> var) $ \name -> do
        --         (DummyAppState ref) <- getState
        --         visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
        --         text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

        get "shuffle" $ do
            deck <- liftIO $ takeMVar currentDeck
            let (currentBoard, currentState) = shuffleDeck deck
            liftIO $ putMVar currentDeck (currentBoard, currentState)
            text  ("Current state of the Deck: " <> T.pack ( show currentBoard ))

        post "create" createRequest

        post "join" joinRequest

        post "/login" loginRequest

joinRequest :: ActionT (WebStateM () MySession MyAppState) ()
joinRequest = do
            (DummyAppState gameServer) <- getState
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String JoinGameInfo
            liftIO $ print bodyDecoded
            case bodyDecoded of
                Left  err      -> text $ T.pack err
                Right gameInfo -> do
                      let roomID = roomIDtoJoin gameInfo
                      let playerID = userIDJoinning gameInfo
                      let fakePlayer = Player {pid= playerID, pname = undefined, state =  undefined}

                      playersOnlineList <- liftIO $ readMVar (playersOnline gameServer)

                      let thisPlayer = find (==fakePlayer) playersOnlineList -- get the player from the list
                      let foundThisPlayer = fromMaybe (error "couldn't find the player in the online list") thisPlayer

                    --   let foundThisPlayer = findPlayerByID playerID gameServer

                      listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)
                      let maybeGameToJoin = findBy roomID listOfGamesInServer
                      let gameToJoin = fromMaybe (error "That game does not exist") maybeGameToJoin
                      let serverListTmp = deleteBy ( equalling fst ) (roomID,undefined) listOfGamesInServer

                      let (game_id,game_state) = gameToJoin
                      let updatedListOfPlayers = insert foundThisPlayer (players game_state)
                      let game_state_updated = game_state { players = updatedListOfPlayers }
                      let gameToJoinUpdated = (game_id,game_state_updated)
                      -- liftIO $ print ("looooool 2:" <> show gameToJoinUpdated)
                      let serverPlusUpdatedGame = insertBy (comparing fst) gameToJoinUpdated serverListTmp

                      liftIO $ putMVar (gameList gameServer) serverPlusUpdatedGame
                      text "You've joined the game"

createRequest :: ActionT (WebStateM () MySession MyAppState) ()
createRequest = do
            (DummyAppState gameServer) <- getState
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String CreateGameInfo
            liftIO $ print bodyDecoded
            case bodyDecoded of
                Left  err      -> text $ T.pack err
                Right gameInfo -> do
                    let how_many_players = howManyPlayers gameInfo
                    let playerID = userIDCreating gameInfo

                    let fakePlayer = Player {pid= playerID, pname = undefined, state =  undefined}
                    playersOnlineList <- liftIO $ readMVar (playersOnline gameServer)
                    let thisPlayer = find (==fakePlayer) playersOnlineList -- get the player from the list
                    let foundThisPlayer = fromMaybe (error "player isn't online, therefore can't create a game") thisPlayer

                    seed <- liftIO getStdGen
                    let newGame = newGameState how_many_players seed foundThisPlayer

                    listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)
                    liftIO $ print listOfGamesInServer
                    let newSessionID = if null listOfGamesInServer
                                     then 1
                                     else fst (last listOfGamesInServer) + 1
                    let serverPlusNewGame = insertBy (comparing fst) (newSessionID,newGame) listOfGamesInServer
                    liftIO $ putMVar (gameList gameServer) serverPlusNewGame
                    text("New Game with id: " <> T.pack ( show newSessionID ) <> " was created by player " <> T.pack ( show thisPlayer ) )

loginRequest :: ActionT (WebStateM () MySession MyAppState) ()
loginRequest = do
            (DummyAppState gameServer) <- getState
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String UserLoginInfo
            liftIO $ print bodyDecoded
            case bodyDecoded of
                Left  err      -> text $ T.pack err
                Right userInfo -> do
                    let uname = cs $ userNameLogin userInfo
                    -- let passwd = userPasswordLogin userInfo  -- not using password for now

                    playersOnlineList <- liftIO $ takeMVar (playersOnline gameServer)
                    let counter = playerCounter gameServer
                    userID <- liftIO $ atomicModifyIORef' counter $ \i -> (i+1, i+1)
                    let newPlayer = Player {pid = userID, pname = uname, state = newPlayerState}
                    let updatedUserList = insert newPlayer playersOnlineList
                    liftIO $ putMVar (playersOnline gameServer) updatedUserList

                    liftIO $ print newPlayer

                    text ("You've just logged in! You are user: " <> T.pack ( show uname ) <> " with userID: " <> T.pack ( show userID)  )



data UserLoginInfo = UserLoginInfo { userNameLogin     :: !T.Text
                                   , userPasswordLogin :: !T.Text
                                   } deriving (Show,Generic)

data JoinGameInfo = JoinGameInfo { userIDJoinning :: !Int
                                 , roomIDtoJoin   :: !Int
                                 } deriving (Show,Generic)


data CreateGameInfo = CreateGameInfo { userIDCreating :: !Int
                                     , howManyPlayers :: !Int
                                     } deriving (Show,Generic)

instance FromJSON JoinGameInfo
instance ToJSON JoinGameInfo

instance FromJSON UserLoginInfo
instance ToJSON UserLoginInfo

instance FromJSON CreateGameInfo
instance ToJSON CreateGameInfo


-- findPlayerByID :: Int -> GameServer -> ActionT (WebStateM () MySession MyAppState) Player
-- findPlayerByID playerID gameServer = do
--      let fakePlayer = Player {pid= playerID, pname = undefined, state =  undefined}
--      playersOnlineList <- liftIO $ readMVar (playersOnline gameServer)
--      let thisPlayer = find (==fakePlayer) playersOnlineList -- get the player from the list
--      let foundThisPlayer = fromMaybe (error "The Player isn't logged in") thisPlayer
--      return foundThisPlayer
