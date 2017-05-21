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
type Handler = ActionT (WebStateM () MySession MyAppState)

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

        post "login" loginRequest

        post "play" playRequest

joinRequest :: Handler ()
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
              let thisPlayer = find (==fakePlayer) playersOnlineList

              --   let foundThisPlayer = fromMaybe (error "couldn't find the player in the online list") thisPlayer
            --   foundThisPlayer <- findPlayerByID playerID gameServer

              case thisPlayer of
                  Nothing     -> text "player isn't online, therefore can't join a game"
                  Just player -> do
                      listOfGamesInServerTMP <- liftIO $ readMVar (gameList gameServer)
                      let maybeGameToJoinTMP = findBy roomID listOfGamesInServerTMP
                      case maybeGameToJoinTMP of
                          Nothing -> text "That game does not exist, you can't join it"
                          Just _ -> do
                              listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)

                              let maybeGameToJoin = findBy roomID listOfGamesInServer -- i need to grab it again becauase previously readMvar was used, now takeMvar was used
                              let gameToJoin = fromMaybe (error "This can't even happen") maybeGameToJoin
                              let serverListTmp = deleteBy ( equalling fst ) (roomID,undefined) listOfGamesInServer
                              let (game_id,game_state) = gameToJoin
                              if length (players game_state) == numPlayers game_state
                                  then text "You can't enter, the room is full!"
                                  else do
                                      let updatedListOfPlayers = insert player (players game_state)
                                      let game_state_updated = game_state { players = updatedListOfPlayers }
                                      if length (players game_state_updated) == numPlayers game_state_updated
                                          then do
                                            let game_state_updated2 = startGame game_state_updated
                                            let gameToJoinUpdated = (game_id,game_state_updated2)
                                            let serverPlusUpdatedGame = insertBy (comparing fst) gameToJoinUpdated serverListTmp
                                            liftIO $ putMVar (gameList gameServer) serverPlusUpdatedGame
                                            text ("The Game starts now!!!! " <> T.pack (show game_state_updated2) )
                                          else do
                                            let gameToJoinUpdated = (game_id,game_state_updated)
                                            let serverPlusUpdatedGame = insertBy (comparing fst) gameToJoinUpdated serverListTmp
                                            liftIO $ putMVar (gameList gameServer) serverPlusUpdatedGame
                                            text ("Still waiting for the game to begin: " <> T.pack ( show game_id) <> " and you are user: " <> T.pack ( show player ) )


createRequest :: Handler ()
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
                    let thisPlayer = find (==fakePlayer) playersOnlineList

                    -- let foundThisPlayer = fromMaybe (error "player isn't online, therefore can't create a game") thisPlayer

                    case thisPlayer of
                        Nothing     -> text "player isn't online, therefore can't create a game"
                        Just player -> do
                            seed <- liftIO getStdGen
                            let newGame = newGameState how_many_players seed player -- when a user creates a game, he joins it too

                            listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)
                            -- liftIO $ print listOfGamesInServer
                            let newSessionID = if null listOfGamesInServer
                                               then 1
                                               else fst (last listOfGamesInServer) + 1
                            let serverPlusNewGame = insertBy (comparing fst) (newSessionID,newGame) listOfGamesInServer
                            liftIO $ putMVar (gameList gameServer) serverPlusNewGame

                            text("New Game with id: " <> T.pack ( show newSessionID ) <> " was created by player: " <> T.pack ( show player ) )


loginRequest :: Handler ()
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


playRequest :: Handler ()
playRequest = do
            (DummyAppState gameServer) <- getState
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String UserPlayInfo
            liftIO $ print bodyDecoded
            case bodyDecoded of
                Left  err      -> text $ T.pack err
                Right playInfo -> do
                      let roomID = roomIDtoPlay playInfo
                      let playerID = userIDPlaying playInfo

                      let moveText = cs $ playerMove playInfo

                      case maybeRead moveText :: Maybe Move of
                          Nothing -> text "Not a valid move type"
                          Just _ ->   do
                            let fakePlayer = Player {pid= playerID, pname = undefined, state =  undefined}
                            playersOnlineList <- liftIO $ readMVar (playersOnline gameServer)
                            let thisPlayer = find (==fakePlayer) playersOnlineList -- get the player from the list
                            let foundThisPlayer = fromMaybe (error "player isn't online") thisPlayer

                            listOfGamesInServer <- liftIO $ takeMVar (gameList gameServer)
                            let maybeGameToJoin = findBy roomID listOfGamesInServer
                            let gameToPlay = fromMaybe (error "That game does not exist") maybeGameToJoin
                            let serverListTmp = deleteBy ( equalling fst ) (roomID,undefined) listOfGamesInServer

                            let (game_id,game_state) = gameToPlay

                            let moveTypeInList = words moveText

                            let moveType = moveTypeInList !! 0

                            case moveType of
                              "PlayCard" -> do
                                  let card = moveTypeInList !! 1
                                  let move = PlayCard (read card :: Card)
                                  liftIO $ print move
                                  let gameStateAfterMove = applyMoveToGame foundThisPlayer move game_state
                                  let gameAfterStateUpdate = (game_id,gameStateAfterMove)
                                  liftIO $ print gameAfterStateUpdate
                                  let serverPlusUpdatedGame = insertBy (comparing fst) gameAfterStateUpdate serverListTmp
                                  liftIO $ putMVar (gameList gameServer) serverPlusUpdatedGame
                                  text "The move has been applied to the game"
                              "SpecialMoveChopStick" -> do
                                  let card1 = moveTypeInList !! 1
                                  let card2 = moveTypeInList !! 2
                                  let move = SpecialMoveChopStick (read card1 :: Card) (read card2 :: Card)
                                  liftIO $ print move
                                  let gameStateAfterMove = applyMoveToGame foundThisPlayer move game_state
                                  let gameAfterStateUpdate = (game_id,gameStateAfterMove)
                                  liftIO $ print gameAfterStateUpdate
                                  let serverPlusUpdatedGame = insertBy (comparing fst) gameAfterStateUpdate serverListTmp
                                  liftIO $ putMVar (gameList gameServer) serverPlusUpdatedGame
                                  text "not finished"
                              _ -> do
                                  _ <- error "this won't happen"
                                  text "ths will never happen"



data UserLoginInfo = UserLoginInfo { userNameLogin     :: !T.Text
                                   , userPasswordLogin :: !T.Text
                                   } deriving (Show,Generic)

data UserPlayInfo = UserPlayInfo { userIDPlaying :: !Int
                                 , roomIDtoPlay  :: !Int
                                 , userRound     :: !Int
                                 , playerMove    :: !T.Text
                                 } deriving (Show,Generic)


data JoinGameInfo = JoinGameInfo { userIDJoinning :: !Int
                                 , roomIDtoJoin   :: !Int
                                 } deriving (Show,Generic)


data CreateGameInfo = CreateGameInfo { userIDCreating :: !Int
                                     , howManyPlayers :: !Int
                                     } deriving (Show,Generic)


instance FromJSON UserPlayInfo
instance ToJSON UserPlayInfo

instance FromJSON JoinGameInfo
instance ToJSON JoinGameInfo

instance FromJSON UserLoginInfo
instance ToJSON UserLoginInfo

instance FromJSON CreateGameInfo
instance ToJSON CreateGameInfo


findPlayerByID :: Int -> GameServer -> Handler Player
findPlayerByID playerID gameServer = do
     let fakePlayer = Player {pid= playerID, pname = undefined, state =  undefined}
     playersOnlineList <- liftIO $ readMVar (playersOnline gameServer)
     let thisPlayer = find (==fakePlayer) playersOnlineList -- get the player from the list
     let foundThisPlayer = fromMaybe (error "The Player isn't logged in") thisPlayer
     return foundThisPlayer
