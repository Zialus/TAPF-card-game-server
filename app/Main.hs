{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Data.Aeson              (FromJSON, Object, ToJSON, decode,
                                          eitherDecode, encode)
import           Data.IORef
import           Data.Monoid
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           System.Random

import           Game.Data
import           Game.Funcs
import           Game.Types

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type SessionID = Int
data GameServer = GameServer {
    gameList :: MVar [(SessionID, GameState)]
}

-- | initialize the server state
newServer :: IO GameServer
newServer = do
  games <- newMVar []
  return GameServer {gameList = games}

main :: IO ()
main = do
        ref <- newIORef 0
        spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
        runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
        gameServer <- liftIO newServer
        seed <- liftIO getStdGen
        let initialDeck = (allcardsDeck,seed)
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
        post "/login" $ do
            boodyOfRequest <- body
            liftIO $ print boodyOfRequest
            let bodyDecoded = eitherDecode $ cs boodyOfRequest :: Either String Object
            case bodyDecoded of
                Left err    -> text $ T.pack err
                Right texto -> text $ cs $ encode texto
            -- t <- jsonBody
            -- case t of
            --     Nothing -> return ()
            --     Just a -> liftIO $ print a
            --
            -- text ("done" <> T.pack ( show t ) )
