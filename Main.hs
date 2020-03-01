{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans (liftIO)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Text as T
import           Web.Spock ((<//>))
import qualified Web.Spock as Spock
import qualified Web.Spock.Config as SC

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- SC.defaultSpockCfg EmptySession SC.PCNoDatabase (DummyAppState ref)
       Spock.runSpock 8080 (Spock.spock spockCfg app)

app :: Spock.SpockM () MySession MyAppState ()
app =
    do Spock.get Spock.root $
           Spock.text "Hello World!"
       Spock.get ("hello" <//> Spock.var) $ \name ->
           do (DummyAppState ref) <- Spock.getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              Spock.text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
