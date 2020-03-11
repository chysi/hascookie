{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Control.Monad.Trans (liftIO)
-- import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import           Web.Spock ((<//>))
import qualified Web.Spock as Spock
import qualified Web.Spock.Config as SC
import Types


type ServerConn    = ()
data ServerSession = ServerSession
data ServerState   = ServerState

type Server a = Spock.SpockM ServerConn ServerSession ServerState a


main :: IO ()
main = do
    -- ref <- newIORef 0
    spockCfg <- SC.defaultSpockCfg ServerSession SC.PCNoDatabase ServerState
    Spock.runSpock 8080 (Spock.spock spockCfg app)


app :: Server ()
app = do
    Spock.get Spock.root $
        Spock.html "<h1 style=\"color:red\">Hello World!</h1>"

    Spock.post "icanhascookie" $ do
        params <- Spock.paramsPost
        let maybeOrderData = parseOrderBody params
        Spock.text $ maybe "Invalid form" (T.pack . show) maybeOrderData

    Spock.post "orderstatus" $ do
        Spock.setStatus Http.status501

    Spock.get ("hello" <//> Spock.var) $ \name -> do
        -- (DummyAppState ref) <- Spock.getState
        -- visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
        -- Spock.text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
        Spock.text ("Hello " <> name)
