{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty 
import Network.HTTP.Simple
import Data.Cache as Cache 
import Network.Wai.Middleware.AddHeaders 
import Control.Exception (SomeException)
import Data.Text.Lazy as TL 




main :: IO ()
main = do
    cache <- newCache Nothing
    scotty 3000 $ do
      middleware (addHeaders [("Access-Control-Allow-Origin","*")])
      get "/api/v2/pokemon" $ do
           liftIO $ putStrLn "GET /api/v2/pokemon"
           maybeLimit <- catch (Just <$> (queryParam ("limit" :: TL.Text) :: ActionM Int)) $ \e -> do
                                                           let err = Prelude.show (e :: SomeException) 
                                                           liftIO $ putStrLn err 
                                                           return Nothing  
           case maybeLimit of 
             Nothing -> return () 
             (Just x) -> do
              maybepokemon <- liftIO $ (Cache.lookup cache ((Prelude.show x) :: String))
              case maybepokemon of 
               Nothing -> do
                 liftIO $ putStrLn "Did not use cache for 150 pokemon" 
                 response <- httpLBS $ parseRequest_ "http://pokeapi.co/api/v2/pokemon?limit=150"
                 let body = getResponseBody response
                 liftIO (insert cache ((Prelude.show x) :: String) body) 
                 setHeader "Content-Type" "application/json" 
                 raw body 
               Just pokemon -> do 
                 liftIO $ putStrLn "Did use cache for 150 pokemon"
                 raw pokemon
