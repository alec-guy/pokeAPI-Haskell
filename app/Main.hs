{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty 
import Network.HTTP.Simple
import Data.Cache as Cache 
import Network.Wai.Middleware.AddHeaders 





main :: IO ()
main = do
    cache <- newCache Nothing
    scotty 3000 $ do
      middleware (addHeaders [("Access-Control-Allow-Origin","*")])
      get "/api/v2/pokemon" $ do
           liftIO $ putStrLn "GET /api/v2/pokemon"
           maybepokemon <- liftIO $ (Cache.lookup cache ("150pokemon" :: String))
           case maybepokemon of 
            Nothing -> do
               liftIO $ putStrLn "Did not use cache for 150 pokemon" 
               response <- httpLBS $ parseRequest_ "http://pokeapi.co/api/v2/pokemon?limit=150"
               let body = getResponseBody response
               liftIO (insert cache ("150pokemon" :: String) body) 
               setHeader "Content-Type" "application/json" 
               raw body 
            Just pokemon -> do 
                 liftIO $ putStrLn "Did use cache for 150 pokemon"
                 raw pokemon
