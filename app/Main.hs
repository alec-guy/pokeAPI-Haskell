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
                 liftIO $ putStrLn ("Did not use cache for " <> (Prelude.show x) <> " pokemon.")
                 response <- httpLBS $ parseRequest_ ("http://pokeapi.co/api/v2/pokemon?limit=" <> (Prelude.show x))
                 let body = getResponseBody response
                 liftIO (insert cache ((Prelude.show x) :: String) body) 
                 setHeader "Content-Type" "application/json" 
                 raw body 
               Just pokemon -> do 
                 liftIO $ putStrLn ("Did use cache for " <> (Prelude.show x) <> " pokemon.") 
                 raw pokemon
      get "/api/v2/pokemon/:pokemonName" $ do 
            pokemonName <- (pathParam "pokemonName") :: ActionM String 
            liftIO $ putStrLn $ "GET /api/v2/pokemon/" <> pokemonName
            maybeData <- liftIO $ (Cache.lookup cache pokemonName) 
            case maybeData of 
             Nothing -> do 
              liftIO $ putStrLn ("Did not use cache for " <> pokemonName <> ".") 
              response <- httpLBS $ parseRequest_ ("http://pokeapi.com/api/v2/pokemon/ " <> pokemonName)
              let body = getResponseBody response 
              liftIO (insert cache pokemonName body) 
              setHeader "Content-Type" "application/json"
              raw body
             Just data1 -> do 
              liftIO $ putStrLn ("Did use cache for " <> pokemonName <> ".") 
              raw data1 
            
