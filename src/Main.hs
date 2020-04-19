{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HelloApi
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Environment             ( getEnv )
import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )

server :: Server HelloAPI
server = hello :<|> user
 where
  hello = return "Hello world"
  user n a = return $ User n a

app :: Application
app = serve helloApi server

main :: IO ()
main = do
  port <- fmap (fromMaybe 8080 . readMaybe) (getEnv "PORT")
  putStrLn ("Serving on port " ++ (show port))
  run port app
