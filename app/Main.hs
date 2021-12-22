module Main where

import Lib

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

extractFirstArg :: [String] -> IO String
extractFirstArg [] = error "No arguments provided"
extractFirstArg (x:_) = pure x

main :: IO ()
main = do
  chatsPath <- getArgs >>= extractFirstArg
  chats <- BL.readFile chatsPath
  parsed <- either (error . errorMsg) pure (parseMessages chats)
  let doc = createDocument $ parsed
  print $ parsed
