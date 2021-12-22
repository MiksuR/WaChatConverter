{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      parseMessages,
      createDocument,
      errorMsg
    ) where

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

data Message = Message {
  date :: (), time :: (), sender :: T.Text, text :: T.Text
} deriving Show

data ParseError = ParseError !Int !String

errorMsg :: ParseError -> String
errorMsg (ParseError line text) = "Could not parse line " ++ show line ++ ": " ++ text

parseStr :: (Int, BL.ByteString) -> Either ParseError Message
parseStr (line, str) = Right Message { date = (), time = (),
                                       sender = T.pack "tester",
                                       text = T.pack . C.unpack $ str }

parseMessages :: BL.ByteString -> Either ParseError [Message]
parseMessages str = mapM parseStr $ zip [1..] (C.lines str)

createDocument :: [Message] -> Pandoc
createDocument (x:xs) = undefined
