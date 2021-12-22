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

import Data.Either.Combinators (maybeToRight)
import Data.Maybe (isJust)
import Data.Time

data Message = Message {
  date :: LocalTime, sender :: BL.ByteString, text :: BL.ByteString
} deriving Show

data ParseError = ParseError !Int !BL.ByteString

errorMsg :: ParseError -> String
errorMsg (ParseError line text) = "Could not parse line " ++ show line ++ ": " ++ C.unpack text

parseStr :: (Int, BL.ByteString) -> Either ParseError Message
parseStr (line, str) = maybeToRight parseError $
                        createMessage <$> date <*> sender
  where
    (dateStr, rest) = C.break ('-'==) str
    date = parseTimeM True defaultTimeLocale "%-d.%-m.%-Y klo %H.%M" $ C.unpack dateStr :: Maybe LocalTime
    (senderPart, textPart) = C.break (':'==) rest
    sender = let s = C.drop 2 senderPart in if C.null s then Nothing else Just s
    text = C.drop 2 textPart
    createMessage d s = Message d s text
    parseError = ParseError line str

parseMessages :: BL.ByteString -> Either ParseError [Message]
parseMessages str = mapM parseStr $ zip [1..] (C.lines str)

createDocument :: [Message] -> Pandoc
createDocument (x:xs) = undefined
