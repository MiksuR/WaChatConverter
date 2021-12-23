module Lib
    (
      parseMessages,
      createDocument,
      errorMsg
    ) where

import Text.Pandoc
import Text.Pandoc.Builder
import Data.Sequence ( Seq(Empty) )
import Data.List (foldl')
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Either.Combinators (maybeToRight)
import Data.Maybe (isJust)
import Data.Time

data Message = Message {
  time :: LocalTime, sender :: B.ByteString, msgText :: B.ByteString
} deriving Show

data ParseError = ParseError !Int !B.ByteString

type BlockAcc = (Blocks, [LocalTime], [LocalTime], B.ByteString)
emptyAcc = (mempty, [], [], C.pack "")

errorMsg :: ParseError -> String
errorMsg (ParseError line text) = "Could not parse line " ++ show line ++ ": " ++ C.unpack text

parseStr :: (Int, B.ByteString) -> Either ParseError Message
parseStr (line, str) = maybeToRight parseError $
                        createMessage <$> date <*> sender
  where
    (dateStr, rest) = C.break ('-'==) str
    date = parseTimeM True defaultTimeLocale "%-d.%-m.%-Y klo %-H.%M" $ C.unpack dateStr :: Maybe LocalTime
    (senderPart, textPart) = C.break (':'==) rest
    sender = let s = C.drop 2 senderPart in if C.null s then Nothing else Just s
    text = C.drop 2 textPart
    createMessage d s = Message d s text
    parseError = ParseError line str

parseMessages :: B.ByteString -> Either ParseError [Message]
parseMessages str = mapM parseStr $ zip [1..] (C.lines str)

updateAcc :: BlockAcc -> Message -> BlockAcc
updateAcc (blocks, months, msgs, lSender) msg = (nBlock, nMonths, nMsgs, nSender)
  where
    nSender = sender msg
    nMsgs = if lSender == nSender then msgs else msgs ++ [time msg]
    (_, prevMonth, _) = case msgs of [] -> (0, 0, 0)
                                     x -> toGregorian . localDay . last $ x
    (_, curMonth, _) = toGregorian . localDay . time $ msg
    nMonths = if prevMonth == curMonth then months else months ++ [time msg]
    monthPar = if prevMonth == curMonth then mempty
               else header 1 (text (T.pack $ show curMonth))
    senderPar = if lSender == nSender then mempty
                else para $ text (decodeUtf8 nSender `T.append` T.pack ": ")
    messageText = para $ text (decodeUtf8 $ msgText msg)
    nBlock = blocks <> monthPar <> senderPar <> messageText

createDocument :: [Message] -> Pandoc
createDocument msgs = doc $ blocks
  where
    toc = undefined
    (blocks, months, _, _) = foldl' updateAcc emptyAcc msgs
