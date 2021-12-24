module Lib
    (
      parseMessages,
      createDocument,
      errorMsg
    ) where

import Text.Pandoc
import Text.Pandoc.Builder
import Data.List (foldl')
import Data.Sequence (Seq)
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
    date = parseTimeM True defaultTimeLocale "%-d.%-m.%-Y klo %-H.%M" $
                            C.unpack dateStr :: Maybe LocalTime
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
    (_, prevMonth, prevDay) = case msgs of
                                [] -> (0, 0, 0)
                                x -> toGregorian . localDay . last $ x
    (_, curMonth, curDay) = toGregorian . localDay . time $ msg
    nMonths = if prevMonth == curMonth then months else months ++ [time msg]
    monthFormat = formatTime defaultTimeLocale "%B %Y" $ localDay . time $ msg
    monthH = if prevMonth == curMonth then mempty
               else header 1 (text (T.pack monthFormat))
    optionMaker n = (T.pack "", [T.pack n], [])
    dateText = if prevDay == curDay then mempty
                else divWith (optionMaker "date") $
                     plain $ text $ T.pack $
                     show curDay ++ "." ++ show curMonth ++ "."
    senderText = if lSender == nSender then mempty
                else divWith (optionMaker "sender") $
                     plain $ text (decodeUtf8 nSender `T.append` T.pack ": ")
    messagePar = divWith (optionMaker "msg") $
                    plain $ text (decodeUtf8 $ msgText msg)
    tod = localTimeOfDay . time $ msg
    timeDay = show (todHour tod) ++ ":" ++ show (todMin tod)
    timeText = divWith (optionMaker "time") $
                    plain $ text $ T.pack timeDay
    nBlock = blocks <> monthH <> dateText <> senderText <> messagePar <> timeText

createDocument :: [Message] -> Pandoc
createDocument msgs = doc $ blocks
  where
    toc = undefined
    (blocks, months, _, _) = foldl' updateAcc emptyAcc msgs
