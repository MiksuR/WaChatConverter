module Lib
    (
      createDocument,
    ) where

import Text.Pandoc
import Text.Pandoc.Builder
import Data.Function ((&))
import Data.List (foldl', intersperse)
import Data.Sequence ( Seq(Empty) )
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
}

data BlockAcc = BlockAcc {
  blocks :: [Blocks], prevMsg :: Message, prevSender :: B.ByteString
}

parseStr :: B.ByteString -> Either B.ByteString Message
parseStr str = maybeToRight str $ createMessage <$> date <*> sender
  where
    (dateStr, rest) = C.break ('-'==) str
    date = parseTimeM True defaultTimeLocale "%d/%m/%Y, %H:%M" $
                            C.unpack dateStr :: Maybe LocalTime
    (senderPart, textPart) = C.break (':'==) rest
    sender = let s = C.drop 2 senderPart in if C.null s then Nothing else Just s
    text = C.drop 2 textPart
    createMessage d s = Message d s text

appendToMessage :: Message -> B.ByteString -> Message
appendToMessage msg str = msg { msgText = C.unlines [(msgText msg), str] }

times :: BlockAcc -> Message -> (Int, Int, Int, Int)
times acc msg = (prevDay, prevMonth, curDay, curMonth)
  where
    (_, prevMonth, prevDay) = toGregorian . localDay . time . prevMsg $ acc
    (_, curMonth, curDay) = toGregorian . localDay . time $ msg

divClass :: String -> Attr
divClass n = (T.pack "", [T.pack n], [])

monthH :: (BlockAcc, Message) -> Blocks
monthH (acc, msg) = if prevMonth == curMonth then mempty
                    else divWith (T.pack monthID, [], []) $
                         header 1 (text (T.pack monthFormat))
  where
    (_, prevMonth, _, curMonth) = times acc msg
    monthFormat = formatTime defaultTimeLocale "%B %Y" $ localDay . time $ msg
    monthID = formatTime defaultTimeLocale "%m%Y" $ localDay . time $ msg

dateP :: (BlockAcc, Message) -> Blocks
dateP (acc, msg) = if prevDay == curDay && prevMonth == curMonth then mempty
        else divWith (divClass "date") . para . text . T.pack $
             show curDay ++ "." ++ show curMonth ++ "."
  where
    (prevDay, prevMonth, curDay, curMonth) = times acc msg

senderP :: B.ByteString -> (BlockAcc, Message) -> Blocks
senderP prevSender (acc, msg) = if prevMonth == curMonth &&
                                   prevDay == curDay &&
                                   (prevSender == curSender ||
                                   B.null curSender)
                                then divWith (divClass "sender") (Many Empty)
                                else divWith (divClass "sender") . plain .
                                     text $ decodeUtf8 curSender
                                     `T.append` T.pack ": "
  where
    (prevDay, prevMonth, curDay, curMonth) = times acc msg
    curSender = sender msg

messageP :: (BlockAcc, Message) -> Blocks
messageP (_, msg) = divWith (divClass "msg") . plain . mconcat .
                    (intersperse linebreak) $
                    map (text . decodeUtf8) (C.lines . msgText $ msg)

timeP :: (BlockAcc, Message) -> Blocks
timeP (_, msg) = divWith (divClass "time") . plain . text . T.pack $ timeDay
  where
    timeDay = formatTime defaultTimeLocale "%H:%M" (time msg)

updateAcc :: BlockAcc -> B.ByteString -> BlockAcc
updateAcc acc msg = BlockAcc nBlocks message nSender
  where
    parsed = parseStr msg
    message = case parsed of Right p -> p
                             Left t -> appendToMessage (prevMsg acc) t
    nSender = case parsed of Right p -> sender p
                             Left _ -> B.empty
    nBlock = (divWith (divClass "block")) . mconcat .
             (map ((acc, message) &)) $
             [senderP (prevSender acc), messageP, timeP]
    dateText = monthH (acc, message) <> dateP (acc, message)
    nBlocks = case parsed of Right _ ->
                               blocks acc ++
                               [dateText <> nBlock]
                             Left _ ->
                               init (blocks acc) ++
                               [dateText <> nBlock]

-- TODO: Generate a nice title
createDocument :: B.ByteString -> Pandoc
createDocument msgs = changeMargins . doc . mconcat $ blocks accumulator
  where
    changeMargins = setMeta (T.pack "margin-left") "0.7in" .
                   setMeta (T.pack "margin-right") "0.5in"
    zeroTime = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)
    emptyAcc = BlockAcc [] (Message zeroTime B.empty B.empty) B.empty
    accumulator = foldl' updateAcc emptyAcc $ C.lines msgs
