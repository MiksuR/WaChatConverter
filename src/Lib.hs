module Lib
    (
      parseMessages,
      createDocument
    ) where

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL

data Message = Message {
  date :: (), time :: (), sender :: T.Text, text :: T.Text
}

parseMessages :: BL.ByteString -> [Message]
parseMessages = undefined

createDocument :: [Message] -> Pandoc
createDocument = undefined
