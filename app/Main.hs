module Main where

import Lib

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Text.Pandoc
import Text.Pandoc.PDF
import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

extractFirstArg :: [String] -> IO String
extractFirstArg [] = error "No arguments provided"
extractFirstArg (x:_) = pure x

main :: IO ()
main = do
  templRaw <- runIO (getTemplate "template.html") >>= handleError
  Right templ <- compileTemplate "" templRaw :: IO (Either String (Template T.Text))
  chatsPath <- getArgs >>= extractFirstArg
  -- TODO: Try implement lazy parsing
  chats <- B.readFile chatsPath
  let doc = createDocument chats
  {--
  pdf <- runIO (writeHtml5String (def {writerTemplate = Just templ}) doc) >>= handleError
  B.writeFile "out.html" $ E.encodeUtf8 pdf
  --}
  pdf <- runIO (makePDF "wkhtmltopdf" ["toc","--enable-internal-links"] writeHtml5String
                (def {writerTemplate = Just templ}) doc) >>= handleError
  case pdf of Right b -> BL.writeFile "out.pdf" b
              Left e -> print e
