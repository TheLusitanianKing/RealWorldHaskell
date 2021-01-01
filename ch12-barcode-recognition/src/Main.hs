module Main where

import Parse (parse)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment (getArgs)
import Barcode (parseRawPPM, findEAN13)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do
    e <- parse parseRawPPM <$> L.readFile arg
    case e of
      Left err ->     print $ "error: " ++ err
      Right pixmap -> print $ findEAN13 pixmap