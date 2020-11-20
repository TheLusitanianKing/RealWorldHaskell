import qualified Data.ByteString.Lazy.Char8 as L

closing :: L.ByteString -> Maybe Int
closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case L.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case L.readInt (L.tail rest) of
          Nothing           -> Nothing
          Just (cents, _) ->
            Just (dollars * 100 + cents)

highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L.lines
-- We use one trick to work around the fact that we cannot supply an empty list to the maximum function

highestCloseFrom :: FilePath -> IO ()
highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)