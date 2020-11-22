import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
    greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
} deriving (Eq)
-- a Haskell Show instance should produce a string representation that we can read back by calling read
-- however, for a bitmap graphics file, this would potentially produce huge text strings
-- for this reason, we're not going to let the compiler automatically derive a Show instance for us
-- we'll write our own, and intentionally simplify it
instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
    case matchHeader (L8.pack "P5") s of
        Nothing -> Nothing
        Just s1 ->
            case getNat s1 of
                Nothing -> Nothing
                Just (width, s2) ->
                    case getNat (L8.dropWhile isSpace s2) of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNat (L8.dropWhile isSpace s3) of
                                Nothing -> Nothing
                                Just (maxGrey, s4)
                                    | maxGrey > 255 -> Nothing
                                    | otherwise ->
                                        case getBytes 1 s4 of
                                            Nothing -> Nothing
                                            Just (_, s5) ->
                                                case getBytes (width * height) s5 of
                                                    Nothing -> Nothing
                                                    Just (bitmap, s6) ->
                                                        Just (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
                Nothing -> Nothing
                Just (x, rest)
                    | x <= 0 -> Nothing
                    | otherwise -> Just (fromIntegral x, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n 
                     both@(prefix, _) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both
