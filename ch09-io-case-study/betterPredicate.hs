import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException(..))
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
-- we can't use filter to call our predicate p,
-- as p's purity means it cannot do the I/O needed to gather the metadata it requires.
-- filterM behaves like the normal filter function, but in this case it evaluates its predicate in the IO monad,
-- allowing the predicate to perform I/O.
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(SomeException _) -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

-- acquire-use-release cycle with bracket
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

-- our predicate will check for a C++ source file that is over 128KB in size.
myTest :: Predicate
myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

type InfoP a =  FilePath      -- path to directory entry
             -> Permissions   -- permissions
             -> Maybe Integer -- file size (Nothing if not file)
             -> UTCTime       -- last modified
             -> a
-- We could thus legitimately get rid of the Predicate type, as it is simply: InfoP Bool

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)