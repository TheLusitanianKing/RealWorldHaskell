module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    -- forM is basically mapM with its arguments flipped
    -- mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
    -- forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
        -- Since an if expression requires an expression of type Bool,
        -- we have to use <- to get the Bool result of the action out of its IO wrapper,
        -- so that we can use the plain, unwrapped Bool in the if
    return (concat paths)