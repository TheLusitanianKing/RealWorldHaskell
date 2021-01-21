module Random where

import Control.Monad.State.Lazy
import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

-- will return a pair of identical values
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
    get >>= \gen ->
        let (val, gen') = random gen in
            put gen' >>
                return val

-- same using the do notation
getRandom' :: Random a => RandomState a
getRandom' = do
    gen <- get
    let (val, gen') = random gen
    put gen'
    return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
    oldState <- getStdGen
    let (result, newState) = runState getTwoRandoms oldState
    setStdGen newState
    return result