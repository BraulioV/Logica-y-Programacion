module UbchiKeygen (module UbchiKeygen) where

import System.Random.Shuffle

genKey n = shuffleM [0..(n-1)]