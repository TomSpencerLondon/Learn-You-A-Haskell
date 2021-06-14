import Data.List (nub, sort)  
import Data.List hiding (nub)  
import qualified Data.Map
import qualified Data.Map as M

import Data.List
:m + data.list

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

find (>4) [1, 2, 3, 4]

