import Control.Exception (assert)
import Control.Monad (forM_)
import Data.List (foldl')

import Util (readInts)

newtype Mass = Mass { massInt :: Int }
  deriving (Eq, Ord, Show)

newtype Fuel = Fuel { fuelInt :: Int }
  deriving (Eq, Ord, Show)

noFuel :: Fuel
noFuel = Fuel 0

instance Semigroup Fuel where
  (Fuel x) <> (Fuel y) = Fuel (x + y)

instance Monoid Fuel where
  mempty = noFuel

main = do

    -- UNIT TESTS
    --

    let tests = [ ("needsFuel 14 2", needsFuel (Mass 14) == Fuel 2)
                , ("recFuel 14 2", recFuel (Mass 14) == Fuel 2)
                , ("recFuel 2 0", recFuel (Mass 2) == noFuel)
                , ("needsFuel 1969 654", recFuel (Mass 1969) == Fuel 654)
                , ("recFuel 1969 966", recFuel (Mass 1969) == Fuel 966)
                , ("mod size 100756", recFuel (Mass 100756) == Fuel 50346)
                ]
    forM_ tests $ \(str, bool) ->
      if bool
        then putStrLn (str ++ " passed!")
        else putStrLn (str ++ " FAILED!")

    -- PART 1
    --

    ms <- map Mass <$> readInts "input1"
    let fuel = foldl' (\a b -> a <> needsFuel b) mempty ms
    putStrLn $ "Part 1: total fuel required for modules: " ++ show (fuelInt fuel)

    -- PART 2
    --

    let fuel2 = foldl' (\a b -> a <> recFuel b) mempty ms
    putStrLn $ "Part 2: revised fuel required for modules: " ++ show (fuelInt fuel2)

needsFuel :: Mass  -- ^ mass
          -> Fuel  -- ^ total fuel req'd to fly mass
needsFuel m = Fuel (massInt m `div` 3 - 2)

-- | Total, recursive, fuel required to fly the given mass
recFuel :: Mass  -- ^ input mass
        -> Fuel  -- ^ total (rec) fuel required to fly with input mass
recFuel m =
  let f = needsFuel m in
  if f <= noFuel
    then noFuel
    else f <> recFuel (fuelToMass f)

-- | How much does fuel weigh?
fuelToMass :: Fuel -> Mass
fuelToMass f = (Mass (fuelInt f))
