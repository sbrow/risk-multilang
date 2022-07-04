-- | Functions for exploring the possibility space of the board game Risk.
module Risk (Result (Result), Possibility (Possibility), battle, fight, odds, possibleOutcomes) where

import Data.List
import Data.Ord
import Data.Tree
import Dice

-- | Represents A number of attackers and defenders respectively.
data Result = Result (Integer, Integer) deriving (Eq, Ord, Show)

instance Num Result where
  Result (x, y) + Result (x', y') = Result (x + x', y + y')
  Result (x, y) - Result (x', y') = Result (x - x', y - y')
  Result (x, y) * Result (x', y') = Result (x * x', y * y')
  abs (Result (x, y)) = Result (abs x, abs y)
  signum (Result (x, y)) = Result (signum x, signum y)

-- | A Particular result, and the Probability of arriving at it.
-- The Probability should be a number between 0 and 1, inclusive.
data Possibility = Possibility (Result, Float) deriving (Ord, Show)

instance Num Possibility where
  Possibility (a, b) + Possibility (a', b') = Possibility (a, b + b')
  Possibility (a, b) - Possibility (a', b') = Possibility (a, b - b')
  abs pos = pos
  signum pos = 1

instance Eq Possibility where
  Possibility (a, b) == Possibility (a', b') = a == a'

-- | Compares two dice and returns the casualties as a 'Result'.
-- The 'Result' returned is relative e.g.
-- @
--   Result(0, 1)
-- @
-- . You must add the 'Result' to Your starting numbers.
battle :: Integer -> Integer -> Result
battle att def
  | att == 0 = Result (0, 0)
  | def == 0 = Result (0, 0)
  | att <= def = Result (-1, 0)
  | att >  def = Result (0, -1)

-- | Returns the losses of a battle given the attacking and defending dice rolls.
-- Requires pre-sorted arguments
fight :: ((Integer, Integer, Integer), (Integer, Integer)) -> Result
fight ((attHigh, attMid, attLow), (defHigh, defLow)) = battle attHigh defHigh + battle attMid defLow

-- | Returns a Tree of all the possible results of fighting,
-- including intermediate (non-terminal) results.
odds :: Integer -> Integer -> Tree Possibility
odds att def = unfoldTree buildNode $ Possibility (Result (att, def), 1.0)
  where
    buildNode :: Possibility -> (Possibility, [Possibility])
    buildNode node
      | def == 0 || att == 0 = (node, [])
      | def == 1 && att == 1 = (node, oneVoneLosses node)
      | def == 1 && att == 2 = (node, twoVoneLosses node)
      | def == 1 && att >= 3 = (node, threeVoneLosses node)
      | def >= 2 && att == 1 = (node, oneVtwoLosses node)
      | def >= 2 && att == 2 = (node, twoVtwoLosses node)
      | def >= 2 && att >= 3 = (node, threeVtwoLosses node)
      where
        Possibility (Result (att, def), p) = node

-- | Returrns a linked list of all the possible terminal outcomes of fighting.
-- Assumes attackers and defenders will always roll as many dice as possible,
-- And will contiune fighting until one side is dead.
possibleOutcomes :: Integer -> Integer -> [Possibility]
possibleOutcomes att def = outcomes
  where
    outcomes = [sumProbabilities p | p <- groupedResults]
    sumProbabilities = foldr (+) (Possibility (Result (0, 0), 0))
    groupedResults = (group . reverse . sort . removeStems . flatten) results
    removeStems = filter onlyLeaves
    onlyLeaves (Possibility (Result (att, def), p)) = att == 0 || def == 0
    results = odds att def

-- Converts a list of dice rolls into tuples that can be fed to fight.
toRolls :: [[Integer]] -> [((Integer, Integer, Integer), (Integer, Integer))]
toRolls dice = [toTuple . sortArgs . splitArgs $ x | x <- dice]
  where
    -- Split args int attackers and defenders
    splitArgs = splitAt 3
    sortArgs (att, def) = (sortOn Down att, sortOn Down def)
    toTuple (x, y) = ((x !! 0, x !! 1, x !! 2), (y !! 0, y !! 1))

groupRolls :: [((Integer, Integer, Integer), (Integer, Integer))] -> [[Result]]
groupRolls rolls = (group . reverse . sort) [fight roll | roll <- rolls]

rollsByProbability :: [((Integer, Integer, Integer), (Integer, Integer))] -> Integer -> Possibility -> [Possibility]
rollsByProbability rolls tot (Possibility (units, p)) = [Possibility (units + head loss, p * (fromIntegral (length loss) / fromIntegral tot)) | loss <- groupRolls rolls]

oneVoneLosses :: Possibility -> [Possibility]
oneVoneLosses = rollsByProbability rolls (total 2 6)
  where
    rolls = toRolls . fill $ 2 `d` 6
    fill rolls = [[roll !! 0, 0, 0, roll !! 1, 0] | roll <- rolls]

twoVoneLosses :: Possibility -> [Possibility]
twoVoneLosses = rollsByProbability rolls (total 3 6)
  where
    rolls = toRolls . fill $ 3 `d` 6
    fill rolls = [[roll !! 0, roll !! 1, 0, roll !! 2, 0] | roll <- rolls]

oneVtwoLosses :: Possibility -> [Possibility]
oneVtwoLosses = rollsByProbability rolls (total 3 6)
  where
    rolls = toRolls . fill $ 3 `d` 6
    fill rolls = [[roll !! 0, 0, 0, roll !! 1, roll !! 2] | roll <- rolls]

twoVtwoLosses :: Possibility -> [Possibility]
twoVtwoLosses = rollsByProbability rolls (total 4 6)
  where
    rolls = toRolls . fill $ 4 `d` 6
    fill rolls = [[roll !! 0, roll !! 1, 0, roll !! 2, roll !! 3] | roll <- rolls]

threeVtwoLosses :: Possibility -> [Possibility]
threeVtwoLosses = rollsByProbability rolls (total 5 6)
  where
    rolls = toRolls $ 5 `d` 6

threeVoneLosses :: Possibility -> [Possibility]
threeVoneLosses = rollsByProbability rolls (total 4 6)
  where
    rolls = toRolls . fill $ 4 `d` 6
    fill rolls = [[roll !! 0, roll !! 1, roll !! 2, roll !! 3, 0] | roll <- rolls]
