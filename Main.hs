{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List
import Control.Monad
import System.Random
import GHC.Enum

-- should these derive Enum as well?
data Suit =
   Clubs    |
   Diamonds |
   Hearts   |
   Spades
   deriving (Show, Eq, Ord, Bounded, Enum)

data Rank =
   Two   |
   Three |
   Four  |
   Five  |
   Six   |
   Seven |
   Eight |
   Nine  |
   Ten   |
   Jack  |
   Queen |
   King  |
   Ace
   deriving (Show, Eq, Ord, Bounded, Enum)

data Card = Card {
   rank :: Rank,
   suit :: Suit
} deriving (Show, Eq, Ord, Bounded)

instance Random Rank where
   -- 3 ways to write this:
   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
   --               = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
   --               = (toEnum x, g) where (x, g') = randomR (fromEnum a, fromEnum b) g
   --               = let (x, g') = randomR (fromEnum a, fromEnum b) g in (toEnum x, g')
   random g = randomR (minBound, maxBound) g

instance Random Suit where
   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
   random g = randomR (minBound, maxBound) g

instance Random Card where
   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
   random g = randomR (minBound, maxBound) g

instance Enum Card where
   fromEnum Card {..} = (fromEnum rank)*4 + fromEnum suit
   toEnum i = Card {rank = toEnum q, suit = toEnum r} where (q, r) = divMod i 4


randHand :: StdGen -> IO [Card]
randHand gen = do
   forM [1..5] $ \x -> do
      card <- getStdRandom random
      return card

-- TODO next: use State monad to deal from a deck

main :: IO ()
main = do
   gen <- getStdGen
   hand <- randHand gen
   print hand
   print $ sort hand
   --print $ boundedEnumFrom (minBound :: Card)
   -- quicker way to express Card types? use Read typeclass for 4H AS 1S JC etc.
   --maybe have derived Show instance to show shorthand using type qual. as well
   --where hand = [Card {rank = Ace, suit = Spades}, Card {rank = Queen, suit = Hearts}, Card {rank = Queen, suit = Spades}]
