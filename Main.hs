--{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import System.Random

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

-- do this without enum somehow? make list using Bounded [minBound..maxBound] then use length
instance Random Rank where
   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x,g') -> (toEnum x, g')
   random g = randomR (minBound, maxBound) g

-- avoid copy-paste?
instance Random Suit where
   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x,g') -> (toEnum x, g')
   random g = randomR (minBound, maxBound) g
   

-- 5 cards
--randHand = 

-- 2 cards
--randHole

main :: IO ()
main = do
   --print $ (minBound :: Rank)
   --print $ (maxBound :: Suit)
   --print $ (maxBound :: Card)
   --print $ sort hand
   gen <- getStdGen
   x <- getStdRandom random
   print (x :: Suit)
   where hand = [Card {rank = Ace, suit = Spades}, Card {rank = Queen, suit = Hearts}, Card {rank = Queen, suit = Spades}]
