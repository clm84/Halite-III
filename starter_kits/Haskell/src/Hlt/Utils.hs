module Hlt.Utils where


import Control.Applicative (liftA2)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import System.IO


instance (Num a, Num b) => Num (a,b) where
  fromInteger x = (fromInteger x, fromInteger x)
  (a,b) + (a',b') = (a + a', b + b')
  (a,b) - (a',b') = (a - a', b - b')
  (a,b) * (a',b') = (a * a', b * b')
  negate (a,b)    = (negate a, negate b)
  abs (a,b)       = (abs a, abs b)
  signum (a,b)    = (signum a, signum b)

caseSign :: (Eq a, Num a) => a -> (b, b, b) -> b
caseSign x (a, b, c)
  | signum x == fromInteger 1 = a
  | signum x == fromInteger 0 = b
  | otherwise                 = c

norm1 :: Num a => (a, a) -> a
norm1 (a, b) = abs a + abs b

rotate :: Int -> [a] -> [a]
rotate n l = take (length l) . drop n . cycle $ l

toMap :: Ord b => (a -> b) -> [a] -> Map b a
toMap f = M.fromList . L.map (\e -> (f e, e))

cart :: [a] -> [b] -> [(a, b)]
cart = liftA2 (,)

flush :: String -> IO ()
flush = (>> hFlush stdout) . putStrLn
