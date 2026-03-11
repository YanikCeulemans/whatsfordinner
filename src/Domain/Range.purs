module Domain.Range
  ( Range
  , Direction(..)
  , create
  , toArray
  , start
  , end
  , diff
  , direction
  ) where

import Prelude

import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)

data Direction
  = Left
  | Right

derive instance Eq Direction

newtype Range a = MkRange { start :: a, end :: a }

instance Functor Range where
  map f (MkRange { start: s, end: e }) = MkRange { start: f s, end: f e }

instance Show (Range String) where
  show (MkRange { start: s, end: e }) = intercalate " "
    [ "start: ", s, " , end: ", e ]

create :: forall a. a -> a -> Range a
create s e = MkRange { start: s, end: e }

start :: forall a. Range a -> a
start (MkRange { start: s }) = s

end :: forall a. Range a -> a
end (MkRange { end: e }) = e

diff :: forall a. Ord a => Ring a => Range a -> a
diff (MkRange { start: s, end: e }) = abs $ e - s

direction :: forall a. Ord a => Range a -> Direction
direction (MkRange { start: s, end: e })
  | compare e s == LT = Left
  | otherwise = Right

toArray :: forall a. Eq a => (a -> a) -> Range a -> Array a
toArray successor (MkRange { start: s, end: e }) =
  unfoldr go s
  where
  go x
    | x == successor e = Nothing
    | otherwise =
        Just (x /\ successor x)
