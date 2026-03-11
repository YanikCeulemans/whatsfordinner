module Domain.RingList
  ( RingList
  , fromFoldable
  , asList
  , toList
  , drop
  , offset
  , take
  , length
  , reverse
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List as List
import Data.Number as Number
import Data.Ord as Ord
import Debug as Debug
import Domain.Range (Range)
import Domain.Range as Range

newtype RingList a = MkRingList (List a)

instance Semigroup (RingList a) where
  append (MkRingList xs) (MkRingList ys) = MkRingList (append xs ys)

instance Monoid (RingList a) where
  mempty = MkRingList mempty

instance Show a => Show (RingList a) where
  show (MkRingList xs) = show xs

instance Eq a => Eq (RingList a) where
  eq (MkRingList xs) (MkRingList ys) = xs `eq` ys

fromFoldable :: forall f. Foldable f => f ~> RingList
fromFoldable = MkRingList <<< List.fromFoldable

asList :: forall a. RingList a -> List a
asList (MkRingList xs) = xs

length :: forall a. RingList a -> Int
length (MkRingList xs) = List.length xs

drop :: forall a. Int -> RingList a -> RingList a
drop n rl@(MkRingList xs) =
  case xs of
    Nil -> MkRingList Nil
    _ -> MkRingList $ List.drop safeN xs
  where
  safeN = max 0 n `mod` length rl

offset :: forall a. Int -> RingList a -> RingList a
offset n rl@(MkRingList xs) =
  case xs of
    Nil -> rl
    _ -> MkRingList $ List.take (length rl)
      ((List.drop (n `mod` length rl) xs) <> xs)

take :: forall a. Int -> RingList a -> RingList a
take n rl@(MkRingList xs)
  | length rl == 0 = rl
  | length rl < n = take n $ rl <> rl
  | otherwise = MkRingList $ List.take n xs

reverse :: forall a. RingList a -> RingList a
reverse (MkRingList xs) = MkRingList $ List.reverse xs

toList :: forall a. Range Int -> RingList a -> List a
toList range rs
  | Range.direction range == Range.Left = toList (map negate range) $ reverse rs
  | otherwise =
      offset start rs # take rangeDiff # asList
      where
      start = Range.start range
      end = Range.end range
      rangeDiff = Range.diff range

