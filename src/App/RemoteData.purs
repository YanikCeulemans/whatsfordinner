module App.RemoteData where

import Prelude

import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)

data RemoteData e a
  = NotRequested
  | Loading
  | Error e
  | Success a

instance Functor (RemoteData e) where
  map f =
    case _ of
      NotRequested -> NotRequested
      Loading -> Loading
      Error e -> Error e
      Success a -> Success $ f a

instance Foldable (RemoteData e) where
  foldl f s =
    case _ of
      NotRequested -> s
      Loading -> s
      Error _e -> s
      Success a -> f s a
  foldr f s =
    case _ of
      NotRequested -> s
      Loading -> s
      Error _e -> s
      Success a -> f a s
  foldMap f =
    case _ of
      NotRequested -> mempty
      Loading -> mempty
      Error _e -> mempty
      Success a -> f a

instance Traversable (RemoteData e) where
  traverse f =
    case _ of
      NotRequested -> pure NotRequested
      Loading -> pure Loading
      Error e -> pure $ Error e
      Success a -> Success <$> f a

  sequence =
    case _ of
      NotRequested -> pure NotRequested
      Loading -> pure Loading
      Error e -> pure $ Error e
      Success a -> Success <$> a

instance Apply (RemoteData e) where
  apply fm m = case fm, m of
    NotRequested, _ -> NotRequested
    Loading, _ -> Loading
    _, NotRequested -> NotRequested
    _, Loading -> Loading
    Error e, _ -> Error e
    _, Error e -> Error e
    Success f, Success a -> Success $ f a

instance Bind (RemoteData e) where
  bind remoteData f = case remoteData of
    NotRequested -> NotRequested
    Loading -> Loading
    Error e -> Error e
    Success a -> f a

note :: forall e a. e -> Maybe a -> RemoteData e a
note err = case _ of
  Nothing -> Error err
  Just x -> Success x

