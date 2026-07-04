module App.RemoteData where

data RemoteData e a
  = NotRequested
  | Loading
  | Error e
  | Success a
