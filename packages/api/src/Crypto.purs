module Api.Crypto (sha1) where

import Data.Function.Uncurried (Fn2, runFn2)
import Node.Encoding (Encoding)
import Node.Encoding as Encoding

foreign import _sha1Impl :: Fn2 String String String

sha1 :: Encoding -> String -> String
sha1 encoding input = runFn2 _sha1Impl nodeEncoding input
  where
  nodeEncoding = Encoding.encodingToNode encoding
