module FFI.URL (URL, mk, pathname, search) where

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

foreign import data URL :: Type

foreign import _mkImpl :: forall a. Fn3 (String -> a) (URL -> a) String a

mk :: String -> Either String URL
mk = runFn3 _mkImpl Left Right

-- | Returns the pathname of the url with leading '/'
foreign import pathname :: URL -> String

-- | Returns the search part of the URL with leading '?'
foreign import search :: URL -> String
