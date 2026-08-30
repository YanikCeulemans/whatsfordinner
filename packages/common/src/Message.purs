module Common.Message where

import Common.DevEx (todo)
import Common.GroceryListId (GroceryListId)
import Data.Argonaut (class EncodeJson)

data Message

instance EncodeJson Message where
  encodeJson x = todo "encode json message"

groceryListId :: Message -> GroceryListId
groceryListId _ = todo "grocery list id message"
