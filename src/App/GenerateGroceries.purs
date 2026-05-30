module App.GenerateGroceries where

import Prelude

import App.Data as Data
import App.Layout as Layout
import App.Shared as S
import Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , upsertGroceryList
  )
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Domain.GroceryList (GroceryList)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
  { groceryList :: GroceryList
  }

data Action = Initialize

component
  :: forall query input output m
   . MonadAff m
  => ManageGroceryList m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

  where
  initialState :: input -> State
  initialState _ =
    { groceryList: mempty
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { groceryList = groceryList }

  render :: State -> H.ComponentHTML Action () m
  render _state =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
        [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.h1_ [ HH.text "Generate groceries" ]
            , HH.div [ HP.class_ $ H.ClassName "flex spaced" ]
                [ S.link Route.Groceries [ HH.text "Cancel" ]
                ]
            ]
        ]
