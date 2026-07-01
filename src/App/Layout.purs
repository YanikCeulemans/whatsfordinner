module App.Layout where

import Prelude

import App.Data as Data
import App.Shared as Shared
import Data.Route as Route
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP

main :: forall w i. HH.HTML w i -> HH.HTML w i
main view =
  HH.div_
    [ HH.main [ HP.class_ $ HC.ClassName "container" ]
        [ view ]
    , HH.footer [ HP.class_ $ HC.ClassName "" ]
        [ HH.nav
            [ HP.class_ $ HC.ClassName "flex spaced justify-center container" ]
            [ Shared.link Route.Home [ HH.text "Home" ]
            , Shared.link Route.Schedule [ HH.text "Schedule" ]
            -- TODO: What is sensible navigation here?
            , Shared.link
                ( Route.GroceryListRoute
                    { groceryListId: Data.dummyListId
                    , groceryListRoute: Route.Groceries
                    }
                )
                [ HH.text "Groceries" ]
            ]
        ]
    ]

