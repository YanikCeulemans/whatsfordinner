module App.Layout where

import Prelude

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
    , HH.footer [ HP.class_ $ HC.ClassName "container" ]
        [ HH.nav [ HP.class_ $ HC.ClassName "flex spaced justify-center" ]
            [ Shared.link Route.Home [ HH.text "Next days" ]
            , Shared.link Route.Groceries [ HH.text "Groceries" ]
            ]
        ]
    ]

