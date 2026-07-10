module App.Layout where

import Prelude

import App.Shared as Shared
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Domain.SpaceId (SpaceId)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP

type MainConfig =
  { spaceId :: Maybe SpaceId
  }

defaultMainConfig :: MainConfig
defaultMainConfig =
  { spaceId: Nothing
  }

main :: forall w i. HH.HTML w i -> HH.HTML w i
main = main' defaultMainConfig

main' :: forall w i. MainConfig -> HH.HTML w i -> HH.HTML w i
main' config view =
  HH.div_
    [ HH.main [ HP.class_ $ HC.ClassName "container" ]
        [ view ]
    , case config.spaceId of
        Nothing -> HH.text ""
        Just spaceId ->
          HH.footer [ HP.class_ $ HC.ClassName "" ]
            [ HH.nav
                [ HP.class_ $ HC.ClassName
                    "flex spaced justify-center container"
                ]
                [ Shared.link Route.Home [ HH.text "Home" ]
                , Shared.link
                    (Route.SpaceRoute spaceId Route.Schedule)
                    [ HH.text "Schedule" ]
                , Shared.link
                    (Route.SpaceRoute spaceId Route.Groceries)
                    [ HH.text "Groceries" ]
                ]
            ]
    ]

