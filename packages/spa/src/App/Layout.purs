module Spa.App.Layout where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Spa.App.Shared as Shared
import Spa.Data.Route as Route
import Spa.Domain.GroceryListId (GroceryListId)
import Spa.Domain.SpaceId (SpaceId)

type Routing =
  { spaceId :: SpaceId
  , groceryListId :: Maybe GroceryListId
  }

type MainConfig =
  { routing :: Maybe Routing
  }

defaultMainConfig :: MainConfig
defaultMainConfig =
  { routing: Nothing
  }

main :: forall w i. HH.HTML w i -> HH.HTML w i
main = main' defaultMainConfig

main' :: forall w i. MainConfig -> HH.HTML w i -> HH.HTML w i
main' config view =
  HH.div_
    [ HH.main [ HP.class_ $ HC.ClassName "container" ]
        [ view ]
    , case config.routing of
        Nothing -> HH.text ""
        Just routing ->
          HH.footer [ HP.class_ $ HC.ClassName "" ]
            [ HH.nav
                [ HP.class_ $ HC.ClassName
                    "flex spaced justify-center container"
                ]
                ( join
                    [ [ Shared.link Route.Home [ HH.text "Home" ]
                      , Shared.link
                          (Route.SpaceRoute routing.spaceId Route.Schedule)
                          [ HH.text "Schedule" ]
                      ]
                    , case routing.groceryListId of
                        Nothing -> []
                        Just groceryListId ->
                          [ Shared.link
                              ( Route.SpaceRoute routing.spaceId $
                                  Route.GroceriesRoute
                                    groceryListId
                                    Route.Groceries
                              )
                              [ HH.text "Groceries" ]
                          ]
                    ]
                )
            ]
    ]
