module App.Layout where

import App.Route as Route
import App.Shared as Shared
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

main :: forall msg. Html msg -> Html msg
main view =
  HE.fragment
    [ HE.main [ HA.class' "container" ]
        [ view ]
    , HE.footer [ HA.class' "container" ]
        [ HE.nav [ HA.class' "flex spaced justify-center" ]
            [ Shared.link Route.Home [ HE.text "Next days" ]
            , Shared.link Route.Groceries [ HE.text "Groceries" ]
            ]
        ]
    ]

