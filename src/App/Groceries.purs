module App.Groceries (Model, Message, init, update, view) where

import Prelude

import App.Route (Route(..))
import App.Route as Route
import Data.Tuple.Nested ((/\))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

type Model = Unit

init :: Model
init = unit

data Message = NoOp

update :: F.Update Model Message
update model msg = model /\ []

view :: Model -> F.Html Message
view model =
  HE.fragment
    [ HE.main [ HA.class' "flex column container" ]
        [ HE.h1_ [ HE.text "Groceries" ]
        , HE.a [ HA.href $ Route.print $ GroceriesGenerate ]
            [ HE.text "Generate" ]
        ]
    , HE.footer [ HA.class' "container" ]
        [ HE.nav [ HA.class' "flex spaced justify-center" ]
            [ HE.a
                [ HA.href $ Route.print $ Route.Home ]
                [ HE.text "Next days" ]
            , HE.a
                [ HA.href $ Route.print $ Route.Groceries ]
                [ HE.text "Groceries" ]
            ]
        ]
    ]
