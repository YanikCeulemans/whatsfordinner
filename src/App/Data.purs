module App.Data where

import Prelude

import Data.Date (Date, Month(..))
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Maybe as Maybe
import Domain.Meal (Meal(..))
import Domain.MealSchedule (Id(..), MealSchedule(..))
import Domain.PlannedMeal (PlannedMeal(..))
import Domain.RingList as RingList
import Partial.Unsafe (unsafeCrashWith)

theDate :: Date
theDate =
  Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid date literal")
    $ Date.canonicalDate
        <$> toEnum 2026
        <*> pure March
        <*> toEnum 2

mealSchedule :: MealSchedule
mealSchedule =
  MkMealSchedule
    { id: MkId 1
    , startDate: theDate
    , schedule: RingList.fromFoldable
        [ PlannedMeal
            ( MkMeal
                { name: "Brussels sprouts with bacon and mashed potatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Mac & Cheese" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chili sin carne" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken wok with noodles" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Vol au vent" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Leek, mashed potatoes and minced meat casserole" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Fishsticks with broccoli and baby potatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Scampi diabolique with bread" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Cordon blue with carrots and potatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name:
                    "Veggie Snitzel with bell peppers, zucchini and baby potatoes"
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken with curry and pineapple" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pita" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicory ham rolls with mashed potatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pasta bolognese" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Sausage with carrot mash" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pasta with broccoli, leek and herbcheese" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Veggie Snitzel with beans and potatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken wrap" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Lasagne" }
            )
        , NoMealPlanned
        , PlannedMeal
            ( MkMeal
                { name: "Codloin with basilicum mash and vine tomatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Orzo with olives and feta" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pasta with mushroom cream sauce" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken, apple mash and fried potatoes" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Veggie burger with spinach mash" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Sandwiches" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Zucchini, minced meat and potato slices casserole" }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Salmon with pesto crust, greens and baby potatoes" }
            )
        ]
    }
