module App.Data where

import Prelude

import Data.Date (Date, Month(..))
import Data.Date as Date
import Data.Either as Either
import Data.Enum (toEnum)
import Data.Maybe as Maybe
import Data.ULID as DULID
import Domain.Amount as Amount
import Domain.GroceryListId (GroceryListId)
import Domain.Id as Id
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
                { name: "Brussels sprouts with bacon and mashed potatoes"
                , ingredients:
                    [ { name: "Brussels sprouts"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Bacon"
                      , amount: Amount.withUnit 125.0 "g"
                      }
                    , { name: "Butter"
                      , amount: Amount.withUnit 0.5 "tbsp"
                      }
                    , { name: "Grated cheese"
                      , amount: Amount.withUnit 100.0 "g"
                      }
                    , { name: "Salt"
                      , amount: Amount.toTaste
                      }
                    , { name: "Pepper"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Mac & Cheese"
                , ingredients:
                    [ { name: "Macaroni"
                      , amount: Amount.withUnit 250.0 "g"
                      }
                    , { name: "Flour"
                      , amount: Amount.withUnit 32.0 "g"
                      }
                    , { name: "Butter"
                      , amount: Amount.withUnit 32.0 "g"
                      }
                    , { name: "Chicken bouillon cube"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Milk"
                      , amount: Amount.withUnit 475.0 "ml"
                      }
                    , { name: "Bacon cubes"
                      , amount: Amount.withUnit 100.0 "g"
                      }
                    , { name: "Shredded cheddar cheese"
                      , amount: Amount.withUnit 1.0 "cup"
                      }
                    , { name: "Pepper"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chili sin carne"
                , ingredients:
                    [ { name: "Bell pepper", amount: Amount.unitless 2.0 }
                    , { name: "Garlic", amount: Amount.unitless 1.0 }
                    , { name: "clove Onion", amount: Amount.unitless 1.0 }
                    , { name: "Butter", amount: Amount.withUnit 1.0 "tbsp" }
                    , { name: "Vegetable broth"
                      , amount: Amount.withUnit 150.0 "ml"
                      }
                    , { name: "Kidney beans"
                      , amount: Amount.withUnit 240.0 "g"
                      }
                    , { name: "Tomato paste"
                      , amount: Amount.withUnit 140.0 "g"
                      }
                    , { name: "Rice (basmati)"
                      , amount: Amount.withUnit 75.0 "g"
                      }
                    , { name: "Corn", amount: Amount.withUnit 140.0 "g" }
                    , { name: "Tortilla (whole wheat, medium)"
                      , amount: Amount.withUnit 8.0 ""
                      }
                    , { name: "Pepper (harisa if you like spicey)"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken wok with noodles"
                , ingredients:
                    [ { name: "Pre-cut wok veggies"
                      , amount: Amount.withUnit 1.0 "bag"
                      }
                    , { name: "Veggie chicken pieces"
                      , amount: Amount.withUnit 200.0 "g"
                      }
                    , { name: "Noodles"
                      , amount: Amount.withUnit 200.0 "g"
                      }
                    , { name: "Olive oil"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Vol au vent"
                , ingredients:
                    [ { name: "Vol au vent"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Prebaked fries"
                      , amount: Amount.withUnit 1.0 "kg"
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Leek, mashed potatoes and minced meat casserole"
                , ingredients:
                    [ { name: "Veggie minced meat"
                      , amount: Amount.withUnit 250.0 "g"
                      }
                    , { name: "Potatoes"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Frozen leek with cream"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Salt"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Fishsticks with broccoli and baby potatoes"
                , ingredients:
                    [ { name: "Broccoli"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Baby potatoes"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Fishsticks"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Salt"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Scampi diabolique with bread"
                , ingredients:
                    [ { name: "Scampi"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Canned cubed tomatoes"
                      , amount: Amount.withUnit 200.0 "g"
                      }
                    , { name: "Onion"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Garlic"
                      , amount: Amount.withUnit 2.0 "clove"
                      }
                    , { name: "Artisanal baguette"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Butter"
                      , amount: Amount.toTaste
                      }
                    , { name: "Pepper"
                      , amount: Amount.toTaste
                      }
                    , { name: "Salt"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Cordon blue with carrots and potatoes"
                , ingredients:
                    [ { name: "Carrots"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Veggie Cordon blue"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Cordon blue"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Potatoes"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Butter"
                      , amount: Amount.toTaste
                      }
                    , { name: "Pepper"
                      , amount: Amount.toTaste
                      }
                    , { name: "Salt"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name:
                    "Veggie Snitzel with bell peppers, zucchini and baby potatoes"
                , ingredients:
                    [ { name: "Veggie Snitzel"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Bell pepper"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Zucchini"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Baby potatoes"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Garlic"
                      , amount: Amount.withUnit 1.0 "clove"
                      }
                    , { name: "Baby potatoes"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Butter"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken with curry and pineapple"
                , ingredients:
                    [ { name: "Veggie Chicken"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Curry sauce"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Rice"
                      , amount: Amount.withUnit 125.0 "g"
                      }
                    , { name: "Canned pineapple slices"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Butter"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pita"
                , ingredients:
                    [ { name: "Veggie Pita"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Pita buns"
                      , amount: Amount.unitless 4.0
                      }
                    , { name: "Pita sauce"
                      , amount: Amount.unitless 1.0
                      }
                    , { name: "Lettuce"
                      , amount: Amount.withUnit 125.0 "g"
                      }
                    , { name: "Tomatoes"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Butter"
                      , amount: Amount.toTaste
                      }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicory ham rolls with mashed potatoes"
                , ingredients:
                    [ { name: "Endive", amount: Amount.unitless 4.0 }
                    , { name: "Cooked ham"
                      , amount: Amount.unitless 4.0
                      }
                    , { name: "Butter", amount: Amount.withUnit 32.0 "g" }
                    , { name: "Flour", amount: Amount.withUnit 32.0 "g" }
                    , { name: "Milk"
                      , amount: Amount.withUnit 475.0 "ml"
                      }
                    , { name: "Emmentaler cheese"
                      , amount: Amount.withUnit 200.0 "g"
                      }
                    , { name: "Potatoes", amount: Amount.withUnit 1.0 "kg" }
                    , { name: "Butter", amount: Amount.toTaste }
                    , { name: "Nutmeg", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Emmentaler cheese", amount: Amount.toTaste }
                    , { name: "Butter", amount: Amount.toTaste }
                    , { name: "Milk", amount: Amount.toTaste }
                    ]
                }
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

dummyListId :: GroceryListId
dummyListId =
  DULID.parse "01KNW48VB0PNCFC0KZ8SW289ZV"
    # Either.fromRight' crash
    # Id.MkId
  where
  crash _ = unsafeCrashWith "invalid hardcoded dummy list id ULID"

