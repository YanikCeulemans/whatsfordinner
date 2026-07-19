module Spa.App.Data where

import Prelude

import Common.Amount as Amount
import Common.Extensions.ULID as ULIDExt
import Common.GroceryListId (GroceryListId)
import Common.Id as Id
import Common.Meal (Meal(..))
import Common.MealSchedule (MealSchedule(..))
import Common.MealScheduleId (MealScheduleId(..))
import Common.PlannedMeal (PlannedMeal(..))
import Common.RingList as RingList
import Data.Date (Date, Month(..))
import Data.Date as Date
import Data.Either as Either
import Data.Enum (toEnum)
import Data.Maybe as Maybe
import Partial.Unsafe (unsafeCrashWith)

theDate :: Date
theDate =
  Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid date literal")
    $ Date.canonicalDate
        <$> toEnum 2026
        <*> pure March
        <*> toEnum 2

dummyMealScheduleId :: MealScheduleId
dummyMealScheduleId =
  ULIDExt.parse "01KNW48VB0PNCFC0KZ8SW289ZZ"
    # Either.fromRight' crash
    # Id.MkId
    # MkMealScheduleId
  where
  crash _ = unsafeCrashWith "invalid hardcoded dummy list id ULID"

mealSchedule :: MealSchedule
mealSchedule =
  MkMealSchedule
    { id: dummyMealScheduleId
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
                    [ { name: "Chicory", amount: Amount.unitless 4.0 }
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
        , NoMealPlanned
        , PlannedMeal
            ( MkMeal
                { name: "Chipolata with carrot patato mash"
                , ingredients:
                    [ { name: "Chipolata", amount: Amount.withUnit 250.0 "g" }
                    , { name: "Potatoes", amount: Amount.withUnit 300.0 "g" }
                    , { name: "Carrots", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Onion", amount: Amount.unitless 1.0 }
                    , { name: "Bay leaves", amount: Amount.toTaste }
                    , { name: "Butter", amount: Amount.toTaste }
                    , { name: "Milk", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pasta with broccoli, leek and herb cheese"
                , ingredients:
                    [ { name: "Pasta", amount: Amount.withUnit 250.0 "g" }
                    , { name: "Broccoli", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Leek", amount: Amount.unitless 1.0 }
                    , { name: "Herb cheese", amount: Amount.withUnit 100.0 "g" }
                    , { name: "Onion", amount: Amount.unitless 1.0 }
                    , { name: "Vegetable stock"
                      , amount: Amount.withUnit 1.0 "cube"
                      }
                    , { name: "Olive oil", amount: Amount.withUnit 2.0 "tbsp" }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Veggie Snitzel with beans and potatoes"
                , ingredients:
                    [ { name: "Veggie schnitzel", amount: Amount.unitless 2.0 }
                    , { name: "Potatoes", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Green beans (frozen)"
                      , amount: Amount.withUnit 500.0 "g"
                      }
                    , { name: "Onion", amount: Amount.unitless 1.0 }
                    , { name: "Butter", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken wrap"
                , ingredients:
                    [ { name: "Veggie Chicken pieces"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Tortilla wraps", amount: Amount.unitless 4.0 }
                    , { name: "Lettuce", amount: Amount.unitless 1.0 }
                    , { name: "Tomato", amount: Amount.unitless 2.0 }
                    , { name: "Canned corn", amount: Amount.unitless 1.0 }
                    , { name: "Sour cream", amount: Amount.withUnit 100.0 "ml" }
                    , { name: "Guacamole", amount: Amount.withUnit 200.0 "g" }
                    , { name: "Butter", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Lasagne"
                , ingredients:
                    [ { name: "Prepared Lasagne", amount: Amount.unitless 1.0 }
                    ]
                }
            )
        , NoMealPlanned
        , PlannedMeal
            ( MkMeal
                { name: "Codloin with basilicum mash and vine tomatoes"
                , ingredients:
                    [ { name: "Cod loin", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Potatoes", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Vine tomatoes"
                      , amount: Amount.withUnit 200.0 "g"
                      }
                    , { name: "Fresh basil", amount: Amount.withUnit 20.0 "g" }
                    , { name: "Butter", amount: Amount.withUnit 25.0 "g" }
                    , { name: "Milk", amount: Amount.withUnit 100.0 "ml" }
                    , { name: "Pine nuts", amount: Amount.withUnit 50.0 "tbsp" }
                    , { name: "Garlic", amount: Amount.unitless 2.0 }
                    , { name: "Milk", amount: Amount.toTaste }
                    , { name: "Olive oil", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Orzo with olives and feta"
                , ingredients:
                    [ { name: "Orzo", amount: Amount.withUnit 150.0 "g" }
                    , { name: "Feta cheese", amount: Amount.withUnit 100.0 "g" }
                    , { name: "Olives"
                      , amount: Amount.withUnit 100.0 "g"
                      }
                    , { name: "Bell pepper", amount: Amount.unitless 1.0 }
                    , { name: "Eggplant", amount: Amount.unitless 1.0 }
                    , { name: "Garlic", amount: Amount.withUnit 1.0 "clove" }
                    , { name: "Shallot", amount: Amount.unitless 1.0 }
                    , { name: "Breadcrumbs", amount: Amount.withUnit 140.0 "g" }
                    , { name: "Olive oil", amount: Amount.toTaste }
                    , { name: "Mediterranean herbs", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Pasta with mushroom cream sauce"
                , ingredients:
                    [ { name: "Pasta", amount: Amount.withUnit 200.0 "g" }
                    , { name: "Mushrooms (frozen)"
                      , amount: Amount.withUnit 250.0 "g"
                      }
                    , { name: "Cooking cream"
                      , amount: Amount.withUnit 100.0 "ml"
                      }
                    , { name: "Flour"
                      , amount: Amount.withUnit 1.0 "tbsp"
                      }
                    , { name: "Vegetable stock"
                      , amount: Amount.withUnit 100.0 "ml"
                      }
                    , { name: "Onion", amount: Amount.unitless 1.0 }
                    , { name: "Olive oil", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Chicken, apple mash and fried potatoes"
                , ingredients:
                    [ { name: "Veggie chicken", amount: Amount.unitless 2.0 }
                    , { name: "Potatoes", amount: Amount.withUnit 600.0 "g" }
                    , { name: "Apple mash", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Butter", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Veggie burger with spinach mash"
                , ingredients:
                    [ { name: "Veggie burger"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Potatoes", amount: Amount.withUnit 400.0 "g" }
                    , { name: "Spinach with cream (frozen)"
                      , amount: Amount.withUnit 300.0 "g"
                      }
                    , { name: "Butter", amount: Amount.withUnit 40.0 "g" }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Sandwiches"
                , ingredients:
                    [ { name: "Artisanal bread", amount: Amount.unitless 2.0 }
                    , { name: "Sliced cheese"
                      , amount: Amount.withUnit 200.0 "g"
                      }
                    , { name: "Eggs", amount: Amount.unitless 2.0 }
                    , { name: "Tomatoes", amount: Amount.unitless 2.0 }
                    , { name: "Mayonaise", amount: Amount.toTaste }
                    , { name: "Pickles", amount: Amount.toTaste }
                    , { name: "Lettuce", amount: Amount.unitless 1.0 }
                    , { name: "Tomato", amount: Amount.unitless 2.0 }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Zucchini, minced meat and potato slices casserole"
                , ingredients:
                    [ { name: "Zucchini", amount: Amount.unitless 2.0 }
                    , { name: "Minced beef", amount: Amount.withUnit 500.0 "g" }
                    , { name: "Potatoes", amount: Amount.withUnit 500.0 "g" }
                    , { name: "Onion", amount: Amount.unitless 2.0 }
                    , { name: "Garlic", amount: Amount.unitless 3.0 }
                    , { name: "Tomato paste"
                      , amount: Amount.withUnit 2.0 "tbsp"
                      }
                    , { name: "Canned diced tomatoes"
                      , amount: Amount.withUnit 400.0 "g"
                      }
                    , { name: "Grated cheese"
                      , amount: Amount.withUnit 150.0 "g"
                      }
                    , { name: "Olive oil", amount: Amount.toTaste }
                    , { name: "Dried basil", amount: Amount.toTaste }
                    , { name: "Dried oregano", amount: Amount.toTaste }
                    , { name: "Paprika powder", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        , PlannedMeal
            ( MkMeal
                { name: "Salmon with pesto crust, greens and baby potatoes"
                , ingredients:
                    [ { name: "Salmon fillet"
                      , amount: Amount.unitless 2.0
                      }
                    , { name: "Baby potatoes"
                      , amount: Amount.withUnit 250.0 "g"
                      }
                    , { name: "Broad beans (frozen)"
                      , amount: Amount.withUnit 125.0 "g"
                      }
                    , { name: "Green beans (frozen)"
                      , amount: Amount.withUnit 250.0 "g"
                      }
                    , { name: "Pesto", amount: Amount.withUnit 80.0 "g" }
                    , { name: "Parmesan", amount: Amount.withUnit 1.5 "tbsp" }
                    , { name: "Breadcrumbs"
                      , amount: Amount.withUnit 1.5 "tbsp"
                      }
                    , { name: "Parmesan", amount: Amount.withUnit 1.5 "tbsp" }
                    , { name: "Olives", amount: Amount.withUnit 2.0 "tbsp" }
                    , { name: "Hazelnuts", amount: Amount.withUnit 1.0 "tbsp" }
                    , { name: "Olive oil", amount: Amount.toTaste }
                    , { name: "Salt", amount: Amount.toTaste }
                    , { name: "Pepper", amount: Amount.toTaste }
                    ]
                }
            )
        ]
    }

dummyListId :: GroceryListId
dummyListId =
  ULIDExt.parse "01KNW48VB0PNCFC0KZ8SW289ZV"
    # Either.fromRight' crash
    # Id.MkId
  where
  crash _ = unsafeCrashWith "invalid hardcoded dummy list id ULID"
