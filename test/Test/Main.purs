module Test.Main where

import Prelude

import Data.Date as Date
import Data.Date.Component (Month(..))
import Data.Enum (toEnum)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Days(..))
import Domain.Meal (Meal(..))
import Domain.MealSchedule (Id(..), MealSchedule)
import Domain.MealSchedule as MealSchedule
import Domain.PlannedMeal (PlannedMeal(..))
import Domain.RingList (RingList)
import Domain.RingList as RingList
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

meal1 :: PlannedMeal
meal1 = PlannedMeal (MkMeal { name: "Brussels sprouts" })

meal2 :: PlannedMeal
meal2 = NoMealPlanned

meal3 :: PlannedMeal
meal3 = PlannedMeal (MkMeal { name: "Mac & Cheese" })

meals :: RingList PlannedMeal
meals = RingList.fromFoldable [ meal1, meal2, meal3 ]

ringListSpec :: Spec Unit
ringListSpec =
  describe "RingList" do
    let
      sut = RingList.fromFoldable [ Just 1, Nothing, Just 2 ]
    it "toListWithRange should work for negative input" do
      RingList.toListWithRange (-1) sut `shouldEqual` Nil
    it "toListWithRange should work for 0 input" do
      RingList.toListWithRange 0 sut `shouldEqual` Nil
    it "toListWithRange should work for 1 input" do
      RingList.toListWithRange 1 sut `shouldEqual` (Just 1 : Nil)
    it "toListWithRange should work for larger than list input" do
      RingList.toListWithRange 5 sut `shouldEqual`
        (Just 1 : Nothing : Just 2 : Just 1 : Nothing : Nil)
    it "toListWithRange should work for larger than list input" do
      RingList.toListWithRange 4 meals `shouldEqual`
        (meal1 : meal2 : meal3 : meal1 : Nil)

mkDate :: Int -> Month -> Int -> Maybe Date.Date
mkDate year month day =
  Date.canonicalDate
    <$> toEnum year
    <*> pure month
    <*> toEnum day

unsafeMkDate :: Int -> Month -> Int -> Date.Date
unsafeMkDate year month day =
  mkDate year month day # Maybe.fromMaybe'
    (\_ -> unsafeCrashWith "invalid date params")

relativeDate :: Number -> Date.Date -> Date.Date
relativeDate n startDate = Date.adjust (Days n) startDate # Maybe.fromMaybe'
  (\_ -> unsafeCrashWith "invalid relative days amount")

mealScheduleSpec :: Spec Unit
mealScheduleSpec = do
  describe "mealSchedule" do
    it "toList should work for empty schedule" do
      let
        theSchedule = mkSchedule []
        twoWeeksFromNow = relativeDate 14.0 startDate
      MealSchedule.toList twoWeeksFromNow theSchedule `shouldEqual` Nil
    it "toList should work for non-empty schedule" do
      let
        theSchedule = mkSchedule [ meal1, meal2 ]
        threeDaysFromNow = relativeDate 3.0 startDate
      MealSchedule.toList threeDaysFromNow theSchedule `shouldEqual`
        List.fromFoldable [ meal1, meal2, meal1, meal2 ]
    it "toList should work for same date as start date" do
      let
        theSchedule = mkSchedule [ meal1, meal2 ]
      MealSchedule.toList startDate theSchedule `shouldEqual`
        List.fromFoldable [ meal1 ]
  where
  startDate = unsafeMkDate 2026 March 2

  mkSchedule :: forall f. Foldable f => f PlannedMeal -> MealSchedule
  mkSchedule ps = MealSchedule.MkMealSchedule
    { id: MkId 1, schedule: RingList.fromFoldable ps, startDate }

spec :: Spec Unit
spec = do
  ringListSpec
  mealScheduleSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
