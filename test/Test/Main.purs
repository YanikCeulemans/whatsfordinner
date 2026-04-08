module Test.Main where

import Prelude

import Data.Date as Date
import Data.Date.Component (Month(..))
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Days(..))
import Data.ULID as DULID
import Domain.Meal (Meal(..))
import Domain.MealSchedule (Id(..), MealSchedule)
import Domain.MealSchedule as MealSchedule
import Domain.PlannedMeal (PlannedMeal(..))
import Domain.Range as Range
import Domain.RingList (RingList)
import Domain.RingList as RingList
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Simple.ULID as ULID
import Test.Domain.GroceryId as GroceryId
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
    describe "drop" do
      let
        sut = RingList.fromFoldable [ 1, 2 ]
      it "works given negative number" do
        RingList.drop (-1) sut `shouldEqual` sut
      it "works given zero" do
        RingList.drop 0 sut `shouldEqual` sut
      it "works for one" do
        RingList.drop 1 sut `shouldEqual` (RingList.fromFoldable [ 2 ])
      it "works for length of ring list" do
        RingList.drop (RingList.length sut) sut `shouldEqual` sut
      it "works for length of ring list plus 1" do
        RingList.drop (RingList.length sut + 1) sut `shouldEqual`
          (RingList.fromFoldable [ 2 ])

    describe "take" do
      let
        sut = RingList.fromFoldable [ 1, 2 ]
      it "works given negative number" do
        RingList.take (-1) sut `shouldEqual` mempty
      it "works given zero" do
        RingList.take 0 sut `shouldEqual` mempty
      it "works given 1" do
        RingList.take 1 sut `shouldEqual` (RingList.fromFoldable [ 1 ])
      it "works for length of ring list" do
        RingList.take (RingList.length sut) sut `shouldEqual` sut
      it "works for length of ring list plus 1" do
        RingList.take (RingList.length sut + 1) sut `shouldEqual`
          (RingList.fromFoldable [ 1, 2, 1 ])

    describe "offset" do
      let
        sut = RingList.fromFoldable [ 1, 2 ]
      it "works for offset -1" do
        RingList.offset (-1) sut `shouldEqual` (RingList.fromFoldable [ 2, 1 ])
      it "works for offset -2" do
        RingList.offset (-2) sut `shouldEqual` (RingList.fromFoldable [ 1, 2 ])
      it "works for offset 0" do
        RingList.offset 0 sut `shouldEqual` (RingList.fromFoldable [ 1, 2 ])
      it "works for offset 1" do
        RingList.offset 1 sut `shouldEqual` (RingList.fromFoldable [ 2, 1 ])
      it "works for offset 2" do
        RingList.offset 2 sut `shouldEqual` (RingList.fromFoldable [ 1, 2 ])

    describe "toList" do
      let
        sut = RingList.fromFoldable [ Just 1, Nothing, Just 2 ]
      it "toList should work for -1..-3" do
        RingList.toList (Range.create (-1) (-3)) sut `shouldEqual`
          (Nothing : Just 1 : Nil)
      it "toList should work for -1..1 and positive end" do
        RingList.toList (Range.create (-1) 1) sut `shouldEqual`
          (Just 2 : Just 1 : Nil)
      it "toList should work for 0..0 input" do
        RingList.toList (Range.create 0 0) sut `shouldEqual` Nil
      it "toList should work for 1..3 input" do
        RingList.toList (Range.create 1 3) sut `shouldEqual`
          (Nothing : Just 2 : Nil)
      it "toList should work for 0..1 input" do
        RingList.toList (Range.create 0 1) sut `shouldEqual` (Just 1 : Nil)
      it "toList should work for range larger than list input" do
        RingList.toList (Range.create 0 5) sut `shouldEqual`
          (Just 1 : Nothing : Just 2 : Just 1 : Nothing : Nil)
      it "toList should work for 2..5 input" do
        RingList.toList (Range.create 2 5) sut `shouldEqual`
          (Just 2 : Just 1 : Nothing : Nil)

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
        dateRange = Range.create startDate twoWeeksFromNow
      MealSchedule.toList dateRange theSchedule `shouldEqual` Nil
    it "toList should work for non-empty schedule" do
      let
        theSchedule = mkSchedule [ meal1, meal2 ]
        threeDaysFromNow = relativeDate 3.0 startDate
        dateRange = Range.create startDate threeDaysFromNow
      MealSchedule.toList dateRange theSchedule `shouldEqual`
        List.fromFoldable [ meal1, meal2, meal1 ]
    it "toList should work for same date as start date" do
      let
        theSchedule = mkSchedule [ meal1, meal2 ]
        dateRange = Range.create startDate $ relativeDate 1.0 startDate
      MealSchedule.toList dateRange theSchedule `shouldEqual`
        List.fromFoldable [ meal1 ]
  where
  startDate = unsafeMkDate 2026 March 2

  mkSchedule :: forall f. Foldable f => f PlannedMeal -> MealSchedule
  mkSchedule ps = MealSchedule.MkMealSchedule
    { id: MkId 1, schedule: RingList.fromFoldable ps, startDate }

rangeSpec :: Spec Unit
rangeSpec =
  describe "Range" do
    describe "toArray" do
      it "should work" do
        let
          sut = Range.create 1 3
        Range.toArray (_ + 1) sut `shouldEqual` [ 1, 2, 3 ]

ulidSpec :: Spec Unit
ulidSpec =
  describe "ULID" do
    describe "parse" do
      it "should work" do
        let
          ulid = "01BX5ZZKBKACTAV9WEVGEMMVRY"
          actual = DULID.parse ulid
        (actual <#> ULID.toString) `shouldEqual` Right ulid

spec :: Spec Unit
spec = do
  ringListSpec
  mealScheduleSpec
  rangeSpec
  ulidSpec
  GroceryId.spec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
