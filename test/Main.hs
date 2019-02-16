{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Coerce             (coerce)
import Data.Either.Validation  (Validation (..))
import Test.Hspec              (describe, hspec, it, shouldBe)
import Test.QuickCheck         (property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Test.Async  as A
import qualified Test.Energy as E
import qualified Test.Form   as F

main :: IO ()
main = do
  hspec do
    describe "Energy" do
      it "Calculates the energy" $ property \xs@(a, m, d) -> do
        E.main xs `shouldBe` E.mkEnergy (E.mkForce a m) d

    describe "Async" do
      it "Calculates the winner" $ property \xs@(a, b, c, d) -> monadicIO do
        let expected = A.winner (A.teamAScore a b) (A.teamBScore c d)

        actual <- run (A.main xs)
        assert (expected == actual)

    describe "Form" do
      it "Sucessfully validates a (valid) user" do
        let forename :: F.Forename
            forename = coerce "Tom"

            surname :: Maybe F.Surname
            surname = Just (coerce "Harding")

            age :: F.Age
            age = coerce @Int 25

        F.main ("Tom", "Harding", "25") `shouldBe`
          Success (F.User forename surname age)

      it "Collects independent errors" do
        F.main ("", "Harding", "") `shouldBe`
          Failure ["Empty forename", "Couldn't parse age!"]

      it "'Binds' over dependent errors" do
        F.main ("Tom", "", "25") `shouldBe`
          Failure ["No surname!"]
