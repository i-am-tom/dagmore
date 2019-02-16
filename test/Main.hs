{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Hspec              (describe, hspec, it, shouldBe)
import Test.QuickCheck         (property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Test.Async  as A
import qualified Test.Energy as E
-- import qualified Test.Form   as F

main :: IO ()
main = do
  hspec do
    describe "Energy" do
      it "Calculates the energy" $ property \xs@(a, m, d) -> do
        E.mkEnergy (E.mkForce a m) d `shouldBe` E.main xs

    describe "Async" do
      it "Calculates the winner" $ property \xs@(a, b, c, d) -> monadicIO do
        let expected = A.winner (A.teamAScore a b) (A.teamBScore c d)

        actual <- run (A.main xs)
        assert (expected == actual)
