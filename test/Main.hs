{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Coerce      (coerce)
import Test.Hspec
import Test.QuickCheck
import System.IO.Unsafe (unsafePerformIO)

import qualified Test.Async  as Async
import qualified Test.Form   as Form
import qualified Test.Energy as Energy

main :: IO ()
main = do
  hspec do
    describe "Energy" do
      it "Calculates the energy" $ property \a m d -> do
        Energy.getEnergy (Energy.getForce m a) d
          `shouldBe` Energy.main (a, m, d)

    describe "Async" do
      it "Calculates the winner" $ property \a b c d -> do
        let teamA = Async.teamAScore a b
            teamB = Async.teamBScore c d

        unsafePerformIO (Async.main a b c d)
          == Async.winner teamA teamB
