{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Debug.Trace

-- import qualified Test.Async  as A
-- import qualified Test.Form   as F
import qualified Test.Energy as E

main :: IO ()
main = do
  hspec do
    describe "Energy" do
      it "Calculates the energy" $ property \xs@(a, m, d) -> do
        E.mkEnergy (E.mkForce a m) d `shouldBe` E.main xs

--     describe "Async" do
--       it "Calculates the winner" $ property \a b c d -> do
--         let teamA = Async.teamAScore a b
--             teamB = Async.teamBScore c d
-- 
--         unsafePerformIO (Async.main a b c d)
--           == Async.winner teamA teamB
