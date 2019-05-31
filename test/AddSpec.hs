module AddSpec where

import Control.Lens ((^.), view)
import Data.Bool
import Data.Either
import Data.List
import Instances ()
import Plan.Env
import Plan.Event
import Plan.Functions
import Plan.Task
import Test.Hspec
import Test.QuickCheck
import Text.Regex.TDFA

data EventTime = EventTime
  { etHours :: Int
  , etMins :: Int
  } deriving (Eq)

instance Show EventTime where
  show (EventTime h m) = f h <> ":" <> f m
    where
      f x =
        if x `elem` [0 .. 9]
          then "0" <> show x
          else show x

instance Arbitrary EventTime where
  arbitrary = do
    h <- elements [0 .. 99]
    m <- elements [0 .. 99]
    return $ EventTime h m

spec :: SpecWith ()
spec = do
  describe "Adding tasks" $ do
    it "increases number of tasks by 1" $
      property $ \(t, c, s) -> do
        (Right _, c') <- runMonads' (addTask Nothing t) c $ Env c s
        length (c' ^. tasks) `shouldBe` length (c ^. tasks) + 1
    it "does nothing if task is later removed" $
      property $ \(t, c, s) -> do
        (Right _, c') <- runMonads' (addTask Nothing t) c $ Env c s
        (Right _, c'') <-
          runMonads'
            (removeItem $
             fromIntegral $ maximum $ fmap (view identifier) $ c' ^. tasks)
            c' $
          Env c' s
        c `shouldBe` c''
    it "results in no duplicate IDs" $
      property $ \(t, c, s) -> do
        (Right _, c') <- runMonads' (addTask Nothing t) c $ Env c s
        let ids = fmap (view identifier) $ c' ^. tasks
        length (nub ids) `shouldBe` length ids
  describe "Adding events" $ do
    it "succeeds if valid times are given, fails for invalid times" $
      property $ \(s, e, c, n, d, r, sit) -> do
        (result, _) <-
          runMonads'
            (addEvent $
             OptEvent n d (show (s :: EventTime)) (show (e :: EventTime)) r)
            c $
          Env c sit
        let f (EventTime h m) = h `elem` [0 .. 23] && m `elem` [0 .. 59]
        result `shouldSatisfy` bool isLeft isRight (f s && f e)
    it "fails for random invalid strings" $
      property $ \(s, e, c, n, d, r, sit) -> do
        (result, _) <- runMonads' (addEvent $ OptEvent n d s e r) c $ Env c sit
        shouldSatisfy result $
          bool isLeft isRight $
          fromEither result =~ "^([0-1][0-9]|2[0-3]):[0-5][0-9]$"

fromEither :: Either p p -> p
fromEither (Right r) = r
fromEither (Left l) = l
