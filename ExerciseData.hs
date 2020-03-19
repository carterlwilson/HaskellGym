{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}


module ExerciseData where

import qualified Data.Text as T
import Lens.Micro ((^.)
                    , (.~)
                    , (&))
import Lens.Micro.TH
import Data.Char as C

data Exercise = 
    Exercise { _exerciseName  :: T.Text
            , _weight         :: Int
            , _sets           :: Int
            , _reps           :: Int}
            deriving (Show)

data ExerciseHistory = 
    ExerciseHistory {
        personname :: !String,
        ename :: !String,
        maxlift  :: !Int
    } deriving (Show)

data WorkoutInfo =
    WorkoutInfo { _name      :: T.Text
                , _block        :: Int
                , _week         :: Int
                , _exerciseList :: [Exercise]
                , _exerciseHistory :: [ExerciseHistory]
                }
                deriving (Show)

makeLenses ''WorkoutInfo

stringToLower :: String -> String
stringToLower s = map C.toLower s

--uses ExerciseHistory generated from csv row to build container for new exercise for the day for a specific person
buildExercise :: WorkoutInfo -> ExerciseHistory -> Maybe Exercise
buildExercise w h =
        if (stringToLower (T.unpack (w ^. name))) == (stringToLower (personname h)) then Just (Exercise (T.pack (ename h)) (((maxlift h) * (60 + ((w ^. block) * 10))) `div` 100) 5 (10 `div` (w ^. week)))
        else Nothing  

--uses a ExerciseHistoryList to build a list of Exercises
buildExerciseList :: [ExerciseHistory] -> WorkoutInfo -> [Maybe Exercise]
buildExerciseList h w = map (\x -> buildExercise w x) h

--adds an Exercise to the workout list if it isn't a Nothing value        
addToWorkoutInfo :: WorkoutInfo -> Maybe Exercise -> WorkoutInfo
addToWorkoutInfo w e = case e of 
                        Just val -> let elist = (w ^. exerciseList) ++ [val]
                                    in w & exerciseList .~ elist
                        Nothing -> w

--uses a fold to pick exercises from a list of Maybe values and add them to athe workout list. foldl rocks!
addToExerciseList :: WorkoutInfo -> [Maybe Exercise] -> WorkoutInfo
addToExerciseList w e = 
    foldl (addToWorkoutInfo) w e

--builds the final workoutInfo with the workout list
buildWorkoutInfo :: WorkoutInfo -> WorkoutInfo
buildWorkoutInfo w = addToExerciseList w (buildExerciseList (w ^. exerciseHistory) w)
