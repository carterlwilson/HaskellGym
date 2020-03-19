{-# LANGUAGE TemplateHaskell #-}

module GuiData where 

import qualified Data.Text as T
import Lens.Micro ((^.)
                 , (.~)
                 , (&))
import Lens.Micro.TH

data WorkoutInfo =
WorkoutInfo { _name      :: T.Text
            , _block        :: Int
            , _week         :: Int
            , _session      :: Int
            , _exerciseList :: [Exercise]
            , _exerciseHistory :: [ExerciseHistory]
            }
            deriving (Show)

makeLenses ''WorkoutInfo