{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Lens.Micro ((^.)
                 , (.~)
                 , (&))
import Lens.Micro.TH
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import qualified Graphics.Vty as V
import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C


data ExerciseHistory = 
        ExericseHistory {
            _exerciseName :: !String,
            _max          :: !Int
        }

data Exercise = 
        Exercise { _exerciseName  :: T.Text
               , _weight         :: Int
               , _sets           :: Int
               , _reps           :: Int}
               deriving (Show)

drawExercise :: Exercise -> Widget Name
drawExercise e = vBox [ txt (_exerciseName e)
                      , str (show (_weight e))
                      , str (show (_sets e))
                      , str (show (_reps e))]

drawExerciseList :: [Exercise] -> [Widget Name]
drawExerciseList l = map drawExercise l

data Name = NameField
          | BlockField
          | WeekField
          | SessionField
          deriving (Eq, Ord, Show)

data WorkoutInfo =
    WorkoutInfo { _name      :: T.Text
             , _block        :: Int
             , _week         :: Int
             , _session      :: Int
             , _exerciseList :: [Exercise]
             }
             deriving (Show)

makeLenses ''WorkoutInfo

buildExercise :: WorkoutInfo -> Exercise
buildExercise w = Exercise ("benchpress") ((200 * (60 + ((w ^. week) * 10))) `div` 100) (5) (10 `div` (w ^. week))

addExercise :: WorkoutInfo -> Exercise -> WorkoutInfo
addExercise f e = 
    let elist = (_exerciseList f) ++ [e]
    in f & exerciseList .~ elist

mkForm :: WorkoutInfo -> Form WorkoutInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Name" @@=
                   editTextField name NameField (Just 1)
               , label "Block" @@=
                   editShowableField block BlockField
               , label "Week" @@=
                   editShowableField week WeekField
               , label "Session" @@=
                   editShowableField session SessionField
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form WorkoutInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter (vBox body)]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        body = drawExerciseList (_exerciseList (formState f))

app :: App (Form WorkoutInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey V.KEnter []) -> continue $ mkForm (addExercise (formState s) (buildExercise (formState s)))
                _ -> do
                    s' <- handleFormEvent ev s
                    continue $ setFieldValid ((formState s')^.block > 0) BlockField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

initialExercise = Exercise {
    _exerciseName = "bench"
    , _weight = 200
    , _sets = 5
    , _reps = 3
}

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialWorkoutInfo = WorkoutInfo { _name = ""
                                   , _block= 0
                                   , _week = 0
                                   , _session = 0
                                   , _exerciseList = []
                                   }
        f = setFieldValid False BlockField $
            mkForm initialWorkoutInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    putStrLn "The starting form state was:"
    print initialWorkoutInfo

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
