{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
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
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Lens.Micro ((^.)
                    , (.~)
                    , (&))
import ExerciseData

instance FromNamedRecord ExerciseHistory where
    parseNamedRecord r = ExerciseHistory 
        <$> r .: "personname" 
        <*> r .: "ename" 
        <*> r .: "maxlift"

--loads CSV data into ByteString format
loadMyData = BL.readFile "maxes.csv"

--decodes bytestring data into vectors, then into list of type ExerciseHistory 
readCsv :: BL.ByteString -> [ExerciseHistory]
readCsv  csvData = do
    case decodeByName csvData of
        Left err -> [ExerciseHistory "error" err 0 ]
        Right (_, v) -> V.toList $ v

--instructions for how to render an Exercise
drawExercise :: Exercise -> Widget FieldType
drawExercise e = vBox [ C.hCenter (txt (_exerciseName e))
                      , C.hCenter (str ("Weight: " ++ show (_weight e)))
                      , C.hCenter (str ("Sets: " ++ show (_sets e)))
                      , C.hCenter (str ("Reps: " ++ show (_reps e)))]

--instructions for how to render a list of exercises
drawExerciseList :: [Exercise] -> [Widget FieldType]
drawExerciseList l = map drawExercise l

--data type necessary to build a Form data type
data FieldType = NameField
          | BlockField
          | WeekField
          | SessionField
          deriving (Eq, Ord, Show)

--use Brick library to build a form 
mkForm :: WorkoutInfo -> Form WorkoutInfo e FieldType
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 25 $ str s <+> fill ' ') <+> w
    in newForm [ label "Name" @@=
                   editTextField name NameField (Just 1)
               , label "Block (pos. num. <5)" @@=
                   editShowableField block BlockField
               , label "Week (pos. num. <5)" @@=
                   editShowableField week WeekField
               ]

--sets attributes for form fields
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

--instructions for how to render the entire form
--the result body is included as a part of the form and is initiated as empty
draw :: Form WorkoutInfo e FieldType -> [Widget FieldType]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter (hBox body)]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        body = drawExerciseList (_exerciseList (formState f))

--sets up draw function for the app, and the event handlers
app :: App (Form WorkoutInfo e FieldType) e FieldType
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                --if all the fields are valid, this will first empty out existing results, and then build a new form with the new results
                --note: by results, I mean the exercises generating by the input
                VtyEvent (V.EvKey V.KEnter []) -> if (allFieldsValid s == True) then continue $ mkForm (buildWorkoutInfo ((formState s) & exerciseList .~ []))
                                                  else continue s
                --handles form events, uses function composition to validate all numeric fields
                _ -> do
                    s' <- handleFormEvent ev s
                    continue $ setFieldValid (((formState s')^.week > 0) && ((formState s')^.week < 5)) WeekField 
                             $ setFieldValid (((formState s')^.block > 0) && ((formState s')^.block < 5)) BlockField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    csvData <- loadMyData
    --build Vty interface, dependency for Brick
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
    
        initialWorkoutInfo = WorkoutInfo { _name = ""
                                   , _block= 0
                                   , _week = 0
                                   , _exerciseList = []
                                   , _exerciseHistory = readCsv csvData
                                   }
        f = setFieldValid False BlockField $
            mkForm initialWorkoutInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    --some logging for debugging
    putStrLn "The starting form state was:"
    print initialWorkoutInfo

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
