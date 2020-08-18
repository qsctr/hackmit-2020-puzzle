{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Monad
import           Data.Foldable
import           Data.List
import qualified Data.Sequence        as S
import qualified Data.Vector.Storable as V
import           Numeric.Natural
import           System.Environment

data State = State
    { program       :: V.Vector Char
    , students      :: Int
    , scores        :: S.Seq Natural
    , pc            :: Int
    , pc_dir        :: Int
    , step          :: Natural
    , unused_breaks :: [Int]
    , used_breaks   :: [Int]
    , cur_reg       :: Int
    , prev_reg      :: Maybe Int
    , reg_dir       :: Int
    , steps_per_reg :: S.Seq Natural
    , cur_reg_steps :: Natural }

main = do
    [read -> students] <- getArgs
    forever do
        program <- getLine
        let (scores, err) = run State
                { program = V.fromList program
                , students
                , scores = S.replicate students 0
                , pc = 0
                , pc_dir = 1
                , step = 0
                , unused_breaks = []
                , used_breaks = []
                , cur_reg = 0
                , prev_reg = Nothing
                , reg_dir = 1
                , steps_per_reg = S.replicate students 1
                , cur_reg_steps = 0 }
            scoresOutput = "scores: " ++ show (toList scores)
        case err of
            Nothing -> putStrLn scoresOutput
            Just e -> putStr $ unlines ["\x1b[91m" ++ scoresOutput, e ++ "\x1b[0m"]

data ExecResult = Step State | Pass | Error String | Debug

run s@State {..} = case program V.!? pc of
    Nothing -> (scores, Nothing)
    Just c -> case exec c of
        Step s' -> run $ next $ tick s'
        Pass -> run $ tick s
        Error e -> (scores, Just e)
        Debug -> (scores, Just $ intercalate "\n"
            [ "pc = " ++ show pc
            , "pc_dir = " ++ show pc_dir
            , "step = " ++ show step
            , "unused_breaks = " ++ show (reverse $ toList unused_breaks)
            , "used_breaks = " ++ show (toList used_breaks)
            , "cur_reg = " ++ show cur_reg
            , "prev_reg = " ++ maybe "none" show prev_reg
            , "reg_dir = " ++ show reg_dir
            , "steps_per_reg = " ++ show (toList steps_per_reg)
            , "cur_reg_steps = " ++ show cur_reg_steps ])
  where exec 'y' = case prev_reg of
            Nothing -> Error "cannot execute yes because no previous register"
            Just pr -> Step s { scores = S.adjust' (+ scores `S.index` pr) cur_reg scores }
        exec 'n' = Debug
        exec 'f' = case steps_per_reg `S.index` cur_reg of
            1 -> Error $ "register " ++ show cur_reg ++ " cannot go any faster"
            _ -> adjustSpr pred
        exec 's' = adjustSpr succ
        exec '+' = adjustScores succ
        exec '-' = adjustScores pred
        exec 'c' = Step s { reg_dir = -reg_dir }
        exec 'b'
            | pc `elem` used_breaks = Pass
            | otherwise = Step s { unused_breaks = pc : unused_breaks }
        exec 'a' = Step case unused_breaks of
            [] -> s
            pc' : uub' -> s { pc = pc', unused_breaks = uub', used_breaks = pc' : used_breaks }
        exec c = Error $ "invalid symbol " ++ show c ++ " at position " ++ show pc
        adjustSpr f = Step s { steps_per_reg = S.adjust' f cur_reg steps_per_reg }
        adjustScores f = Step s { scores = S.adjust' f cur_reg scores }

tick s@State {..} = s { pc = pc + pc_dir }

next s@State {..}
    | crs' >= steps_per_reg `S.index` cur_reg = s' { prev_reg = Just cur_reg,
        cur_reg = (cur_reg + reg_dir) `mod` students, cur_reg_steps = 0 }
    | otherwise = s' { cur_reg_steps = crs' }
  where s' = s { step = succ step }
        crs' = succ cur_reg_steps
