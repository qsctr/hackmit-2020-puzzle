#!/usr/bin/env stack
-- stack --resolver lts-16.8 script --package base

{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor
import Data.Maybe

main = do
    keymap <- map (bimap (read @Int) (head . tail) . break (== ':')) . lines <$> readFile "keymap.txt"
    putStrLn . map (fromJust . flip lookup keymap . read) . lines =<< getContents
