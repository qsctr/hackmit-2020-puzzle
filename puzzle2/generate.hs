#!/usr/bin/env stack
-- stack --resolver lts-16.8 script --optimize --package "bytestring,JuicyPixels,unordered-containers,vector"

{-# LANGUAGE BlockArguments, ViewPatterns #-}

import Codec.Picture
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as VS

cifarFiles = map (("cifar-10-batches-bin/" ++) . (++ ".bin")) $
    "test_batch" : map (("data_batch_" ++) . show) [1..5]

main = do
    assignment <- readFile "assignment.txt"
    let coords = HM.fromList [ (read line, i)
            | (line, i) <- zip (lines assignment) [0..], any (/= ' ') line ]
        toMap bs = case BS.uncons bs of
            Nothing -> HM.empty
            Just (label, BS.splitAt 3072 -> (pixels, rest)) ->
                let modify = case label of
                        0 -> HM.insert pixels $ PixelRGB8 0 0 0
                        5 -> HM.insert pixels $ PixelRGB8 255 255 255
                        _ -> id
                in  modify $ toMap rest
    cifarMap <- HM.unions <$> traverse (fmap toMap . BS.readFile) cifarFiles
    writePng "output.png" =<< withImage 69 69 \x y -> do
        let i = coords HM.! (x, y)
        Right (ImageRGB8 img) <- readImage $ "images/" ++ show i ++ ".png"
        let getChannel c = BS.pack $
                map ((imageData img VS.!) . (+ c) . (* 3)) [0..1023]
        pure $ cifarMap HM.! BS.concat (map getChannel [0..2])
