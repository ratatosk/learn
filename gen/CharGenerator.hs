module Main where

import Data.List (tails, intercalate)

height :: Int
height = 64

width :: Int
width = 64

sW :: String
sW = show width

sH :: String
sH = show height

mx :: Int
mx = width - 1

my :: Int
my = height - 1


-- generate distortion by specifying new corner positions
bilinear :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
bilinear ulx uly llx lly urx ury lrx lry  = 
  "-distort BilinearForward '" ++
  "0,0 " ++ show ulx ++ "," ++ show uly ++ " " ++
  "0," ++ mH ++ " " ++ show llx ++ "," ++ show lly ++ " " ++
  mW ++ ",0 " ++ show urx ++ "," ++ show ury ++ " " ++
  mW ++ "," ++ mH ++ " " ++ show lrx ++ "," ++ show lry ++ "'"
  where 
    mW = show my
    mH = show mx
    
  
-- squish right/left/top/bottom
blRight :: String
blRight = bilinear 0 0 0 my mx (height `div` 3) mx (my - height `div` 3)
                    
blLeft :: String
blLeft = bilinear 0 (height `div` 3) 0 (my - height `div` 3) mx 0 mx my

blTop :: String
blTop  = bilinear (width `div` 3) 0 0 my (mx - width `div` 3) 0 mx my

blBottom :: String
blBottom = bilinear 0 0 (width `div` 3) my mx 0 (mx - width `div`3) my

rot :: Int -> String
rot a = "-distort SRT '" ++ show a ++ "'"

-- in percents
type Point = (Int, Int)

mapCoords :: Point -> Point
mapCoords (x, y) = ((x * width) `div` 100, (y * height) `div` 100) 

printCoords :: Point -> String
printCoords (x, y) = show x ++ "," ++ show y

-- coords are specified in percentage of width or height
polyline :: [Point] -> String
polyline pts = "-fill none -strokewidth 2 -stroke black -draw 'polyline " ++
               intercalate " " (map (printCoords.mapCoords) pts) ++ "'"

lines1 :: [Point]
lines1 = [(0,0), (100, 100), (50,0), (0, 100)]

lines2 :: [Point]
lines2 = [(0,50), (100, 50), (66,0), (33, 100)]

lines3 :: [Point]
lines3 = [(0, 0), (100, 20), (0, 40), (100, 60), (0, 80), (100, 100)]

lines4 :: [Point]
lines4 = [(0, 0), (50, 50), (75, 25), (75,75), (0, 75)]

mirror :: Point -> Point
mirror (x, y) = (100 - x, y)

rot1 :: Point -> Point
rot1 (x, y) = (100 - y, x)

rot2 :: Point -> Point
rot2 (x, y) = (100 - x, 100 - y)

rot3 :: Point -> Point
rot3 (x, y) = (y, 100 -x)

lineVariants :: [Point] -> [[Point]]
lineVariants a = r ++ (map (map mirror) r)
  where r = [a, map rot1 a, map rot2 a, map rot3 a]
        
genpolys :: [Point] -> [String]
genpolys d = map polyline (lineVariants d)

-- grouped distortions
distGroups :: [[String]]
distGroups = [ genpolys lines1
             , genpolys lines2
             , [blRight, blLeft, blTop, blBottom] -- bilinear forward distortions
             , [rot 20, rot (-20), rot 40, rot (-40)] -- rotations 
             , genpolys lines3
             , genpolys lines4
             ]
              
chars :: [Char]
chars = ['0'] --, '1', '2', '3', '4', '5', '6', '7', '8', '9']

fontsizes :: [Int]
fontsizes = [80, 70]

fonts :: [String]
fonts = ["Arial-Normal", "Helvetica", "Verdana-Normal"]

-- of all kinds of distortions pick two kinds, pick one from each kind and combine

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']
                    
prod :: [[a]] -> [[a]]
prod [] = [[]]
prod (xs:xss) = [ y:ys | y <- xs, ys <- prod xss]

kinds :: [[[String]]]
kinds = combinations 2 distGroups

distortions :: [String]
distortions = map (intercalate " ") $ concat $ map prod kinds

mkLine :: Char -> String -> Int -> String -> Int -> String
mkLine c fnt siz dist n = "convert -size " ++ sW ++ "x" ++ sH ++ " -colorspace Gray -gravity Center -font " ++ fnt ++ 
                          " -pointsize " ++ show siz ++ " label:" ++ [c] ++ " " ++ dist ++ [' ', c, '_'] ++ show n ++ ".tif"

allcmds :: [String]
allcmds = [mkLine c fnt siz dist n | ((fnt, siz, dist), n) <- numberedVariants, c <- chars]
  where numberedVariants = zip [(fnt, siz, dist) | fnt <- fonts, siz <- fontsizes, dist <- distortions] [1..]

main :: IO ()
main = do
  putStrLn "#!/bin/sh"
  mapM_ putStrLn allcmds