-- MATHFUN
-- Haskell assignment program
-- UP885013

import Text.Printf
import Data.List

-- Types
type LatLong = (Float, Float)
data Place = Place
  { name :: String
  , latLong :: LatLong
  , rainfall :: [Int]
  } deriving (Show)

testData :: [Place]
testData = [Place "London"  (51.5, -0.1)  [0, 0, 5, 8, 8, 0, 0],
  Place  "Cardiff"  (51.5, -3.2)  [12, 8, 15, 0, 0, 0, 2],
  Place  "Norwich"  (52.6, 1.3)  [0, 6, 5, 0, 0, 0, 3],
  Place  "Birmingham"  (52.5, -1.9)  [0, 2, 10, 7, 8, 2, 2],
  Place  "Liverpool"  (53.4, -3.0)  [8, 16, 20, 3, 4, 9, 2],
  Place  "Hull"  (53.8, -0.3)  [0, 6, 5, 0, 0, 0, 4],
  Place  "Newcastle"  (55.0, -1.6)  [0, 0, 8, 3, 6, 7, 5],
  Place  "Belfast"  (54.6, -5.9)  [10, 18, 14, 0, 6, 5, 2],
  Place  "Glasgow"  (55.9, -4.3)  [7, 5, 3, 0, 6, 5, 0],
  Place  "Plymouth"  (50.4, -4.1)  [4, 9, 0, 0, 0, 6, 5],
  Place  "Aberdeen"  (57.1, -2.1)  [0, 0, 6, 5, 8, 2, 0],
  Place  "Stornoway"  (58.2, -6.4)  [15, 6, 15, 0, 0, 4, 2],
  Place  "Lerwick"  (60.2, -1.1)  [8, 10, 5, 5, 0, 0, 3],
  Place  "St Helier"  (49.2, -2.1)  [0, 0, 0, 0, 6, 10, 0]]


-- All functional code
placeNames :: [Place] -> String
placeNames [] = []
placeNames (Place name _ _ : places)
    | length places == 0 = name
    | otherwise = name ++ "\n" ++ placeNames places

placeByName :: String -> [Place] -> Place
placeByName name places = head (filter (\(Place n _ _) -> n == name) places)

averageRainfallByName :: String -> [Place] -> Float
averageRainfallByName name places = average (rainfall (placeByName name places))

placeToString :: Place -> String
placeToString (Place name (lat, long) rainfall) = name ++ "  " ++ show lat ++ "  "  ++ show long ++ "  " ++ listToString rainfall

placesToString :: [Place] -> String
placesToString [] = []
placesToString (place : places)
    | length places == 0 = placeToString place
    | otherwise = placeToString place ++ "\n" ++ placesToString places

placesRainfallToString :: [Place] -> String
placesRainfallToString [] = []
placesRainfallToString (Place name _ rainfall : places)
    | length places == 0 = name ++ "  " ++ listToString rainfall
    | otherwise = name ++ "  " ++ listToString rainfall ++ "\n" ++ placesRainfallToString places

dryPlacesByDay :: Int -> [Place] -> [Place]
dryPlacesByDay day places = filter (\(Place _ _ rainfall) -> rainfall !! (day - 1) == 0) places

updateRainfall :: Place -> Int -> Place
updateRainfall (Place name (lat, long) rainfall) x = Place name (lat, long) ([x] ++ (deleteNthElement (length rainfall - 1) rainfall))

updateRainfalls :: [Int] -> [Place] -> [Place]
updateRainfalls [] [] = []
updateRainfalls (x : xs) (place : places) = [updateRainfall place x] ++ (updateRainfalls xs places)

updatePlaceByName :: Place -> String -> [Place] -> [Place]
updatePlaceByName _ _ [] = []
updatePlaceByName x find (Place name (lat, long) rainfall : places)
    | name == find = [x] ++ (updatePlaceByName x find places)
    | otherwise = [Place name (lat, long) rainfall] ++ (updatePlaceByName x find places)

findDistances :: LatLong -> [Place] -> [(Place, Float)]
findDistances latLong places = auxFindDistance latLong places []

auxFindDistance :: LatLong -> [Place] -> [(Place, Float)] -> [(Place, Float)]
auxFindDistance _ [] dists = dists
auxFindDistance (lat, long) (place : places) dists = auxFindDistance (lat, long) places ((distPlace, distFloat) : dists) 
  where (distPlace, distFloat) = (place, distance (lat, long) (latLong place))

findClosestPlace :: LatLong -> [Place] -> (Place, Float)
findClosestPlace (lat, long) places = head (sortBy sortByDistance relativeDistances)
  where relativeDistances = (findDistances (lat, long) places)

-- Helper functions
average :: [Int] -> Float
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

listToString :: [Int] -> String
listToString [] = []
listToString (x : xs) = show x ++ "  " ++ listToString xs

deleteNthElement :: Int -> [a] -> [a]
deleteNthElement i (a : as)
    | i == 0 = as
    | otherwise = a : deleteNthElement (i - 1) as

distance :: LatLong -> LatLong -> Float
distance (x, y) (x1, y1) = pythagoreanTheorem (x - x1) (y - y1)

pythagoreanTheorem :: Float -> Float -> Float
pythagoreanTheorem x y = sqrt (x * x + y * y)

sortByDistance (a1, b1) (a2, b2)
  | b1 > b2 = GT
  | b1 <= b2 = LT

--  Demo
demo :: Int -> IO ()
demo 1 = putStrLn (placeNames testData)
demo 2 = putStrLn (printf "%3.2f" (averageRainfallByName "Cardiff" testData))
demo 3 = putStrLn (placesRainfallToString testData)

demo 4 = putStrLn (placeNames dryPlaces)
  where dryPlaces = dryPlacesByDay 2 testData

demo 5 = putStrLn (placesToString (updateRainfalls rainfallValues testData))
  where rainfallValues = [0, 8, 0, 0, 5, 0, 0, 3, 4, 2, 0, 8, 0, 0]

demo 6 = putStrLn (placesToString (updatePlaceByName newPlace "Plymouth" testData))
  where newPlace = Place "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1]

demo 7 = putStrLn (placeToString (fst closestPlace))
  where dryPlaces = dryPlacesByDay 1 testData
        closestPlace = findClosestPlace (50.9, -1.3) dryPlaces

-- demo 8 = -- display the rainfall map



-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your rainfall map code goes here
--



-- User interface (and loading/saving)
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Please enter a option below:"
  
  putStrLn " 1 - List all places."
  putStrLn " 2 - Show average rainfall for Cardiff."
  putStrLn " 3 - List all places and rainfall values as columns."
  putStrLn " 4 - List all places that were dry 2 days ago."
  putStrLn " 5 - Add new rainfall values to all places."
  putStrLn " 6 - Replace Plymouth with Portsmouth."
  putStrLn " 7 - Find closest place to 50.9 N, -1.3 E."
  putStrLn ""

  putStr "Input: "
  option <- getLine

  putStrLn ""
  case option of
    "1" -> demo 1
    "2" -> demo 2
    "3" -> demo 3
    "4" -> demo 4
    "5" -> demo 5
    "6" -> demo 6
    "7" -> demo 7
    _ -> putStrLn "Invalid option, please try again."

  putStrLn ""
  putStr "Press Enter to return to menu."
  _ <- getLine

  main
