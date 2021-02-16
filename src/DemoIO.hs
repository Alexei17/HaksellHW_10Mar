module DemoIO where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

--------------
-- Data types.
--------------

-- Config for colors.
data ColorConfig = ColorConfig
  { color1 :: Color
  , color2 :: Color
  }

-- General application state.
data AppState = AppState
  { colors :: ColorConfig -- Colors config.
  , number :: Int -- Current number.
  , randomGen :: StdGen -- Random number generator.
  }

-------------
-- Constants.
-------------

-- Path to config file.
configPath :: FilePath
configPath = "config.txt"

-- Random numbers range.
range :: (Int, Int)
range = (-10, 10)

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = black

-- Simulation steps per second.
fps :: Int
fps = 60

-- Text shift on screen.
textShift :: Float
textShift = 250

------------------
-- Pure functions.
------------------

-- Draw a picture: two numbers of different colors defined in config.
drawApp :: AppState -> Picture
drawApp (AppState (ColorConfig c1 c2) n _) = Pictures [pic1, pic2]
  where
    txt = Text (show n)
    pic1 = Translate (-textShift) 0 $ Color c1 txt
    pic2 = Translate textShift 0 $ Color c2 txt

-- Handle events.
handleEvent :: Event -> AppState -> AppState
-- Increase number when UP is pressed.
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
  state { number = (number state) + 1 }
-- Decrease number when DOWN is pressed.
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
  state { number = (number state) - 1 }
-- Generate new random number when Space is pressed.
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state =
  -- Get new random number and generator.
  let (newn, newGen) = randomR range (randomGen state)
  -- Update BOTH number AND generator.
  in state { number = newn, randomGen = newGen }
-- Ignore all other events.
handleEvent _ state = state

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ x = x

-- Parse config from string.
-- Config format: 2 lines, one color per line.
parseConfig :: String -> Maybe ColorConfig
parseConfig str = case map findColor (lines str) of
  [Just c1, Just c2] -> Just (ColorConfig c1 c2)
  _ -> Nothing
  where
    findColor :: String -> Maybe Color
    findColor s = lookup s colorMap
    colorMap = zip names colors
    colors = [red, green, blue, white]
    names = ["red", "green", "blue", "white"]

------------------------------
-- Main function for this app.
------------------------------

-- Run game. This is the ONLY unpure function.
run :: IO ()
run = do
  -- Load config file contents (unpure action).
  str <- readFile configPath
  -- Try to parse config.
  case parseConfig str of
    Nothing -> putStrLn "Parse error"
    Just cfg -> do
      -- Get new random number generator (unpure action).
      gen <- newStdGen
      -- Run application.
      let initState = AppState cfg 0 gen
      play display bgColor fps initState drawApp handleEvent updateApp
