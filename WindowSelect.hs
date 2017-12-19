module WindowSelect
  (
    currentWsWindows
  , windowPrompt
  , selectWindow
  , Win (..)
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Util.NamedWindows (getName)
  

currentWsWindows :: Eq a => W.StackSet i l a s sd -> [a]
currentWsWindows = W.integrate' . W.stack . W.workspace . W.current

newtype Win = Win String 
instance XPrompt Win where
  showXPrompt (Win _) = "select window: "

windowPrompt :: XPConfig -> ([(String, Window)] -> String -> X ()) -> X ()
windowPrompt conf job = do
  ss <- gets windowset
  let currentWindows = currentWsWindows ss -- :: [Window]
  winNames <- mapM (fmap (convertSpaces '_' . show) . getName) $ currentWindows --all window names in current workspace with spaces removed
  mkXPrompt (Win "") conf (mkComplFunFromList' $ winNames) (job $ zip winNames currentWindows )

selectWindow :: XPConfig -> X ()
selectWindow conf = windowPrompt conf job where --job takes a string (window name) and use focusWindow to focus
   job wList wName =
    case lookup wName wList of
      Nothing -> return ()
      Just win -> windows $ W.focusWindow win

convertSpaces :: Char -> String -> String
convertSpaces new = map (\c -> if c == ' ' then new else c)
