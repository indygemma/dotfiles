module Topics (
    promptedGoto
  , promptedShift
  , myTopicNames
)where

import XMonad
import XMonad.Actions.TopicSpace (Topic, TopicConfig(..), currentTopicDir, switchTopic)
import XMonad.Actions.GridSelect (defaultGSConfig, GSConfig(..), gridselect)
import qualified XMonad.StackSet as W

import Common (runTerminal, myFont)

import Control.Arrow ((&&&))
import qualified Data.Map as M

data TopicItem = TI { topicName   :: Topic
                    , topicDir    :: String
                    , topicAction :: X ()
                    }

myTopics :: [TopicItem]
myTopics = [ TI "main"   ""        (return ())
           , TI "web"    ""        (spawn "google-chrome")
           , TI "email"  ""        (spawn $ runTerminal "mutt" $ Just "mutt")
           , TI "blog"   ""        (spawn "google-chrome --new-window https://stackedit.io/editor")
           , TI "xmonad" ".xmonad" (spawnInTopicDir "emacs xmonad.hs")
           ]

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs          = M.fromList $ map (topicName &&& topicDir) myTopics
  , defaultTopicAction = const $ return ()
  , defaultTopic       = "main"
  , maxTopicHistory    = 10
  , topicActions       = M.fromList $ map (topicName &&& topicAction) myTopics
  }

spawnInTopicDir action = currentTopicDir myTopicConfig >>= spawnIn action
spawnIn action dir     = spawn $ "cd " ++ dir ++ "; " ++ action

crizer :: String -> Bool -> X (String, String)
crizer _ False = return ("#002b36", "#839496")
crizer _ True  = return ("#839496", "#002b36")

gsConfig = defaultGSConfig {
           gs_colorizer = crizer
         , gs_font = myFont
}

wsgrid = withWindowSet $ \w -> do
  let wss = W.workspaces w
      usedNames = map W.tag $ wss
      newNames = filter (\used -> (show used `notElem` (map show myTopicNames))) usedNames
  gridselect gsConfig (map (\x -> (x,x)) $ myTopicNames ++ newNames)

promptedGoto  = wsgrid >>= flip whenJust (switchTopic myTopicConfig)
promptedShift = wsgrid >>= flip whenJust (windows . W.shift)
