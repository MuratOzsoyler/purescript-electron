module Electron.Shell
  ( showItemInFolder
  , openItem
  , openExternal
  , moveItemToTrash
  , beep
  ) where

import Effect (Effect)
import Prelude (Unit)

-- | Show the given file in a file manager. If possible, select the file.
foreign import showItemInFolder :: String -> Effect Unit

-- | Open the given file in the desktop's default manner.
foreign import openItem :: String -> Effect Unit

-- | Open the given external protocol URL in the desktop's default manner.
-- | (For example, mailto: URLs in the user's default mail agent.)
foreign import openExternal :: String -> Effect Unit

-- | Move the given file to trash and returns a boolean status for the operation.
foreign import moveItemToTrash :: String -> Effect Boolean

-- | Play the beep sound.
foreign import beep :: Effect Unit
