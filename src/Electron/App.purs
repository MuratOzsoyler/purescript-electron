module Electron.App
  ( getAppPath
  , getPath
  , Path(..)
  , onReady
  , onAllWindowsClosed
  , quit
  ) where

import Effect (Effect)
import Prelude (Unit)

foreign import getAppPath :: Effect String

foreign import getPath :: Path -> Effect String

foreign import quit :: Effect Unit

data Path
  = Home
  | Documents
  | AppData

-- | Emitted when Electron has finished initialization.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/all/#event-39-ready-39)
foreign import onReady
  :: Effect Unit
  -> Effect Unit

foreign import onAllWindowsClosed
  :: Effect Unit
  -> Effect Unit
