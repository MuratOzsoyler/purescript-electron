module Electron.Types
  ( Listener
  , Channel
  , mkListener
  ) where

import Effect (Effect)
import Electron.Event (Event)
import Prelude (Unit)

-- | A boxed function that can be used as an event listener. This is necessary
-- | due to the underling implementation of Eff functions.
foreign import data Listener :: Type

-- | Creates a Listener from a normal PureScript Eff function.
foreign import mkListener
  :: forall m
   . (Event -> m -> Effect Unit)
  -> Listener

type Channel = String
