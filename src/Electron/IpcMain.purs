module Electron.IpcMain
  ( on
  , once
  , removeListener
  , removeAllListeners
  , removeAllChannelListeners
  , sender
  ) where

import Effect (Effect)
import Electron.BrowserWindow (WebContents)
import Electron.Event (Event)
import Electron.Types (Channel, Listener)
import Prelude (Unit)

-- Listening for Messages

-- | Listens to channel, when a new message arrives the listener is called.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-main/#ipcmainonchannel-listener)
foreign import on :: Channel
                  -> Listener
                  -> Effect Unit

-- | Adds a one time listener function for the event.
-- | This listener is invoked only the next time a message is sent to channel,
-- | after which it is removed.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-main/#ipcmainoncechannel-listener)
foreign import once :: Channel
                    -> Listener
                    -> Effect Unit

-- | Removes the specified listener from the listener array for the
-- | specified channel.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-main/#ipcmainremovelistenerchannel-listener)
foreign import removeListener :: Listener
                              -> Effect Unit

-- | Removes all listeners for the specified channel.
foreign import removeAllChannelListeners :: Channel
                                         -> Effect Unit

-- | Removes all listeners.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-main/#ipcmainremovealllistenerschannel)
foreign import removeAllListeners :: Effect Unit


-- Event object

foreign import sender :: Event -> Effect WebContents
