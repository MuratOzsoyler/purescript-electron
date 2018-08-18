module Electron.IpcRenderer
  ( on
  , once
  , removeListener
  , removeAllListeners
  , send
  , sendSync
  , sendToHost
  ) where

import Effect (Effect)
import Electron.Types (Channel, Listener)
import Prelude (Unit)

-- Listening for Messages


-- | Listens to channel, when a new message arrives the listener is called.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrendereronchannel-listener)
foreign import on :: Channel
                  -> Listener
                  -> Effect Unit

-- | Adds a one time listener function for the event.
-- | This listener is invoked only the next time a message is sent to channel,
-- | after which it is removed.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrendereroncechannel-listener)
foreign import once :: Channel
                    -> Listener
                    -> Effect Unit

-- | Removes the specified listener from the listener array for the
-- | specified channel.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrendererremovelistenerchannel-listener)
foreign import removeListener :: Channel
                              -> Listener
                              -> Effect  Unit

-- | Removes all listeners for the specified channel.
foreign import removeAllChannelListeners :: Channel
                                         -> Effect Unit

-- | Removes all listeners.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrendererremovealllistenerschannel)
foreign import removeAllListeners :: Effect Unit

-- | Send a message to the main process asynchronously via channel.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrenderersendchannel-arg1-arg2-)
foreign import send :: forall a .
                       Channel
                    -> a
                    -> Effect  Unit

-- | Send a message to the main process synchronously via channel.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrenderersendsyncchannel-arg1-arg2-)
foreign import sendSync :: forall a .
                           Channel
                        -> a
                        -> Effect  Unit

-- | Like ipcRenderer.send but the event will be sent to the <webview> element in the host page instead of the main process.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/api/ipc-renderer/#ipcrenderersendtohostchannel-arg1-arg2-)
foreign import sendToHost :: forall a .
                             Channel
                          -> a
                          -> Effect  Unit
