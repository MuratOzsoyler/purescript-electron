module Electron.BrowserWindow
  ( BrowserWindowOption(..)
  , WebPreference(..)
  , BrowserWindowOptions(..)
  , BrowserWindow()
  , newBrowserWindow
  , onClose
  , loadURL
  , WebContents(..)
  , webContents
  , openDevTools
  , DevToolOption(..)
  , DevToolOptions(..)
  , send
  , onDidFinishLoad
  , onNewWindow
  , onWillNavigate
  ) where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Electron.Event (Event)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (Unit, (>>>))

newtype BrowserWindowOption = BrowserWindowOption
  { width          :: Int
  , height         :: Int
  , minWidth       :: Int
  , minHeight      :: Int
  , useContentSize :: Boolean
  , webPreferences :: Array WebPreference
  }

derive instance newtypeBrowserWindowOption :: Newtype BrowserWindowOption _
derive instance genericBrowserWindowOption :: Generic BrowserWindowOption _
instance decodeBrowserWindowOption :: Decode BrowserWindowOption where
  decode = genericDecode defaultOptions{ unwrapSingleConstructors = true }
instance encodeBrowserWindowOption :: Encode BrowserWindowOption where
  encode = genericEncode defaultOptions{ unwrapSingleConstructors = true }

type BrowserWindowOptions = Array BrowserWindowOption

newtype WebPreference = WebPreference
  { zoomFactor                     :: Number
  , allowDisplayingInsecureContent :: Boolean
  , allowRunningInsecureContent    :: Boolean
  , overlayScrollbars              :: Boolean
  }

derive instance newtypeWebPreference :: Newtype WebPreference _
derive instance genericWebPreference :: Generic WebPreference _
instance decodeWebPreference :: Decode WebPreference where
  decode = genericDecode defaultOptions{ unwrapSingleConstructors = true }
instance encodeWebPreference :: Encode WebPreference where
  encode = genericEncode defaultOptions{ unwrapSingleConstructors = true }

foreign import data BrowserWindow :: Type

newBrowserWindow
  :: BrowserWindowOptions
  -> Effect BrowserWindow
newBrowserWindow = encode >>> newBrowserWindowImpl

foreign import newBrowserWindowImpl
  :: Foreign
  -> Effect BrowserWindow

foreign import loadURL
  :: BrowserWindow
  -> String
  -> Effect Unit

foreign import onClose
  :: BrowserWindow
  -> Effect Unit
  -> Effect Unit

foreign import data WebContents :: Type

foreign import webContents
  :: BrowserWindow
  -> Effect WebContents

-- | Opens the devtools.
-- |
-- | [Official Electron documentation](http://electron.atom.io/docs/all/#webcontents-opendevtools-options)
openDevTools
  :: WebContents
  -> DevToolOptions
  -> Effect Unit
openDevTools wc = encode >>> openDevToolsImpl wc

foreign import openDevToolsImpl
  :: WebContents
  -> Foreign
  -> Effect Unit

newtype DevToolOption = DevToolOption
  { detach :: Boolean
  }

derive instance newtypeDevToolOption :: Newtype DevToolOption _
derive instance genericDevToolOption :: Generic DevToolOption _
instance decodeDevToolOption :: Decode DevToolOption where
  decode = genericDecode defaultOptions{ unwrapSingleConstructors = true }
instance encodeDevToolOption :: Encode DevToolOption where
  encode = genericEncode defaultOptions{ unwrapSingleConstructors = true }

type DevToolOptions = Array DevToolOption

foreign import send :: forall a
   . WebContents
  -> String
  -> a
  -> Effect Unit

foreign import onDidFinishLoad
  :: WebContents
  -> Effect Unit
  -> Effect Unit

foreign import onNewWindow
  :: WebContents
  -> (Event -> String -> Effect Unit)
  -> Effect Unit

foreign import onWillNavigate
  :: WebContents
  -> (Event -> String -> Effect Unit)
  -> Effect Unit
