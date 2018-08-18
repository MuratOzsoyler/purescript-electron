module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Electron.BrowserWindow (BrowserWindowOption(..), WebPreference(..))
import Electron.Options (encodeOptions)
import Node.Process (PROCESS)
import Prelude (Unit)
import Test.Spec (it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: forall eff. Effect Unit
main = run [consoleReporter] do
  describe "encodeOptions :: BrowserWindowOptions -> Json" do
    it "can encode all options" do
      let options = [ BrowserWindowOption
                      { width: 640
                      , height: 480
                      , webPreferences: WebPreference
                        { overlayScrollbars: true
                        }
                      }
                    ]
      encodeOptions options `shouldEqual` (  "width"  := 640
                                          ~> "height" := 480
                                          ~> "webPreferences" := ( "overlayScrollbars" := true
                                                                 ~> jsonEmptyObject )
                                          ~> jsonEmptyObject )
