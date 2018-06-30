module Component.Cookie where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Effect (Effect)
import Prelude (map, (<<<))

foreign import setCookieImpl :: Fn2 String String (Effect Unit)

setCookie :: String -> String -> Effect Unit
setCookie = runFn2 setCookieImpl

foreign import getCookieImpl :: String -> Effect (Nullable String)

getCookie :: String -> Effect (Maybe String)
getCookie = map toMaybe <<< getCookieImpl

foreign import removeCookieImpl :: String -> Effect Unit

removeCookie :: String -> Effect Unit
removeCookie = removeCookieImpl
