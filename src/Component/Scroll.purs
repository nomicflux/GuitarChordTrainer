module Component.Scroll where

import Effect (Effect)
import Prelude (Unit)

foreign import getScrollTop :: Effect Number

foreign import getScrollLeft :: Effect Number
