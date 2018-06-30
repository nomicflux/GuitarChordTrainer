module Component.Scroll where

import Effect (Effect)
import Web.HTML.HTMLElement (HTMLElement)

type Offset = { top :: Number
              , left :: Number
              }

foreign import getScrollTop :: Effect Number

foreign import getScrollLeft :: Effect Number

foreign import getOffset :: HTMLElement -> Effect Offset
