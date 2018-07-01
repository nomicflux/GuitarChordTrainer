module Component.Common.Offset where

import Effect (Effect)
import Web.DOM.Element (Element)

type Offset = { top :: Number
              , left :: Number
              }

foreign import getScrollTop :: Effect Number

foreign import getScrollLeft :: Effect Number

foreign import getOffset :: Element -> Effect Offset
