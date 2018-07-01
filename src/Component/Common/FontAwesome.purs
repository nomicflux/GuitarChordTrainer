module Component.Common.FontAwesome where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (($), (<>))

icon :: forall p i. String -> HH.HTML p i
icon name = HH.i [ HP.class_ $ HH.ClassName ("fas fa-" <> name) ] []
