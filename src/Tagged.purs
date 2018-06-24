module Tagged where

import Data.Array as A
import Data.Map (Map)
import Data.Map as M

data Tagged a = Tagged String a

getName :: forall a. Tagged a -> String
getName (Tagged name _) = name

getValue :: forall a. Tagged a -> a
getValue (Tagged _ a) = a

taggedToMap :: forall a. Array (Tagged a) -> Map String a
taggedToMap =
  A.foldl (\acc (Tagged name a) -> M.insert name a acc) M.empty
