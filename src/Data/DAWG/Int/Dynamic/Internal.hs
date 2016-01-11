-- | The module exports internal representation of dynamic DAWG.


module Data.DAWG.Int.Dynamic.Internal
(
-- * DAWG type
  DAWG (..)
) where


-- import Control.Applicative ((<$>), (<*>))
-- import Data.Binary (Binary, put, get)

import           Data.DAWG.Gen.Types
import           Data.DAWG.Gen.Graph (Graph)
import qualified Data.DAWG.Int.Dynamic.Node as N


-- | A directed acyclic word graph (DAWG), which can be seen as a map
-- from keys (sequences of 'Sym`'s) to values 'Val'.
-- See "Data.DAWG.Ord" for a more generic version of DAWGs.
data DAWG = DAWG
    { graph :: !(Graph N.Node)
    -- | The root (start state) of the DAWG.
    , root  :: !ID }
    deriving (Show, Eq, Ord)

-- instance Binary (DAWG a) where
--     put d = do
--         put (graph d)
--         put (root d)
--     get = DAWG <$> get <*> get
