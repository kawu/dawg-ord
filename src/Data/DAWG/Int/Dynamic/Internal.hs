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


-- | A directed acyclic word graph with phantom type `a`
-- representing the type of alphabet symbols.
-- Type `a` must probide an `Enum` instance.
--
-- A DAWG is, semantically, a map from keys (sequences of `a`s) to
-- integral values (see `Data.DAWG.Ord` for a more generic version of
-- DAWGs).
data DAWG a = DAWG
    { graph :: !(Graph N.Node)
    -- | Foot of the DAWG.
    , root  :: !ID }
    deriving (Show, Eq, Ord)

-- instance Binary (DAWG a) where
--     put d = do
--         put (graph d)
--         put (root d)
--     get = DAWG <$> get <*> get
