module Five where

import Data.List
import Data.Semigroup
import Data.Monoid
import Control.Monad.State.Strict as S
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void (..))
import Data.Char
import Debug.Trace
import Control.Lens
import Data.Foldable
import qualified Data.Map.Strict as Map