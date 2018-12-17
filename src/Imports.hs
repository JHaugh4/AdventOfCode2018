module Imports where

import Data.List
import Data.Monoid
import Control.Monad.State.Strict
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void (..))
import Debug.Trace
import Control.Lens

type Parser = Parsec Void Text.Text