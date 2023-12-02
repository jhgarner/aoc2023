{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude (module Prelude) where

import BasePrelude as Prelude hiding (filter, span)
import Data.Coerce as Prelude
import Data.Foldable as Prelude
import Data.Functor as Prelude
import Data.Map.Strict as Prelude (Map, elems, keys)
import Data.Monoid as Prelude (Sum (..))
import Data.Semigroup as Prelude (Max (..))
import GHC.Exts as Prelude hiding (toList)
import Generics.Deriving as Prelude
import System.IO as Prelude
import Text.Trifecta as Prelude
import Witherable as Prelude

instance (a ~ ()) => IsString (Parser a) where
    fromString = void . symbol

nonemptyLines :: IO [String]
nonemptyLines = filter (not . null) . lines <$> getContents'

parse :: Parser a -> String -> a
parse parser = foldResult (error . show) id . parseString parser mempty

int :: Parser Int
int = fmap fromInteger integer

foldAs :: forall b a f. (Coercible a b, Coercible b a, Foldable f, Monoid b) => f a -> a
foldAs = coerce @b @a . foldMap' coerce

foldTo :: forall b a f. (Coercible a b, Foldable f, Monoid a) => f a -> b
foldTo = coerce . fold
