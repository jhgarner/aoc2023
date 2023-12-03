{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude (module Prelude) where

import BasePrelude as Prelude hiding (filter, span)
import Control.Applicative
import Control.Comonad as Prelude
import Control.Monad.State.Strict as Prelude
import Data.Coerce as Prelude
import Data.Foldable as Prelude
import Data.Functor as Prelude
import Data.Map.Strict qualified as M
import Data.Maybe as Prelude (isJust)
import Data.Monoid as Prelude (Sum (..))
import Data.Semigroup as Prelude (Last (..), Max (..))
import Data.Vector as Prelude (Vector)
import Debug.Trace as Prelude
import GHC.Exts as Prelude hiding (toList, traceEvent)
import Generics.Deriving as Prelude
import System.IO as Prelude
import Text.Trifecta (anyChar)
import Text.Trifecta as Prelude hiding (anyChar)
import Text.Trifecta.Delta
import Witherable as Prelude

traceShowIdWith :: (Show a, Show b) => a -> b -> b
traceShowIdWith a b = traceShow (a, b) b

deriving instance (Num n) => Num (Last n)

data Loc = Loc {x :: Int, y :: Int}
    deriving (Show, Generic, Ord, Eq)

instance Semigroup Loc where
    a <> b = Loc (a.x + b.x) (a.y + b.y)

instance Monoid Loc where
    mempty = Loc 0 0

newtype MyParser a = MyParser (StateT Int Parser a)
    deriving (Functor, Applicative, Monad, Parsing, DeltaParsing, CharParsing, Alternative, MonadPlus, MonadState Int)

instance TokenParsing MyParser where
    someSpace = skipMany ((newline *> modify' (+ 1)) <|> void space)

runMyParser :: MyParser a -> String -> a
runMyParser (MyParser parser) = parse $ evalStateT parser 0

getLoc :: MyParser Loc
getLoc = Loc <$> fmap (fromIntegral . column) position <*> get

instance (a ~ ()) => IsString (MyParser a) where
    fromString = void . symbol

nonemptyLines :: IO [String]
nonemptyLines = filter (not . null) . lines <$> getContents'

parse :: Parser a -> String -> a
parse parser = foldResult (error . show) id . parseString parser mempty

int :: (TokenParsing m, Integral i) => m i
int = fmap fromInteger integer

nat :: (TokenParsing m, Num i) => m i
nat = fmap fromInteger natural

anyChar :: (TokenParsing m, Coercible Char c) => m c
anyChar = coerce <$> token Text.Trifecta.anyChar

foldAs :: forall b a f. (Coercible a b, Coercible b a, Foldable f, Monoid b) => f a -> a
foldAs = coerce @b @a . foldMap' coerce

foldTo :: forall b a f. (Coercible a b, Foldable f, Monoid a) => f a -> b
foldTo = coerce . fold

foldFrom :: forall a b f. (Coercible a b, Foldable f, Monoid a) => f a -> b
foldFrom = coerce . fold

justWhen :: (a -> Bool) -> a -> Maybe a
justWhen p a = if p a then Just a else Nothing

newtype OnlyOne a = Only a
    deriving (Show, Functor, Num)

instance Semigroup (OnlyOne a) where
    _ <> _ = error "Attempt to merge values when only one was expected"

-- This Map is the exact same as the other except I override Semigroup (and
-- Monoid) to merge the values instead of throwing away the later one. You can
-- get back the default Map's Semigroup behavior with `Map k (First v)`.
newtype Map k v = Map (M.Map k v)
    deriving (Show, Generic, Functor, Foldable, Traversable, Filterable)

instance (Ord k, Semigroup v) => Semigroup (Map k v) where
    Map a <> Map b = Map $ M.unionWith (<>) a b

instance (Ord k, Semigroup v) => Monoid (Map k v) where
    mempty = Map mempty

mapOf :: k -> v -> Map k v
mapOf k = Map . M.singleton k

(!) :: (Ord k) => Map k v -> k -> v
Map m ! k = m M.! k

(!?) :: (Ord k) => Map k v -> k -> Maybe v
Map m !? k = m M.!? k

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map m) = Map $ M.mapWithKey f m
