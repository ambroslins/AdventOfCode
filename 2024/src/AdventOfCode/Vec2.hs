module AdventOfCode.Vec2 (Vec2 (..), scale) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Vec2 a = Vec2 {x :: !a, y :: !a}
  deriving (Eq, Show, Functor, Generic)

instance (Hashable a) => Hashable (Vec2 a)

scale :: (Num a) => a -> Vec2 a -> Vec2 a
scale s = fmap (* s)
{-# INLINE scale #-}

instance Applicative Vec2 where
  pure a = Vec2 a a
  {-# INLINE pure #-}
  Vec2 fx fy <*> Vec2 x y = Vec2 (fx x) (fy y)
  {-# INLINE (<*>) #-}

instance (Num a) => Num (Vec2 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}
