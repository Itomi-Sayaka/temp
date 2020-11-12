{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Bitset where

import           Data.Bits
import           Data.Coerce
import qualified Data.Foldable               as F
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.Exts

newtype BitSet = BitSet { getBitSet :: Int }
  deriving (Eq, Ord)

instance Show BitSet where
  showsPrec p xs = showParen (p > 10) $ showString "fromList " . shows (toList xs)

instance IsList BitSet where
  type Item BitSet = Int
  fromList = BitSet . F.foldl' (\acc x -> acc .|. unsafeShiftL 1 x) 0
  toList bs = filter (`memberBS` bs) [0..63]

emptyBS :: BitSet
emptyBS = BitSet 0

singletonBS :: Int -> BitSet
singletonBS (I# i#) = BitSet (I# (uncheckedIShiftL# 1# i#))

insertBS :: Int -> BitSet -> BitSet
insertBS (I# i#) (BitSet (I# bs#)) = BitSet (I# (uncheckedIShiftL# 1# i# `orI#` bs#))

deleteBS :: Int -> BitSet -> BitSet
deleteBS (I# i#) (BitSet (I# bs#)) = BitSet (I# (notI# (uncheckedIShiftL# 1# i#) `andI#` bs#))

memberBS :: Int -> BitSet -> Bool
memberBS (I# i#) (BitSet (I# bs#)) = isTrue# (uncheckedIShiftRL# bs# i# `andI#` 1#)

notMemberBS :: Int -> BitSet -> Bool
notMemberBS i = not . memberBS i

nullBS :: BitSet -> Bool
nullBS = (== 0) . coerce @BitSet @Int

sizeBS :: BitSet -> Int
sizeBS = coerce (popCount @Int)

isSubsetOf :: BitSet -> BitSet -> Bool
isSubsetOf x y = intersectionBS x y == x

unionBS :: BitSet -> BitSet -> BitSet
unionBS = coerce ((.|.) @Int)

complementBS :: BitSet -> BitSet
complementBS = coerce (complement @Int)

differenceBS :: BitSet -> BitSet -> BitSet
differenceBS x y = intersectionBS x (complementBS y)

intersectionBS :: BitSet -> BitSet -> BitSet
intersectionBS = coerce ((.&.) @Int)

findMinBS :: BitSet -> Int
findMinBS = coerce (countTrailingZeros @Int)

findMaxBS :: BitSet -> Int
findMaxBS = (63 -) . coerce (countLeadingZeros @Int)

deleteMinBS :: BitSet -> BitSet
deleteMinBS (BitSet x) = BitSet (x .&. (x - 1))

deleteMaxBS :: BitSet -> BitSet
deleteMaxBS x = deleteBS (findMaxBS x) x

deleteFindMin :: BitSet -> (Int, BitSet)
deleteFindMin x = (findMinBS x, deleteMinBS x)

deleteFindMax :: BitSet -> (Int, BitSet)
deleteFindMax x = let i = findMaxBS x in (i, deleteBS i x)

minView :: BitSet -> Maybe (Int, BitSet)
minView x
  | x /= BitSet 0 = Just $ deleteFindMin x
  | otherwise     = Nothing

maxView :: BitSet -> Maybe (Int, BitSet)
maxView x
  | x /= BitSet 0 = Just $ deleteFindMax x
  | otherwise     = Nothing

newtype instance VUM.MVector s BitSet = MV_BitSet (VUM.MVector s Int)
newtype instance VU.Vector BitSet = V_BitSet (VU.Vector Int)

instance VU.Unbox BitSet

instance VGM.MVector VUM.MVector BitSet where
  basicLength (MV_BitSet v) = VGM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_BitSet v) = MV_BitSet $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_BitSet v1) (MV_BitSet v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_BitSet `fmap` VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_BitSet v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_BitSet `fmap` VGM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_BitSet v) i = coerce `fmap` VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_BitSet v) i x = VGM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_BitSet v) = VGM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_BitSet v) x = VGM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_BitSet v1) (MV_BitSet v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_BitSet v1) (MV_BitSet v2) = VGM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_BitSet v) n = MV_BitSet `fmap` VGM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance VG.Vector VU.Vector BitSet where
  basicUnsafeFreeze (MV_BitSet v) = V_BitSet `fmap` VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_BitSet v) = MV_BitSet `fmap` VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_BitSet v) = VG.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_BitSet v) = V_BitSet $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_BitSet v) i = coerce `fmap` VG.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_BitSet mv) (V_BitSet v) = VG.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
