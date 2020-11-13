{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module FWT where

import           Control.Monad
import           Data.Bits
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

test :: IO ()
test = do
  let
    xs = VU.fromList ([4, 7, 11, 54, 23] :: [Int])
    ys = VU.fromList ([45, 23, 14, 38, 21, 45] :: [Int])
  print $ convolute xs ys AND
  print $ convolute xs ys OR
  print $ convolute xs ys XOR

data Kinds = AND | OR | XOR
  deriving Eq

convolute :: VU.Vector Int -> VU.Vector Int -> Kinds -> VU.Vector Int
convolute xs ys k
  | k == AND = convoluteAND xs ys
  | k == OR  = convoluteOR xs ys
  | otherwise = convoluteXOR xs ys

convoluteAND :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
convoluteAND xs ys =
  let
    xs' = fwt xs False True False
    ys' = fwt ys False True False
    zs' = VU.zipWith (*) xs' ys'
  in
    VU.take m $ fwt zs' True True False
  where
    !m = min (VU.length xs) (VU.length ys)

convoluteOR :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
convoluteOR xs ys =
  let
    xs' = fwt xs False False False
    ys' = fwt ys False False False
    zs' = VU.zipWith (*) xs' ys'
  in
    fwt zs' True False False

walshSpectre :: VU.Vector Int ->　VU.Vector Int
walshSpectre vec = fwt vec False False True

convoluteXOR :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
convoluteXOR xs ys =
  let
    xs' = fwt xs False False True
    ys' = fwt ys False False True
    zs' = VU.zipWith (*) xs' ys'
  in
    fwt zs' True False True

-- 高速アダマール変換
-- inv: 順変換ならFalse 逆変換ならTrue
-- isAND: and もしくは or による畳み込み AndならTrue OrならFalse
-- isXOR: xor畳み込みならTrue
fwt :: VU.Vector Int -> Bool -> Bool -> Bool -> VU.Vector Int
fwt f' inv isAND isXOR = VU.create $ do
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    if isXOR
      then do
        itemX <- VUM.unsafeRead g j
        itemY <- VUM.unsafeRead g (j .|. i)
        if inv
          then do
            VUM.unsafeWrite g j ((itemX + itemY) `div` 2)
            VUM.unsafeWrite g (j .|. i) ((itemX - itemY) `div` 2)
          else do
            VUM.unsafeWrite g j (itemX + itemY)
            VUM.unsafeWrite g (j .|. i) (itemX - itemY)
      else do
        if isAND
          then do
            item <- VUM.unsafeRead g (j .|. i)
            if inv
              then VUM.unsafeModify g (subtract item) j
              else VUM.unsafeModify g (+ item) j
          else do
            item <- VUM.unsafeRead g j
            if inv
              then VUM.unsafeModify g (subtract item) (j .|. i)
              else VUM.unsafeModify g (+ item) (j .|. i)
  return g
  where
    !f = growVU f'
    !n = VU.length f

growVU :: VU.Vector Int -> VU.Vector Int
growVU v
  | VU.null v = VU.singleton 0
  | VU.length v == 1 = v
  | otherwise = v VU.++ VU.replicate (ceilPow2 n - n) 0
  where !n = VU.length v

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
{-# INLINE rep #-}

stream' :: Monad m => Int -> Int -> VFSM.Stream m Int
stream' !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x .<<. 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream' #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream' 1 n)
{-# INLINE rep' #-}

infixl 8 .<<., .>>., .>>>.
infixl 6 .^.
(.<<.), (.>>.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}
(.>>>.) :: Int -> Int -> Int
(.>>>.) (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}
(.^.) :: Bits b => b -> b -> b
(.^.)  = xor
{-# INLINE (.^.)  #-}
clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}
ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}
ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1
{-# INLINE ceilPow2 #-}
floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - clz n)
  | otherwise = 0
{-# INLINE floorPow2 #-}
