{-# LANGUAGE BangPatterns #-}

module For where

import           Control.Monad.Cont
import           Control.Monad.ST
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM

-- | l -> x -> r, +d
stream :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
stream !l !r !d = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

-- | 0 <= x < n, interval = 1
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 (n - 1) 1)
{-# INLINE rep #-}

-- | 0 <= x <= n, interval = 1
rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream 0 n 1)
{-# INLINE rep' #-}

-- | 1 <= x < n, interval = 1
rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (stream 1 (n - 1) 1)
{-# INLINE rep1 #-}

-- | 1 <= x <= n, interval = 1
rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (stream 1 n 1)
{-# INLINE rep1' #-}

-- | l <= x <= r, interval = d
for :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
for l r d = flip VFSM.mapM_ (stream l r d)
{-# INLINE for #-}

-- | r -> x -> l, -d
streamR :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamR !r !l !d = VFSM.Stream step r
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

-- | n > x >= 0, interval = -1
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR (n - 1) 0 1)
{-# INLINE rev #-}

-- | n >= x >= 0, interval = -1
rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamR n 0 1)
{-# INLINE rev' #-}

-- | n > x >= 1, interval = -1
rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR (n - 1) 1 1)
{-# INLINE rev1 #-}

-- | n >= x >= 1, interval = -1
rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamR n 1 1)
{-# INLINE rev1' #-}

-- | r >= x >= l, interval = -d
forR :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
forR r l d = flip VFSM.mapM_ (streamR r l d)
{-# INLINE forR #-}

-- | for (int i = l; f(i, p) <= r ; g(i, d))
streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG l r f p g d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}
