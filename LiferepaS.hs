{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Exception

import qualified Data.Array as Array
import Data.Char
import Data.IORef
import Data.Foldable as F (Foldable(fold,foldMap))
import Data.Monoid
import qualified Data.Text.IO as Text

import Data.Array.Repa (DIM2(..), (:.)(..), Z(..), U)
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import qualified Data.Array.Repa             as A
import qualified Data.Array.Repa.Eval        as A
import qualified Data.Array.Repa.Repr.Vector as A
import qualified Data.Array.Repa.Operators.Traversal as A

import qualified Data.Vector           as V
import qualified Data.Vector.Generic   as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed   as VU
import qualified Data.Vector.Generic.Mutable as VM

import Graphics.Gloss
import Graphics.Gloss.Raster.Array

import System.IO
import System.Environment

import GHC.Prim
import GHC.Types

import Life

screen = (1366,768)

main = do
  args <- getArgs
  gridstring <- Text.getContents
  let grid = parseGrid gridstring
      (w,h) = size grid
      (w', h') = (fst screen `div` w, snd screen `div` h)
      ratio = min h' w' `max` 1
  let fps  = case args of [] -> 10 ; [x] -> read x
  playArrayIO
       (FullScreen (((*ratio) *** (*ratio)) (size grid)))
       (ratio,ratio)
       fps
       (initialize grid)
       (return . toBitmap)
       (\_ -> return)
       (\_ -> A.computeP . step)

-- Encoding the rule in a type, for enabling the use of repa stencils for stepping
data Cell = Me !Bool | Neighbour !Bool | Wrap !Bool | Alive !Int | Dead !Int deriving (Eq, Ord, Show)
instance Monoid Cell where
  mempty = Dead 0
  {-# INLINE mappend #-}
  mappend !(Alive m)      (Wrap x) = let n = m + neigh x in Me(n == 2 || n == 3)
  mappend !(Dead  m)      (Wrap x) = let n = m + neigh x in Me(n == 3)
  mappend !(Alive m) (Neighbour x) = Alive (m + neigh x)
  mappend !(Dead  m) (Neighbour x) = Dead  (m + neigh x)
  mappend !(Dead  m)        (Me x) = (if x then Alive else Dead) m
  mappend !x !y                    = error ("mappend " ++ show x ++ " " ++ show y)

{-# INLINE neigh #-}
neigh x = if x then 1 else 0 :: Int

initialize :: Grid Bool -> A.Array U DIM2 Cell
initialize (Grid a _) = A.fromListUnboxed (Z :. h :. w) $ map Me $ Array.elems $ a
  where (_,(h,w)) = Array.bounds a

--step :: A.Array A.D DIM2 Cell -> (A.Array U DIM2 Cell)
step = mapStencil2 BoundMirror stencil where
  stencil =
    StencilStatic (Z :. 3 :. 3) mempty stencilFun

{-# INLINE stencilFun #-}
stencilFun !ix (Me val) !acc =
      case ix of
        (Z :.  0 :.  0) -> mappend acc $ Me val
        (Z :. -1 :. -1) -> mappend acc $ Wrap val
        (Z :. 1  :.  1) -> mappend acc $ Neighbour val
        (Z :. 1  :.  0) -> mappend acc $ Neighbour val
        (Z :. 1  :. -1) -> mappend acc $ Neighbour val
        (Z :. 0  :.  1) -> mappend acc $ Neighbour val
        (Z :. 0  :. -1) -> mappend acc $ Neighbour val
        (Z :. -1 :.  1) -> mappend acc $ Neighbour val
        (Z :. -1 :.  0) -> mappend acc $ Neighbour val
        _               -> acc

toBitmap = A.map (\(Me x) -> if x then red else black)


-- Various boilerplates

{- Boilerplate code to store cells in unboxed arrays -}
newtype instance VU.MVector s Cell = MV_Cell (VU.MVector s Bool)
newtype instance VU.Vector   Cell =  V_Cell  (VU.Vector Bool)
instance VG.Vector VU.Vector Cell where
  basicLength       (V_Cell v)  = VG.basicLength v
  basicUnsafeFreeze (MV_Cell v) = V_Cell  `liftM` VG.basicUnsafeFreeze v
  basicUnsafeThaw   (V_Cell  v) = MV_Cell `liftM` VG.basicUnsafeThaw   v
  basicUnsafeSlice  i n (V_Cell v) = V_Cell $ VG.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Cell v) i = Me `liftM` VG.basicUnsafeIndexM v i
instance VM.MVector VU.MVector Cell where
  basicLength      (MV_Cell v) = VM.basicLength v
  basicUnsafeSlice i n (MV_Cell v) = MV_Cell $ VM.basicUnsafeSlice i n v
  basicOverlaps    (MV_Cell v1) (MV_Cell v2) = VM.basicOverlaps v1 v2
  basicUnsafeNew   n = MV_Cell `liftM` VM.basicUnsafeNew n
  basicUnsafeRead  (MV_Cell v) i   = Me `liftM` VM.basicUnsafeRead v i
  basicUnsafeWrite (MV_Cell v) i (Me x) = VM.basicUnsafeWrite v i x
instance VU.Unbox Cell
{- end of vector boilerplate -}

-- Required by the computeP application, not actually used
instance Num Cell

-- Required by repa
instance A.Elt Cell where
  {-# INLINE touch #-}
  touch (Me b)
   = IO (\state -> case touch# b state of
   		state' -> (# state', () #))
  zero = Me False
  one  = Me True
