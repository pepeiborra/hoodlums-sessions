{-# LANGUAGE BangPatterns #-}

import Control.Arrow

import qualified Data.Array as Array

import Data.Array.Repa (DIM2(..), (:.)(..), Z(..), U)
import qualified Data.Array.Repa             as A
import qualified Data.Array.Repa.Operators.Traversal as A

import Graphics.Gloss
import Graphics.Gloss.Raster.Array

import System.IO
import System.Environment

import Life

screen = (1366,768)

main = do
  args <- getArgs
  gridstring <- getContents
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

initialize :: Grid Bool -> A.Array U DIM2 Bool
initialize (Grid a _) = A.fromListUnboxed (Z :. h :. w) $ Array.elems $ a
  where (_,(h,w)) = Array.bounds a

step arr = A.unsafeTraverse arr id elemFn where
  Z :. w :. h = A.extent arr

  {-# INLINE elemFn #-}
  elemFn !ix me@(Z :. i :. j) =
    case neighbours of
      3 -> True
      2 -> ix me
      _ -> False
   where
    !neighbours =
      get ( 1,  1) +
      get ( 1,  0) +
      get ( 1, -1) +
      get ( 0,  1) +
      get ( 0, -1) +
      get (-1, -1) +
      get (-1,  0) +
      get (-1,  1)

    {-# INLINE get #-}
    get !(x,y) = if ix(Z :. (i+x+w) `mod` w :. (j+y+h) `mod` h) then 1 else 0

toBitmap = A.map (\x -> if x then red else black)
