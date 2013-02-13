{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Life where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array.Unboxed
import Data.Attoparsec.Text hiding (take)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text(Text)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Grid a = Grid {-# UNPACK #-} !(Array (Int,Int) a) {-# UNPACK #-} !(Int, Int)

instance Show (Grid Bool) where
  show (Grid a _) = unlines $  rows
    where
      ((x0,y0),(w,h)) = bounds a
      rows = [ [ if a!(i,j) then 'o' else '.'
               | j <- [y0..h] ]
             | i <- [x0..w]
             ]

class Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b

{-# INLINE (=>>) #-}
(=>>) = flip extend
infixl 1 =>>

instance Comonad Grid where
  extract (Grid a p) = a ! p
  {-# INLINE extend #-}
  extend f (Grid a p) = Grid ( listArray (bounds a) . map (f . Grid a) $ indices a) p

parseGrid :: Text -> Grid Bool
parseGrid txt = fromMaybe (parsePlain txt) (parseRLE txt)

parsePlain :: Text -> Grid Bool
parsePlain s = Grid ( listArray ((1,1), (height,width)) es) (1,1)
  where
    ls     = dropWhile ("!" `Text.isPrefixOf`) $ Text.lines s
    width  = Text.length $ head ls
    height = length ls
    es     = map ((||) <$> (== 'O') <*> (== 'o') ) $ Text.unpack $ Text.concat ls

--parseRLE :: Text -> Maybe (Grid Bool)
parseRLE = either (const Nothing) Just
         . parseOnly parser
         . Text.unlines
         . filter (not . ("#" `Text.isPrefixOf`))
         . Text.lines
  where

   parser = do
     (w,h) <- wh
     skipWhile (not . isEndOfLine)
     endOfLine
     elems <- many elem
     char '!'
     _ <- takeText
     let contents = expand (w,h) elems
     let grid = Grid (listArray ((1,1),(h,w)) (contents ++ repeat False)) (1,1)
     return grid
    where
     w = "x" .*> skipSpace *> "=" .*> skipSpace *> decimal
     h = "y" .*> skipSpace *> "=" .*> skipSpace *> decimal
     wh = (,) <$> w <*. "," <* skipSpace <*> h
     elem = (,) <$> optional decimal <*> satisfy(inClass "bo$") <* skipSpace

   expand (w,h)
          = concat
          . take h
          . (++ repeat (replicate w False))
          . map ( take w
                . (++ repeat False)
                . concatMap (\(count,tag) -> replicate count (tag == 'o')))
          . splitOn [(1,'$')]
          . concatMap (\(count,tag) -> if tag == '$' then replicate count (1,'$') else [(count,tag)])
          . map (first (fromMaybe 1))

size :: Grid a -> (Int, Int)
size (Grid a _) = snd (bounds a)

gridIndex :: Grid a -> (Int, Int)
gridIndex (Grid _ p) = p

gridIndices :: Grid a -> Grid (Int, Int)
gridIndices g = g =>> gridIndex

moveGrid :: (Int, Int) -> Grid a -> Grid a
moveGrid (xx,yy) (Grid a (x,y)) = Grid a (x',y')
  where
    ((w0,h0),(w,h)) = bounds a
    x' = (x + xx - w0) `mod` w + w0
    y' = (y + yy - w0) `mod` h + w0

neighbours :: Grid Bool -> Int
neighbours g = length . filter id $ bools
  where
    bools = map (\o -> extract $ moveGrid o g) offsets
    offsets = [(x,y) | x <- [(-1)..1], y <- [(-1)..1], (x,y) /= (0,0)]

stepGrid :: x -> Float -> Grid Bool -> Grid Bool
stepGrid _ _ g = g =>> (rule <$> extract <*> neighbours)
