{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}


module ResizableSpacing (
                               -- * Usage
                               -- $usage
                               spacing, Spacing, SpacingMessage(..)
                             ) where

import XMonad.Core

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import ResizableSpacing
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = spacing 2 $ Tall 1 (3/100) (1/2)
-- >                      -- put a 2px space around every window
--


-- | Messages which can be sent to a spacing modifier.
data SpacingMessage = IncSpacing !Int | DecSpacing !Int
  deriving (Typeable)

instance Message SpacingMessage

data Spacing a = Spacing Int deriving (Show, Read)

instance LayoutModifier Spacing a where
  pureModifier (Spacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

  pureMess (Spacing x) m
    | Just (IncSpacing i)  <- fromMessage m
      = Just $ Spacing (x + i)
    | Just (DecSpacing i)  <- fromMessage m
      = Just $ Spacing (max 0 (x - i))
    | otherwise = Nothing


shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect p (Rectangle x y w h) = Rectangle (x+fi p) (y+fi p) (w-2*fi p) (h-2*fi p)


-- | Surround all windows by a certain number of pixels of blank space.
spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing p = ModifiedLayout (Spacing p)