module PvdState (
  State(..),
  CachedImg(..)
) where

import Control.Concurrent.STM (TVar)
import Graphics.X11.Xlib (Display, Window)
import Network.Socket (Socket)
import XUtils (XImg)

data State = State {
  stIdx :: TVar Int,
  stPlaylist :: TVar [String],
  stDpy :: Display,
  stWin :: Window,
  stImgCache :: TVar [(String, CachedImg)],
  stImgCacheSize :: Int,
  stSocket :: Socket
}

data CachedImg = CachedImg XImg | LoadingImg | LoadFailed
