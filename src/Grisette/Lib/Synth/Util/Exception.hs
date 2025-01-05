{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Util.Exception (catchErrno) where

import Control.Exception (catch)
import Foreign.C (Errno, getErrno)

catchErrno :: IO a -> (IOError -> Errno -> IO a) -> IO a
catchErrno action handler =
  action `catch` \(e :: IOError) ->
    getErrno >>= handler e
