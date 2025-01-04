{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Serialize
  ( finiteBitsToByteStringLE,
    byteStringToFiniteBitsLE,
    byteStringToWord64,
    word64ToByteString,
    byteStringToWord32,
    word32ToByteString,
    byteStringToBool,
    boolToByteString,
    writeByteStringWithSize,
    writeObject,
    readByteStringWithSize,
    readObject,
    nonBlockingReadByteStringWithSize,
    nonBlockingReadObject,
  )
where

import Control.Exception (catch)
import Data.Bits (Bits (shiftL, shiftR, (.|.)), FiniteBits (finiteBitSize))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Unsafe as B
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Word (Word32, Word64)
import Foreign.C (eAGAIN, getErrno)
import System.Posix (Fd)
import System.Posix.IO.ByteString (fdRead, fdWrite)

finiteBitsToByteStringLE :: (FiniteBits n, Integral n) => n -> B.ByteString
finiteBitsToByteStringLE n =
  B.pack $ fromIntegral . shiftR n <$> shiftIdx
  where
    bytesNeeded = (finiteBitSize n + 7) `div` 8
    shiftIdx = (* 8) <$> [0 .. bytesNeeded - 1]

byteStringToFiniteBitsLE :: (FiniteBits n, Integral n) => B.ByteString -> n
byteStringToFiniteBitsLE s =
  foldl (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 $ reverse $ B.unpack s

byteStringToWord64 :: B.ByteString -> Word64
byteStringToWord64 s =
  (fromIntegral (s `B.unsafeIndex` 7) `shiftL` 56)
    .|. (fromIntegral (s `B.unsafeIndex` 6) `shiftL` 48)
    .|. (fromIntegral (s `B.unsafeIndex` 5) `shiftL` 40)
    .|. fromIntegral ((s `B.unsafeIndex` 4) `shiftL` 32)
    .|. (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24)
    .|. (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16)
    .|. (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8)
    .|. fromIntegral (s `B.unsafeIndex` 0)

word64ToByteString :: Word64 -> B.ByteString
word64ToByteString w = B.toStrict $ B.toLazyByteString $ B.word64LE w

byteStringToWord32 :: B.ByteString -> Word32
byteStringToWord32 s =
  (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24)
    .|. (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16)
    .|. (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8)
    .|. fromIntegral (s `B.unsafeIndex` 0)

word32ToByteString :: Word32 -> B.ByteString
word32ToByteString w = B.toStrict $ B.toLazyByteString $ B.word32LE w

byteStringToBool :: B.ByteString -> Bool
byteStringToBool bs = bs `B.unsafeIndex` 0 /= 0

boolToByteString :: Bool -> B.ByteString
boolToByteString True = B.singleton 1
boolToByteString False = B.singleton 0

writeByteStringWithSize :: Fd -> B.ByteString -> IO ()
writeByteStringWithSize fd bs = do
  let n = word32ToByteString $ fromIntegral $ B.length bs
  fdWrite fd n
  fdWrite fd bs
  return ()

writeObject :: (Serial a) => Fd -> a -> IO ()
writeObject fd obj = writeByteStringWithSize fd (runPutS (serialize obj))

readByteStringWithSize :: Fd -> IO B.ByteString
readByteStringWithSize fd = do
  bs <- fdRead fd 4
  let n = fromIntegral $ byteStringToWord32 bs
  fdRead fd n

readObject :: (Serial a) => Fd -> IO a
readObject fd = do
  bs <- readByteStringWithSize fd
  case runGetS deserialize bs of
    Left err -> error $ "Failed to deserialize object: " <> err
    Right obj -> return obj

nonBlockingReadObject :: (Serial a) => Fd -> IO (Maybe a)
nonBlockingReadObject fd = do
  bs <- nonBlockingReadByteStringWithSize fd
  case bs of
    Nothing -> return Nothing
    Just bs -> do
      case runGetS deserialize bs of
        Left err -> error $ "Failed to deserialize object: " <> err
        Right obj -> return $ Just obj

nonBlockingReadByteStringWithSize :: Fd -> IO (Maybe B.ByteString)
nonBlockingReadByteStringWithSize fd = do
  bytesToRead <-
    (Just . byteStringToWord32 <$> fdRead fd 4)
      `catch` \(_ :: IOError) -> do
        errno <- getErrno
        if errno == eAGAIN
          then return Nothing
          else error "Failed to read from pipe"
  let go 0 (Just buf) = return buf
      go i maybeBuf = do
        r <-
          fdRead fd i `catch` \(_ :: IOError) -> do
            errno <- getErrno
            if errno == eAGAIN
              then go i maybeBuf
              else error "Failed to read from pipe"
        let bytesRead = fromIntegral $ B.length r
        case maybeBuf of
          Nothing ->
            if bytesRead == i
              then return r
              else go (i - bytesRead) (Just r)
          Just oldBuf ->
            if bytesRead == i
              then return $ oldBuf <> r
              else go (i - bytesRead) (Just $ oldBuf <> r)
  case bytesToRead of
    Nothing -> return Nothing
    Just bytesToRead -> do
      buf <- go (fromIntegral bytesToRead) Nothing
      return $ Just buf
