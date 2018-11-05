{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module System.FTDI.MPSSE
    ( Command
    , run

      -- * Clock divisor
    , setClockDivisor

      -- ** FT232H divide-by-5
    , enableClkDivBy5
    , disableClkDivBy5

    , enable3PhaseClocking
    , disable3PhaseClocking

      -- * Loopback
    , enableLoopback
    , disableLoopback

      -- * Data transfer
    , BitOrder(..)
    , ClockEdge(..)
    , flush
      -- ** Pausing
    , waitOnHigh
    , waitOnLow
      -- ** Byte-wise
    , readBytes
    , writeBytes
    , readWriteBytes

      -- * GPIO
    , Gpios(..)
    , Direction(..)
    , GpioBank(..)
    , setGpioDirValue
    ) where

import Control.Monad
import Data.Bits
import Data.Word
import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSB

import Control.Concurrent.Async

import qualified System.FTDI as FTDI
import System.FTDI (InterfaceHandle)

debug :: Bool
debug = False

-- Useful for debugging
showBS :: BS.ByteString -> String
showBS = foldr (\n rest -> showHex n . showChar ' ' $ rest) "" . BS.unpack

data Command a = Command { command :: BSB.Builder
                         , expectedBytes :: !Int
                         , parseBytes :: BS.ByteString -> a
                         }

instance Functor Command where
    fmap f (Command a b c) = Command a b (f . c)
    {-# INLINE fmap #-}

instance Applicative Command where
    pure x = Command mempty 0 (const x)
    {-# INLINE pure #-}
    Command a b c <*> Command a' b' c' =
        Command (a <> a') (b + b') parse
      where
        parse bs =
            let (bs1, bs2) = BS.splitAt b bs
            in (c bs1) (c' bs2)
    {-# INLINE (<*>) #-}

opCode :: Word8 -> Command ()
opCode = byte
{-# INLINE opCode #-}

byte :: Word8 -> Command ()
byte o = () <$ transfer (BSB.word8 o) 0
{-# INLINE byte #-}

word16 :: Word16 -> Command ()
word16 o = () <$ transfer (BSB.word16BE o) 0
{-# INLINE word16 #-}

transfer :: BSB.Builder -> Int -> Command BS.ByteString
transfer b n = Command { command = b
                       , expectedBytes = n
                       , parseBytes = id }
{-# INLINE transfer #-}

writeByteString :: BS.ByteString -> Command ()
writeByteString bs = () <$ transfer (BSB.byteString bs) 0
{-# INLINE writeByteString #-}

readN :: Int -> Command BS.ByteString
readN n = transfer mempty n
{-# INLINE readN #-}

-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

data Failure = WriteTimedOut Int
               -- ^ number of bytes written
             | InsufficientRead Int BS.ByteString
               -- ^ expected bytes and data actually read.
             deriving (Show)

-- | Assumes that the interface has already been placed in 'BitMode_MPSSE'
-- using 'setBitMode'.
run :: InterfaceHandle -> Command a -> IO (Either Failure a)
run ifHnd (Command cmd n parse) = do
    let cmd' = BSL.toStrict $ BSB.toLazyByteString cmd
    when debug $ putStrLn $ showBS cmd'
    writer <- async $ FTDI.writeBulk ifHnd cmd'
    (resp, _readStatus) <- FTDI.readBulk ifHnd n
    (written, _writeStatus) <- wait writer
    if | written /= BS.length cmd' -> return $ Left $ WriteTimedOut written
       | BS.length resp /= n -> return $ Left $ InsufficientRead n resp
       | otherwise -> return $ Right $ parse resp
{-# INLINE run #-}

-------------------------------------------------------------------------------
-- Clocking
-------------------------------------------------------------------------------

setClockDivisor :: Word16 -> Command ()
setClockDivisor n = opCode 0x86 *> word16 n
{-# INLINE setClockDivisor #-}

-- | The FT232H, FT2232H, and FT4232H can achieve higher data rates if the
-- clock divider is disabled.
disableClkDivBy5 :: Command ()
disableClkDivBy5 = opCode 0x8a

-- | Enable clock divider on for compatibility with FT2232D.
enableClkDivBy5 :: Command ()
enableClkDivBy5 = opCode 0x8b

enable3PhaseClocking :: Command ()
enable3PhaseClocking = opCode 0x8c

disable3PhaseClocking :: Command ()
disable3PhaseClocking = opCode 0x8d

-------------------------------------------------------------------------------
-- Loopback
-------------------------------------------------------------------------------

enableLoopback :: Command ()
enableLoopback = opCode 0x84
{-# INLINE enableLoopback #-}

disableLoopback :: Command ()
disableLoopback = opCode 0x85
{-# INLINE disableLoopback #-}

-------------------------------------------------------------------------------
-- GPIO
-------------------------------------------------------------------------------

data Gpios a = Gpios { gpio0, gpio1, gpio2, gpio3
                     , gpio4, gpio5, gpio6, gpio7 :: a
                     }
             deriving (Functor, Foldable, Traversable)

data Direction i o = Input i | Output o

data GpioBank = BankL | BankH

gpioBits :: Gpios Bool -> Word8
gpioBits Gpios{..} =
    b 0 gpio0 .|.
    b 1 gpio1 .|.
    b 2 gpio2 .|.
    b 3 gpio3 .|.
    b 4 gpio4 .|.
    b 5 gpio5 .|.
    b 6 gpio6 .|.
    b 7 gpio7
  where b n True  = bit n
        b _ False = 0

setGpioDirValue :: GpioBank -> Gpios (Direction () Bool) -> Command ()
setGpioDirValue bank vals = do
    opCode o *> byte valueByte *> byte dirByte
  where o = case bank of
              BankL -> 0x80
              BankH -> 0x82
        !dirByte = gpioBits $ fmap f vals
          where f (Output _) = True
                f _          = False
        !valueByte = gpioBits $ fmap f vals
          where f (Output True) = True
                f _             = False

-------------------------------------------------------------------------------
-- Transfers
-------------------------------------------------------------------------------

flush :: Command ()
flush = opCode 0x87

waitOnHigh :: Command ()
waitOnHigh = opCode 0x88

waitOnLow :: Command ()
waitOnLow = opCode 0x89

data BitOrder = MsbFirst | LsbFirst

data ClockEdge = Rising | Falling

otherEdge :: ClockEdge -> ClockEdge
otherEdge Rising  = Falling
otherEdge Falling = Rising

bitOrderBit :: BitOrder -> Word8
bitOrderBit LsbFirst = 0x0
bitOrderBit MsbFirst = 0x8

outEdgeBit :: ClockEdge -> Word8
outEdgeBit Rising  = 0x0
outEdgeBit Falling = 0x1

inEdgeBit :: ClockEdge -> Word8
inEdgeBit Rising  = 0x0
inEdgeBit Falling = 0x4

writeBytes :: ClockEdge -> BitOrder -> BS.ByteString -> Command ()
writeBytes edge order bs
  | BS.null bs = error "writeBytes: too short"
  | BS.length bs > 0x10000 = error "writeBytes: too long"
  | otherwise =
    opCode o *> word16 (fromIntegral $ BS.length bs - 1) *> writeByteString bs
  where
    o = 0x10 .|. bitOrderBit order .|. outEdgeBit edge
{-# INLINE writeBytes #-}

readBytes :: ClockEdge -> BitOrder -> Int -> Command BS.ByteString
readBytes edge order n
  | n == 0 = error "readBytes: too short"
  | n > 0x10000 = error "readBytes: too long"
  | otherwise =
    opCode o *> word16 (fromIntegral $ n - 1) *> readN (fromIntegral n)
  where
    o = 0x20 .|. bitOrderBit order .|. inEdgeBit edge
{-# INLINE readBytes #-}

readWriteBytes :: ClockEdge  -- ^ which edge to clock *out* data on
               -> BitOrder -> BS.ByteString -> Command BS.ByteString
readWriteBytes outEdge order bs
  | BS.null bs = error "readWriteBytes: too short"
  | BS.length bs > 0x10000 = error "readWriteBytes: too long"
  | otherwise =
    opCode o *> word16 (fromIntegral $ BS.length bs - 1) *> transfer (BSB.byteString bs) (BS.length bs)
  where
    o = 0x30 .|. bitOrderBit order .|. inEdgeBit (otherEdge outEdge) .|. outEdgeBit outEdge
{-# INLINE readWriteBytes #-}
