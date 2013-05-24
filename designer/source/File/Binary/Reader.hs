-- grid is a game written in Haskell
-- Copyright (C) 2013 Carl Joachim Svenn
-- 
-- This file is part of grid.
-- 
-- grid is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grid is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grid.  If not, see <http://www.gnu.org/licenses/>.
--
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module File.Binary.Reader
  (
    Reader,

    readBinary,
    readBinaryAt,
    readBinary',
    readBinaryAt',

    rSatisfy,
    rWord8s,
    rOneOf,
    rNoneOf,
    rAnyWord8,
    rWord8, 
    rNUL,
    rNonNUL,
    rSkipNULs,
    rBool,
    rInt32,
    rUInt32,
    rCString,
    rOffset,
    rAlign,

    word8sAsWord32,

  ) where


import MyPrelude
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error
import System.IO
import Data.Binary.Put
import Control.Monad.Identity

--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS

import Data.Char
import Data.Word
import Data.Bits

import Control.Exception as C


-- this is tmp!
#define PLATFORM_LE

{-
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Int
import Data.Int
-}

--------------------------------------------------------------------------------
--  
-- todo:  * write custom parser library for binary files as 'parsec', using 'binary' 
--          (and 'binary-strict'?). this should fix current problems. currently,
--          we hack on top of 'parsec'
--        * implement functions for zlib, in order to work with directories?
--
-- fixme  * write custom parser library for Word8!
--        * satisfy and string from Text.Parsec.Char should not be used, since
--          they update SourcePos wrong


--------------------------------------------------------------------------------
--  Reader


-- | fixme: parameterize monad as m. 
--          then readBinaryXXX working in MonadIO m?
type Reader = 
    ParsecT ByteStream () IO


newtype ByteStream =
    ByteStream
    {
        unwrapByteString :: BS.ByteString
    }


wrapByteString :: BS.ByteString -> ByteStream
wrapByteString bs =
    ByteStream { unwrapByteString = bs }


packByteStream :: [Word8] -> ByteStream
packByteStream =
    wrapByteString . BS.pack


unpackByteStream :: ByteStream -> [Word8]
unpackByteStream =
    BS.unpack . unwrapByteString


instance (Monad m) => Stream ByteStream m Word8 where
    uncons = return . 
             fmap (\(w, bs) -> (w, wrapByteString bs)) . 
             BS.uncons . 
             unwrapByteString






--------------------------------------------------------------------------------
--  read

readByteStream :: FilePath -> IO ByteStream
readByteStream path = 
    wrapByteString `fmap` BS.readFile path


readByteStreamAt :: FilePath -> UInt -> IO ByteStream
readByteStreamAt path off = 
    wrapByteString `fmap` do C.bracket (openBinaryFile path ReadMode) hClose $ \h -> do
                             size <- hFileSize h
                             hSeek h AbsoluteSeek (fI off)
                             BS.hGet h (fI size - fI off)

parsingAt :: 
             Reader a -> 
             SourceName -> 
             UInt -> 
             ByteStream -> 
             IO (Either String a)
parsingAt p name off s = 
   runPT p () name s
   where
    runPT p u name s = do 
        res <- runParsecT p (State s (newPos name 0 (fI off)) u)
        r <- parserReply res
        case r of
            Ok x _ _  -> return (Right x)
            Error err -> return (Left $ showParseError err)
        where
          parserReply res = case res of
              Consumed r -> r
              Empty    r -> r
     
showParseError :: ParseError -> String
showParseError err =
    showSrc err ++ showMsg err
    where
      showSrc err =
          "\"" ++ (sourceName $ errorPos err) ++ 
          "\" (offset " ++ show (sourceColumn $ errorPos err) ++ "): "
      showMsg err =
          showErrorMessages "or" "unknown parse error" "expecting" 
                            "unexpected" "end of input" (errorMessages err)


-- | read binary file
readBinary :: Reader a -> FilePath -> IO (Either String a)
readBinary ra path =
    C.catch (readByteStream path >>= parsingAt ra path 0) $ \e -> 
        return $ Left $ show (e :: IOException)

    

-- | read binary file, at offset in 8-bit bytes
readBinaryAt :: Reader a -> FilePath -> UInt -> IO (Either String a)
readBinaryAt ra path off = 
    C.catch (readByteStreamAt path off >>= parsingAt ra path off) $ \e -> 
        return $ Left $ show (e :: IOException)
    

-- | assuming success, else error
readBinary' :: Reader a -> FilePath -> IO a
readBinary' ra path = do
    readByteStream path >>= parsingAt ra path 0 >>= \res -> case res of
        Left msg  -> error msg
        Right a   -> return a

-- | assuming success, else error
readBinaryAt' :: Reader a -> FilePath -> UInt -> IO a
readBinaryAt' ra path off =
    readByteStreamAt path off >>= parsingAt ra path off >>= \res -> case res of
        Left msg  -> error msg
        Right a   -> return a




--------------------------------------------------------------------------------
--  

rSatisfy :: (Word8 -> Bool) -> Reader Word8
rSatisfy f = 
    tokenPrim (\w -> showWord8 w)
              (\pos w _ws -> incSourceColumn pos 1)
              (\w -> if f w then Just w else Nothing)

rWord8s :: [Word8] -> Reader ()
rWord8s s = 
    tokens show (\pos string -> incSourceColumn pos (length string)) s >> return ()


rOffset :: Reader UInt
rOffset = 
    (fI . sourceColumn) `fmap` getPosition

rOneOf :: [Word8] -> Reader Word8
rOneOf cs            = rSatisfy (\c -> elem c cs)


rNoneOf :: [Word8] -> Reader Word8
rNoneOf cs           = rSatisfy (\c -> not (elem c cs))


rAnyWord8 :: Reader Word8
rAnyWord8             = rSatisfy (const True)


rWord8 :: Word8 -> Reader ()
rWord8 c =
    (rSatisfy (==c)  >> return ()) <?> showWord8 c


rNUL :: Reader ()
rNUL               = 
    (rSatisfy (== 0x00)       <?> "NUL") >> return ()


rNonNUL :: Reader Word8
rNonNUL =
    rSatisfy (/= 0x00) <?> "non-NUL"

rSkipNULs :: Reader ()
rSkipNULs = do
    many rNUL
    return ()

-- | False == 0x01
--   True  == 0x02 
rBool :: Reader Bool
rBool =
    (rSatisfy (\w -> w == 0x00 || w == 0x01) >>= \w -> case w of
        0x01  -> return False
        0x02  -> return True
    ) <?> "Bool"

-- | reading a C-string
rCString :: Reader String
rCString = do
    ws <- many rNonNUL
    rNUL
    return $ map (chr . fI) ws


-- | reading a value [-2^31, 2^31)
rInt32 :: Reader Int
rInt32 = do
    w0 <- rAnyWord8
    w1 <- rAnyWord8
    w2 <- rAnyWord8
    w3 <- rAnyWord8
    case word8sAsWord32 w0 w1 w2 w3 of
        w32 -> convert w32

    where
{-
      convert (W32# w32#) =
          (I# (unsafeCoerce# w32#))
-}
      convert w32 = 
          if testBit w32 31
             then return $ negate $ fI $ complement w32 + 1
             else return $ fI w32


-- | reading a value [0, 2^32)
rUInt32 :: Reader UInt
rUInt32 = do
    w0 <- rAnyWord8
    w1 <- rAnyWord8
    w2 <- rAnyWord8
    w3 <- rAnyWord8
    return $ fI $ word8sAsWord32 w0 w1 w2 w3


rAlign :: UInt -> Reader ()
rAlign n = 
    rOffset >>= \off -> do
        replicateM_ (fI $ (n - off `mod` n) `mod` n) rAnyWord8
        return ()


--------------------------------------------------------------------------------
-- 


-- | little endian -> platform endian
word8sAsWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word8sAsWord32 w0 w1 w2 w3 = 
#ifdef PLATFORM_LE
    (shiftL (fI w0) 0)  .|. 
    (shiftL (fI w1) 8)  .|. 
    (shiftL (fI w2) 16) .|. 
    (shiftL (fI w3) 24)
#else
    (shiftL (fI w3) 0)  .|. 
    (shiftL (fI w2) 8)  .|. 
    (shiftL (fI w1) 16) .|. 
    (shiftL (fI w0) 24)
#endif

showWord8 :: Word8 -> String
showWord8 = 
    show . chr . fI


