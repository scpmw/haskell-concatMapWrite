{-# LANGUAGE BangPatterns, RankNTypes #-}

import Blaze.ByteString.Builder
import Data.Bits
import Data.Monoid              (mappend, mconcat, mempty)
import Data.ByteString          (ByteString)
import Data.Word                (Word8)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L

import Blaze.ByteString.Builder.Internal.Types
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)

import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Storable         (peek)
import Foreign.Ptr              (plusPtr)
import System.IO.Unsafe

writeBackslash :: Write
writeBackslash = writeWord8 92

escape1 :: Word8 -> Builder
escape1 92 = fromWrite $ writeBackslash
               `mappend` writeBackslash
               `mappend` writeBackslash
               `mappend` writeBackslash
escape1 c | c >= 32 && c <= 126 = fromWrite $ writeWord8 c
          | otherwise = fromWrite $ writeBackslash
                          `mappend` writeBackslash
                          `mappend` writeWord8 (48 + ((c `shiftR` 6) .&. 0x7))
                          `mappend` writeWord8 (48 + ((c `shiftR` 3) .&. 0x7))
                          `mappend` writeWord8 (48 + (c .&. 0x7))

escapeCopyBytea2 :: ByteString -> Builder
escapeCopyBytea2 = foldBuilder escape1

--escapeCopyBytea2 :: ByteString -> Builder
--escapeCopyBytea2 = foldrL f mempty
--    where
--        f c b = escape1 c `mappend` b

-- | Lazy right fold over byte strings
foldrL :: (Word8 -> a -> a) -> a -> ByteString -> a
foldrL f v (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        return $ lgo (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        lgo !p !q | p == q    = v
                  | otherwise = f (inlinePerformIO (peek p)) (lgo (p `plusPtr` 1) q)

-- | Specialized version for folding with a Builder result
foldBuilder :: (Word8 -> Builder) -> B.ByteString -> Builder
foldBuilder f (PS x s l) =
  fromBuildStepCont $ \cont range ->
    withForeignPtr x $ \ptr -> do
      let lgo !p !q !range'
            | p == q    = cont range'
            | otherwise = do
                c <- peek p
                let p' = p `plusPtr` 1
                    step = unBuilder (f c) (BuildStep $ lgo p' q)
                runBuildStep step range'
      lgo (ptr `plusPtr` s) (ptr `plusPtr` (s+l)) range


main :: IO ()
main = L.getContents >>= L.putStr . toLazyByteString . mconcat
                       . map escapeCopyBytea2 . L.toChunks
