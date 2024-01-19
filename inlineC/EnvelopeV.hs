{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Coerce (coerce)
import Data.Monoid ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO.Unsafe (unsafePerformIO)

C.context (C.baseCtx <> C.vecCtx <> C.funCtx)

data CComplexFloat = CComplexFloat CFloat CFloat

instance Storable CComplexFloat where
    sizeOf _ = 2 * sizeOf (undefined :: CFloat)
    alignment _ = alignment (undefined :: CFloat)

    peek ptr = do
        re <- peekByteOff ptr 0
        im <- peekByteOff ptr (sizeOf (undefined :: CFloat))
        return (CComplexFloat re im)

    poke ptr (CComplexFloat re im) = do
        pokeByteOff ptr 0 re
        pokeByteOff ptr (sizeOf (undefined :: CFloat)) im

toCComplex :: (Float, Float) -> CComplexFloat
toCComplex (re, im) = CComplexFloat (realToFrac re) (realToFrac im)

fromCComplex :: CComplexFloat -> (Float, Float)
fromCComplex (CComplexFloat re im) = (realToFrac re, realToFrac im)

C.include "<complex.h>"
C.include "<math.h>"
passThroughC :: V.Vector CComplexFloat -> IO (V.Vector Float)
passThroughC vec = do
    let len = V.length vec  -- length as Int
    let len_hs = fromIntegral len :: CInt  -- Convert len to CInt for the inline C code
    resVec <- VM.new len :: IO (VM.IOVector CFloat) -- Use len as Int for VM.new
    V.unsafeWith vec $ \ptr -> do
        let cPtr = castPtr ptr :: Ptr CFloat
        VM.unsafeWith resVec $ \resPtr -> [C.block| void {
            int i;
            int len = $(int len_hs); // Use len_hs in the inline C code
            float complex *vec = (float complex *)$(float *cPtr);
            float *res = $(float *resPtr);

            for(i = 0; i < len; ++i) {
                float re = crealf(vec[i]);
                float im = cimagf(vec[i]);
                res[i] = sqrtf(re*re + im*im);
            }
        } |]
    V.freeze resVec >>= \immutableResVec -> return $ V.map realToFrac immutableResVec


main :: IO ()
main = do
    let hsVec = V.fromList $ map toCComplex [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
    envelopes <- passThroughC hsVec
    let originalList = map fromCComplex $ V.toList hsVec
    putStrLn "Original Vector:"
    print originalList
    
    putStrLn "Envelopes of the complex vector:"
    print $ V.toList envelopes