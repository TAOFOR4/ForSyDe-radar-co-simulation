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
import Data.Complex (Complex((:+)))
C.context (C.baseCtx <> C.vecCtx <> C.funCtx)

-- Define a new data type for CComplexFloat
data CComplexFloat = CComplexFloat CFloat CFloat

-- Implement Storable instance for the new CComplexFloat data type
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

-- Utility functions to convert between Haskell and C complex types
toCComplex :: (Float, Float) -> CComplexFloat
toCComplex (re, im) = CComplexFloat (realToFrac re) (realToFrac im)

fromCComplex :: CComplexFloat -> (Float, Float)
fromCComplex (CComplexFloat re im) = (realToFrac re, realToFrac im)

-- Inline C function to pass the complex vector and return it
C.include "<complex.h>"

passThroughC :: V.Vector CComplexFloat -> IO (V.Vector CComplexFloat)
passThroughC vec = do
    let len = fromIntegral $ V.length vec
    V.unsafeWith vec $ \ptr -> do
        let cPtr = castPtr ptr :: Ptr CFloat -- Cast the pointer
        [C.block| void {
            int i;
            int len = $(int len);
            float complex *vec = (float complex *)$(float *cPtr); // Use the casted pointer

            for(i = 0; i < len; ++i) {
                // Process the complex numbers as needed
            }
        } |]
        return vec

main :: IO ()
main = do
    -- Create a complex float vector purely in Haskell
    let hsComplexVec = V.fromList [1.0 :+ 2.0, 3.0 :+ 4.0, 5.0 :+ 6.0] :: V.Vector (Complex Float)
    putStrLn "Original Haskell Complex Vector:"
    print $ V.toList hsComplexVec

    -- Convert Haskell complex float vector to CComplexFloat vector
    let cVec = V.map (\(re :+ im) -> toCComplex (re, im)) hsComplexVec :: V.Vector CComplexFloat

    -- Pass the vector to the C function and get it back
    resultCVec <- passThroughC cVec

    -- Convert the CComplexFloat vector back to a Haskell complex float vector
    let hsResultVec = V.map (\(CComplexFloat re im) -> realToFrac re :+ realToFrac im) resultCVec :: V.Vector (Complex Float)

    -- Print the converted vector
    putStrLn "Returned Haskell Complex Vector:"
    print $ V.toList hsResultVec
