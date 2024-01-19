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

-- Process a flat vector of complex numbers
processFlatVector :: V.Vector CComplexFloat -> IO (V.Vector CComplexFloat)
processFlatVector vec = do
    let len = fromIntegral $ V.length vec
    V.unsafeWith vec $ \ptr -> do
        let cPtr = castPtr ptr :: Ptr CFloat
        [C.block| void {
            int i;
            int len = $(int len);
            float complex *vec = (float complex *)$(float *cPtr);

            for(i = 0; i < len; ++i) {
                // Process each complex number
            }
        } |]
        return vec

main :: IO ()
main = do
    -- Create a flattened vector of complex numbers
    let hsVec = V.concat [ V.fromList $ map toCComplex [(1.0, 2.0), (3.0, 4.0)],
                           V.fromList $ map toCComplex [(5.0, 6.0), (7.0, 8.0)] ]

    -- Process the flat vector
    resultVec <- processFlatVector hsVec

    -- Split the processed vector back into the original structure
    let splitIndices = [2, 4] -- Indices where the original vectors ended
    let splitVecs = splitAtIndices splitIndices resultVec

    -- Convert the vectors to list after applying fromCComplex
    let originalList = map (map fromCComplex . V.toList) splitVecs
    let resultList = map (map fromCComplex . V.toList) splitVecs

    -- Print the original and returned vectors
    putStrLn "Original Vector:"
    mapM_ print originalList

    putStrLn "Returned Vector:"
    mapM_ print resultList

-- Function to split a vector at given indices
splitAtIndices :: Storable a => [Int] -> V.Vector a -> [V.Vector a]
splitAtIndices indices vec = map (\(i, j) -> V.slice i (j - i) vec) $ zip (0 : indices) indices
