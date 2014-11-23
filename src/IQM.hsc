{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module IQM where

import Foreign.C
import Foreign.Safe
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Control.Applicative
import System.IO.MMap
import Data.Traversable
import Debug.Trace
import Linear
import Data.Vector.Storable.Internal

#include "iqm.h"

loadIQMFile :: FilePath -> IO Header
loadIQMFile fp = 
    do (fptr, offset, size) <- mmapFileForeignPtr fp ReadOnly Nothing
       withForeignPtr fptr $ \ptr -> 
           do let optr = plusPtr ptr offset
              txt_ofs <- (#peek struct iqmheader, ofs_text) optr :: IO CUInt
              let txtPtr = plusPtr optr $ fromIntegral txt_ofs
              trianglesOffset <- (#peek struct iqmheader, ofs_triangles) optr :: IO CUInt
              numTriangles <- (#peek struct iqmheader, num_triangles) optr :: IO CUInt
              let triangleVec = V.unsafeFromForeignPtr0 (castForeignPtr $ updPtr (`plusPtr` (fromIntegral trianglesOffset + offset)) fptr) 
                                                        (fromIntegral numTriangles) 
              Header <$> pure fptr
                     <*> (#peek struct iqmheader, version) optr
                     <*> (#peek struct iqmheader, flags) optr
                     <*> getMeshes optr txtPtr
                     <*> getVertexArrays optr
                     <*> pure triangleVec

printTriangles :: Ptr (V3 CUInt) -> CUInt -> IO ()
printTriangles _ 0 = return ()
printTriangles ptr num = do tr <- peek ptr :: IO (V3 CUInt)
                            print tr
                            printTriangles (advancePtr ptr 1) (num - 1)

data Header = Header
            { filePtr :: ForeignPtr IQMHeader
            , version :: CUInt
            , flags :: CUInt
            , meshes :: [Mesh]
            , vertexArrays :: [VertexArray]
            , triangles :: Vector (V3 CUInt)
--                  , joints :: [Joint]
--                  , poses :: [Pose]
--                  , animations :: [Animation]
--                  , frames :: [Frame]
--                  , bounds :: [Bound]
--                  , comments :: [ByteString]
--                  , extensions :: [Extension]
            } deriving (Show)

data IQMHeader

getTexts :: Ptr IQMHeader -> IO [ByteString]
getTexts hp = do offset <- (#peek struct iqmheader, ofs_text) hp :: IO CUInt
                 num <- (#peek struct iqmheader, num_text) hp :: IO CUInt
                 getBytestrings (fromIntegral num) (plusPtr hp $ fromIntegral offset)

getBytestrings :: Int -> CString -> IO [ByteString]
getBytestrings 0   _   = return []
getBytestrings num ptr = do str <- BS.packCString ptr :: IO ByteString
                            let newPtr = plusPtr ptr (fromIntegral . (+1) . BS.length $ str)
                            strs <- getBytestrings (num -1) newPtr
                            return $ str : strs 

getComments :: Ptr IQMHeader -> IO [ByteString]
getComments hp = do offset <- (#peek struct iqmheader, ofs_comment) hp :: IO CUInt
                    num <- (#peek struct iqmheader, num_comment) hp :: IO CUInt
                    getBytestrings (fromIntegral num) (plusPtr hp $ fromIntegral offset)

data Mesh = Mesh
          { name :: ByteString
          , material :: ByteString
          , baseVertex :: CUInt
          , numVertices :: CUInt
          , baseIndice :: CUInt
          , numIndices :: CUInt
          } deriving (Show)

getMeshes :: Ptr IQMHeader -> CString -> IO [Mesh]
getMeshes hp txt = 
    do offset <- (#peek struct iqmheader, ofs_meshes) hp :: IO CUInt
       num <- (#peek struct iqmheader, num_meshes) hp :: IO CUInt
       forM [0.. (fromIntegral num - 1)] $ \i -> 
                 let ptr = plusPtr hp $ (fromIntegral offset) 
                                      + i * (#size struct iqmmesh)
                     getString :: CUInt -> IO ByteString
                     getString = BS.packCString . plusPtr txt . fromIntegral
                 in Mesh <$> (getString =<< (#peek struct iqmmesh, name) ptr) 
                         <*> (getString =<< (#peek struct iqmmesh, material) ptr)
                         <*> (#peek struct iqmmesh, first_vertex) ptr
                         <*> (#peek struct iqmmesh, num_vertexes) ptr
                         <*> (#peek struct iqmmesh, first_triangle) ptr
                         <*> (#peek struct iqmmesh, num_triangles) ptr

data VertexArray = VertexArray
                 { vaflags :: CUInt
                 , buffer :: VertexArrayBuffer
                 } deriving (Show)

data VertexArrayBuffer = PositionBuffer (Ptr (V3 CFloat))
                       | TexCoordBuffer (Ptr (V2 CFloat))
                       | NormalBuffer   (Ptr (V3 CFloat))
                       | TangentBuffer  (Ptr (V4 CFloat))
                       | BlendIndexesBuffer (Ptr (V4 CUChar))
                       | BlendWeightsBuffer (Ptr (V4 CUChar))
                       | ColorBuffer (Ptr (V4 CUChar))
                       | CustomBuffer (Ptr ()) CUInt CUInt CUInt
                       deriving (Show)

getVertexArrays :: Ptr IQMHeader -> IO [VertexArray]
getVertexArrays hp = 
    do offset <- (#peek struct iqmheader, ofs_vertexarrays) hp :: IO CUInt
       num <- (#peek struct iqmheader, num_vertexarrays) hp :: IO CUInt
       forM [0.. (fromIntegral num - 1)] $ \i ->
           do let ptr = plusPtr hp $ (fromIntegral offset)
                                + i * (#size struct iqmvertexarray)
              vatype <- (#peek struct iqmvertexarray, type) ptr :: IO CUInt
              format <- (#peek struct iqmvertexarray, format) ptr :: IO CUInt
              offset <- (#peek struct iqmvertexarray, offset) ptr :: IO CUInt
              size <- (#peek struct iqmvertexarray, size) ptr :: IO CUInt
              let vertexPtr = plusPtr hp $ fromIntegral offset
              VertexArray <$> (#peek struct iqmvertexarray, flags) ptr
                          <*> do pure $ if | vatype == 0 -> PositionBuffer vertexPtr
                                           | vatype == 1 -> TexCoordBuffer vertexPtr
                                           | vatype == 2 -> NormalBuffer vertexPtr
                                           | vatype == 3 -> TangentBuffer vertexPtr
                                           | vatype == 4 -> BlendIndexesBuffer vertexPtr
                                           | vatype == 5 -> BlendWeightsBuffer vertexPtr
                                           | vatype == 6 -> ColorBuffer vertexPtr
                                           | vatype >= 0x10 -> CustomBuffer vertexPtr 
                                                                            vatype 
                                                                            format
                                                                            size

