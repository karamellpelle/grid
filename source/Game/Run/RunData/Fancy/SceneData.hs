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
module Game.Run.RunData.Fancy.SceneData
  (
    SceneData (..),
   
    loadSceneData,
    unloadSceneData,

  ) where


import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade




-- fixme: remove attribute texcoord, compute instead from [-1, 1] coords, 
--        also pos of type vec2
data SceneData =
    SceneData
    {
        scenedataVAO :: !GLuint,
        --shadeSceneVBO :: !GLuinAt

        scenedataTexA :: !GLuint,
        scenedataTexB :: !GLuint,
        scenedataTexC :: !GLuint
    }



loadSceneData :: IO SceneData
loadSceneData = do
  
    vao <- loadVAO

    (texA, texB, texC) <- loadTexABC
    
    return  SceneData
            {
                scenedataVAO = vao,
                scenedataTexA = texA,
                scenedataTexB = texB,
                scenedataTexC = texC
            }


unloadSceneData :: SceneData -> IO ()
unloadSceneData sh = do
    return ()




--------------------------------------------------------------------------------
--  

loadVAO :: IO GLuint
loadVAO = do
    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- vbo 
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 32 nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr (0  + 0)  (-1 :: GLbyte)
        pokeByteOff ptr (0  + 1)  (1 :: GLbyte)
        pokeByteOff ptr (0  + 2)  (0 :: GLbyte)
        pokeByteOff ptr (0  + 3)  (1 :: GLbyte)
        pokeByteOff ptr (0  + 4)  (0 :: GLushort)
        pokeByteOff ptr (0  + 6)  (1 :: GLushort)
        
        pokeByteOff ptr (8  + 0)  (-1 :: GLbyte)
        pokeByteOff ptr (8  + 1)  (-1 :: GLbyte)
        pokeByteOff ptr (8  + 2)  (0 :: GLbyte)
        pokeByteOff ptr (8  + 3)  (1 :: GLbyte)
        pokeByteOff ptr (8  + 4)  (0 :: GLushort)
        pokeByteOff ptr (8  + 6)  (0 :: GLushort)

        pokeByteOff ptr (16 + 0)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 1)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 2)  (0 :: GLbyte)
        pokeByteOff ptr (16 + 3)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 4)  (1 :: GLushort)
        pokeByteOff ptr (16 + 6)  (1 :: GLushort)

        pokeByteOff ptr (24 + 0)  (1 :: GLbyte)
        pokeByteOff ptr (24 + 1)  (-1 :: GLbyte)
        pokeByteOff ptr (24 + 2)  (0 :: GLbyte)
        pokeByteOff ptr (24 + 3)  (1 :: GLbyte)
        pokeByteOff ptr (24 + 4)  (1 :: GLushort)
        pokeByteOff ptr (24 + 6)  (0 :: GLushort)

    glVertexAttribPointer attPos 3 gl_BYTE gl_FALSE 8 $ 
                          mkPtrGLvoid 0
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_FALSE 8 $
                          mkPtrGLvoid 4
  
    return vao
  


loadTexABC :: IO (GLuint, GLuint, GLuint)
loadTexABC = do
    
    path <- fileStaticData "Run/Scene"
    
    texA <- loadTex $ path ++ "/texa.png"
    texB <- loadTex $ path ++ "/tex_b.png"
    texC <- loadTex $ path ++ "/tex_c.png"

    return (texA, texB, texC)

    where
      loadTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_REPEAT
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_REPEAT
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
        
          return tex 
