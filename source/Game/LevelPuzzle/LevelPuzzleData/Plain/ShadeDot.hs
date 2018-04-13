-- grid is a game written in Haskell
-- Copyright (C) 2018 karamellpelle@hotmail.com
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
module Game.LevelPuzzle.LevelPuzzleData.Plain.ShadeDot
  (
    ShadeDot (..),

    loadShadeDot,
    unloadShadeDot,

  ) where

import MyPrelude
import File
import Game.Values

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data ShadeDot =
    ShadeDot
    {
        shadeDotPrg :: !GLuint,
        shadeDotUniAlpha :: !GLint,
        shadeDotUniProjModvMatrix :: !GLint,
        shadeDotUniNormalMatrix :: !GLint,
        shadeDotUniPos :: !GLint,
        shadeDotUniColor :: !GLint,
        shadeDotUniRefDir :: !GLint,
    
        shadeDotVAO :: !GLuint,
        shadeDotTexPlain :: !GLuint,
        shadeDotTexBonus :: !GLuint,
        shadeDotTexTele0 :: !GLuint,
        shadeDotTexTele1 :: !GLuint,
        shadeDotTexFinish :: !GLuint
    }



loadShadeDot :: IO ShadeDot
loadShadeDot = do
    vsh <- fileStaticData "shaders/LevelDot.vsh"
    fsh <- fileStaticData "shaders/LevelDot.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attNormal, "a_normal"),
                                (attTexCoord, "a_texcoord") ] [

                                (tex0, "u_tex") ]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uNormalMatrix <- getUniformLocation prg "u_normal_matrix"
    uPos <- getUniformLocation prg "u_pos"
    uAlpha <- getUniformLocation prg "u_alpha"
    uColor <- getUniformLocation prg "u_color"
    uRefDir <- getUniformLocation prg "u_ref_dir"

    vao <- makeVAO
    
    texPlain <- makeTex "LevelPuzzle/Output/dotplain_tex.png"
    texBonus <- makeTex "LevelPuzzle/Output/dotbonus_tex.png"
    texTele0 <- makeTex "LevelPuzzle/Output/dottele0_tex.png"
    texTele1 <- makeTex "LevelPuzzle/Output/dottele1_tex.png"
    texFinish <- makeTex "LevelPuzzle/Output/dotfinish_tex.png"
    
    -- tmp, set ref dir
    glProgramUniform3fEXT prg uRefDir 1.0 0.0 0.0

    return  ShadeDot
            {
                shadeDotPrg = prg,
                shadeDotUniAlpha = uAlpha,
                shadeDotUniColor = uColor,
                shadeDotUniProjModvMatrix = uProjModvMatrix,
                shadeDotUniNormalMatrix = uNormalMatrix,
                shadeDotUniPos = uPos,
                shadeDotUniRefDir = uRefDir,

                shadeDotVAO = vao,
                shadeDotTexPlain = texPlain,
                shadeDotTexBonus = texBonus,
                shadeDotTexTele0 = texTele0,
                shadeDotTexTele1 = texTele1,
                shadeDotTexFinish = texFinish
            }

    where
      makeTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ 
                          fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ 
                          fI gl_NEAREST_MIPMAP_NEAREST
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          path <- fileStaticData path
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path
          glGenerateMipmap gl_TEXTURE_2D 
          return tex



unloadShadeDot :: ShadeDot -> IO ()
unloadShadeDot sh = 
    return ()


-- | vao containing vbo for dot
makeVAO :: IO GLuint
makeVAO = do
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attNormal
    glEnableVertexAttribArray attTexCoord
    vbo <- makeSphereVBO valueLevelPuzzleDotRadius
                         valueLevelPuzzleDotStacks
                         valueLevelPuzzleDotSlices

    glVertexAttribPointer attPos 3 gl_FLOAT gl_FALSE 28
                          (mkPtrGLvoid 0)
    glVertexAttribPointer attNormal 3 gl_FLOAT gl_FALSE 28 
                          (mkPtrGLvoid 12)
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 28
                          (mkPtrGLvoid 24)
    return vao


makeSphereVBO :: Float -> UInt -> UInt -> IO GLuint
makeSphereVBO radius stacks slices = do
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let elemsize = 28
        bytesize = stacks * (2 * (slices + 1) + 2) * elemsize
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ helper 0
    return vbo

    where
      stacks' = fI stacks
      stacksInv' = 1.0 / stacks'
      slices' = fI slices
      slicesInv' = 1.0 / slices'

      helper ix ptr = 
          if ix == stacks
            then return ()
            else case fI ix of
              ix'  -> do
                let (p0x, p0y) = cossin $ (ix' * tau) / (2 * stacks')
                    (p1x, p1y) = cossin $ ((ix' + 1.0) * tau) / (2 * stacks')
                    t0 = ix' * stacksInv'
                    t1 = (ix' + 1.0) * stacksInv'
                ptr' <- writeSlice p0x p0y p1x p1y t0 t1 ptr
                helper (ix + 1) ptr'

      writeSlice p0x p0y p1x p1y t0 t1 ptr = do
          -- begin strip
          ptr' <- pokePair 0 ptr
          -- iterate strip
          helper 0 ptr'
          
          where
            pokePair :: UInt -> Ptr a -> IO (Ptr a)
            pokePair ix ptr = do
                let (z, x) = cossin $ (fI ix * tau) * slicesInv'
                    x0 = x * p0y
                    y0 = p0x
                    z0 = z * p0y
                    x1 = x * p1y
                    y1 = p1x
                    z1 = z * p1y
                pokeByteOff ptr 0 $ (rTF $ radius * x0 :: GLfloat)
                pokeByteOff ptr 4 $ (rTF $ radius * y0 :: GLfloat)
                pokeByteOff ptr 8 $ (rTF $ radius * z0 :: GLfloat)
                pokeByteOff ptr 12 $ (rTF x0 :: GLfloat)
                pokeByteOff ptr 16 $ (rTF y0 :: GLfloat)
                pokeByteOff ptr 20 $ (rTF z0 :: GLfloat)
                pokeByteOff ptr 24 $ normGLushort $ (fI ix) * slicesInv'
                pokeByteOff ptr 26 $ normGLushort $ t0
                pokeByteOff ptr 28 $ (rTF $ radius * x1 :: GLfloat)
                pokeByteOff ptr 32 $ (rTF $ radius * y1 :: GLfloat)
                pokeByteOff ptr 36 $ (rTF $ radius * z1 :: GLfloat)
                pokeByteOff ptr 40 $ (rTF x1 :: GLfloat)
                pokeByteOff ptr 44 $ (rTF y1 :: GLfloat)
                pokeByteOff ptr 48 $ (rTF z1 :: GLfloat)
                pokeByteOff ptr 52 $ normGLushort $ (fI ix) * slicesInv'
                pokeByteOff ptr 54 $ normGLushort $ t1
                return $ plusPtr ptr 56

            helper ix ptr = 
                if ix == slices
                  then do
                    -- write previous element 2 times again
                    let (z, x) = cossin $ (fI ix * tau) * slicesInv'
                        x1 = x * p1y
                        y1 = p1x
                        z1 = z * p1y
                    pokeByteOff ptr  0 $ (rTF $ radius * x1 :: GLfloat)
                    pokeByteOff ptr  4 $ (rTF $ radius * y1 :: GLfloat)
                    pokeByteOff ptr  8 $ (rTF $ radius * z1 :: GLfloat)
                    pokeByteOff ptr 12 $ (rTF x1 :: GLfloat)
                    pokeByteOff ptr 16 $ (rTF y1 :: GLfloat)
                    pokeByteOff ptr 20 $ (rTF z1 :: GLfloat)
                    pokeByteOff ptr 24 $ normGLushort $ (fI ix) * slicesInv'
                    pokeByteOff ptr 26 $ normGLushort $ t1
                    pokeByteOff ptr 28 $ (rTF $ radius * x1 :: GLfloat)
                    pokeByteOff ptr 32 $ (rTF $ radius * y1 :: GLfloat)
                    pokeByteOff ptr 36 $ (rTF $ radius * z1 :: GLfloat)
                    pokeByteOff ptr 40 $ (rTF x1 :: GLfloat)
                    pokeByteOff ptr 44 $ (rTF y1 :: GLfloat)
                    pokeByteOff ptr 48 $ (rTF z1 :: GLfloat)
                    pokeByteOff ptr 52 $ normGLushort $ (fI ix) * slicesInv'
                    pokeByteOff ptr 54 $ normGLushort $ t1
                    return $ plusPtr ptr 56
                  else do
                    ptr' <- pokePair (ix + 1) ptr
                    helper (ix + 1) ptr'
      


