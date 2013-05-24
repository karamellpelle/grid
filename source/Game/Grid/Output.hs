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
module Game.Grid.Output
  (
    cameraModelMat4,
    cameraViewMat4,

    mat4TranslateNode,
    mat4Turn,
    mat4RotateTurn,
    mat4Segment,

#ifdef GRID_STYLE_FANCY
    module Game.Grid.Output.Fancy,
#endif
#ifdef GRID_STYLE_PLAIN
    module Game.Grid.Output.Plain
#endif

  ) where

import MyPrelude
import Game
import Game.Grid

import OpenGL
import OpenGL.Helpers

#ifdef GRID_STYLE_FANCY
import Game.Grid.Output.Fancy
#endif
#ifdef GRID_STYLE_PLAIN
import Game.Grid.Output.Plain
#endif


--------------------------------------------------------------------------------
--  

-- how to typically output to Screen:
-- 
-- 0. setup screen:                                             done by Scene
-- 1. define projection matrix (a view property of GL camera):  sceneProj2D/sceneProj3D
-- 2. define modelview matrix (GL camera placement):            cameraViewMat4/cameraModelMat4 
-- 3. set shaders using these matrices, and draw


--------------------------------------------------------------------------------
--  


-- | view matrix from Camera
cameraViewMat4 :: Camera -> Mat4
cameraViewMat4 camera = 
    let View a b c = cameraView camera
        View a' b' c' = cameraViewIdeal camera
        
        a'' = smooth a a' $ cameraViewAAlpha camera
        b'' = smooth b b' $ cameraViewBAlpha camera
        c'' = smooth c c' $ cameraViewCAlpha camera
    in  mat4ViewABC (tau * 0.25 + a'') b'' c''


-- | model matrix from camera
cameraModelMat4 :: Camera -> Mat4
cameraModelMat4 camera =
    let
        -- TurnB:
        turnB = turnInverse $ cameraTurnB camera
        turnB' = turnInverse $ cameraTurnBIdeal camera
        matTurnB = mat4SmoothTurn turnB turnB' $ cameraTurnBAlpha camera

        -- TurnA:
        turnA = turnInverse $ cameraTurnA camera
        turnA' = turnInverse $ cameraTurnAIdeal camera
        matTurnA = mat4SmoothTurn turnA turnA' $ cameraTurnAAlpha camera

        -- translate:
        node = cameraNode camera
        node' = cameraNodeIdeal camera
        Vec4 x y z w = smoothNode node node' $ cameraNodeAlpha camera
        matTranslate = mat4TranslationAnti x y z w 

    in  matTurnB `mappend` matTurnA `mappend` matTranslate




mat4Turn :: Turn -> Mat4
mat4Turn (Turn a0 a1 a2 
               b0 b1 b2 
               c0 c1 c2) = 
    Mat4 (fI a0) (fI a1) (fI a2) 0
         (fI b0) (fI b1) (fI b2) 0
         (fI c0) (fI c1) (fI c2) 0
         0       0       0       1


mat4RotateTurn :: Mat4 -> Turn -> Mat4
mat4RotateTurn mat turn =
    mat `mappend` mat4Turn turn


mat4TranslateNode :: Mat4 -> Node -> Mat4
mat4TranslateNode mat (Node x y z) =
    case Mat4 1       0     0       0
              0       1     0       0
              0       0     1       0
              (fI x) (fI y) (fI z)  1 of

        mat' -> mat `mappend` mat'


mat4Segment :: Segment -> Mat4
mat4Segment (Segment (Node x y z) (Turn a0 a1 a2 
                                        b0 b1 b2 
                                        c0 c1 c2)) = 
    Mat4 (fI a0) (fI a1) (fI a2) 0
         (fI b0) (fI b1) (fI b2) 0
         (fI c0) (fI c1) (fI c2) 0
         (fI x)  (fI y)  (fI z)  1





--------------------------------------------------------------------------------
--  

smoothNode :: Node -> Node -> Float -> Vec4
smoothNode (Node x y z) (Node x' y' z') alpha =
    let a = 1 - alpha
        a' = alpha
    in  Vec4 (a * fromIntegral x + a' * fromIntegral x')
             (a * fromIntegral y + a' * fromIntegral y')
             (a * fromIntegral z + a' * fromIntegral z')
             1


-- | note: this will only work if no column c in turn is mapped to -c in turn' !
mat4SmoothTurn :: Turn -> Turn -> Float -> Mat4
mat4SmoothTurn turn turn' alpha =
    let Turn x0 x1 x2 
             y0 y1 y2
             z0 z1 z2 = turn
        Turn x0' x1' x2'
             y0' y1' y2'
             z0' z1' z2' = turn'
        a = (1 - alpha)
        a' = alpha
        x0'' = a * (fromIntegral x0) + a' * (fromIntegral x0')
        x1'' = a * (fromIntegral x1) + a' * (fromIntegral x1')
        x2'' = a * (fromIntegral x2) + a' * (fromIntegral x2')
        y0'' = a * (fromIntegral y0) + a' * (fromIntegral y0')
        y1'' = a * (fromIntegral y1) + a' * (fromIntegral y1')
        y2'' = a * (fromIntegral y2) + a' * (fromIntegral y2')
        z0'' = a * (fromIntegral z0) + a' * (fromIntegral z0')
        z1'' = a * (fromIntegral z1) + a' * (fromIntegral z1')
        z2'' = a * (fromIntegral z2) + a' * (fromIntegral z2')
        xscale = scale x0'' x1'' x2''
        yscale = scale y0'' y1'' y2''
        zscale = scale z0'' z1'' z2''
 
    in
        Mat4 (xscale * x0'') (xscale * x1'') (xscale * x2'') 0
             (yscale * y0'') (yscale * y1'') (yscale * y2'') 0
             (zscale * z0'') (zscale * z1'') (zscale * z2'') 0
             0    0    0    1

    where
        scale a0 a1 a2 =
            1 / (sqrt $ a0 * a0 + a1 * a1 + a2 * a2) -- note: fails if zero


