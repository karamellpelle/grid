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
module Game.LevelPuzzleMode.Output.Fancy.Draw
  (
    drawSpaceBox,
    drawSpaceBoxColor,
    drawWall,
    drawDotPlain,
    drawDotBonus,
    drawDotTele,
    drawDotFinish,

  ) where

import MyPrelude
import Game

import Game.Helpers
import Game.GameData
import Game.Data.Color
import Game.Grid.GridData
import Game.Grid.Output
import Game.LevelPuzzleMode.LevelPuzzleWorld

import qualified Graphics.Rendering.OpenGL as GL
import OpenGL as GL
import OpenGL.Helpers as GL
import OpenGL.Helpers.Drawings as GL


--------------------------------------------------------------------------------
--  drawSpaceBox

drawSpaceBox = do
    glClearColor 0 0 0 0
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]

drawSpaceBoxColor (Color r g b a) = do
    glClearColor (rTF $ darken * r) (rTF $ darken * g) (rTF $ darken * b) (rTF $ darken * a)
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    where
      darken = 0.4


--------------------------------------------------------------------------------
--  


drawWall alpha gamedata wall = do
   let Node n0 n1 n2 = wallNode wall
       Node x0 x1 x2 = wallX wall
       Node y0 y1 y2 = wallY wall

   glColor4f 1 0.4 0 alpha
   glBegin gl_TRIANGLE_STRIP
   glVertex3f (fI $ n0 + y0) (fI $ n1 + y1) (fI $ n2 + y2) 
   glVertex3f (fI $ n0) (fI $ n1) (fI $ n2)
   glVertex3f (fI $ n0 + x0 + y0) (fI $ n1 + x1 + y1) (fI $ n2 + x2 + y2)
   glVertex3f (fI $ n0 + x0) (fI $ n1 + x1) (fI $ n2 + x2)
   unless (wallIsDouble wall) $ glColor4f 0.4 1 0 1
   glVertex3f (fI $ n0 + x0 + y0) (fI $ n1 + x1 + y1) (fI $ n2 + x2 + y2)
   glVertex3f (fI $ n0 + x0) (fI $ n1 + x1) (fI $ n2 + x2)
   glVertex3f (fI $ n0 + y0) (fI $ n1 + y1) (fI $ n2 + y2) 
   glVertex3f (fI $ n0) (fI $ n1) (fI $ n2)
   glEnd



 

drawDotPlain alpha gamedata dot = do
    let colormap = griddataColorMap $ gamedataGridData gamedata
    let Color r g b a = colormapAt colormap $ dotplainRoom dot
    let Node x y z = dotplainNode dot

    glPushMatrix
    glColor4f r g b (alpha * a)
    glTranslatef (fI x) (fI y) (fI z) 
    GL.scaleXYZ 0.1
    GL.drawSphere
    glPopMatrix

    
drawDotBonus alpha gamedata dot = do
    let Node x y z = dotbonusNode dot

    glPushMatrix
    glColor4f 0.5 0.5 0.7 alpha
    glTranslatef (fI x) (fI y) (fI z) 
    GL.scaleXYZ 0.1
    GL.drawCube
    glPopMatrix



drawDotTele alpha gamedata dot = do
    let Node x y z = dotteleNode dot
        Node x' y' z' = dotteleNode' dot

    glPushMatrix
    glColor4f 0.1 0.1 0.8 alpha
    glTranslatef (fI x) (fI y) (fI z) 
    GL.scaleXYZ 0.07
    GL.drawCube
    glPopMatrix

    glPushMatrix
    glColor4f 0.8 0.1 0.1 alpha
    glTranslatef (fI x') (fI y') (fI z') 
    GL.scaleXYZ 0.07
    GL.drawCube
    glPopMatrix


drawDotFinish alpha gamedata dot = do
    let Node x y z = dotfinishNode dot

    glPushMatrix
    glColor4f 1.0 1.0 1.0 alpha
    glTranslatef (fI x) (fI y) (fI z) 
    GL.scaleXYZ 0.2
    GL.drawSphere
    glPopMatrix

    
