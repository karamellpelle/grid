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
module LevelTools.Output
  (
    outputEdit,

  ) where

import MyPrelude
import Game

import Game.Grid.Output
import Game.Grid.GridData
import LevelTools.EditWorld
import Game.LevelPuzzleMode.Output.Fancy.Draw
import Game.Grid.Output.Draw

import OpenGL as GL
import qualified Graphics.Rendering.OpenGL as GL
import Game.Data.Color
import Data.List
import Game.Helpers.Shade

-- tmp
import Graphics.UI.GLFW
import OpenGL.Helpers as GL
import OpenGL.Helpers.Drawings as GL





outputEdit :: s -> EditWorld -> b -> MEnv' (s, EditWorld, b)
outputEdit s edit b = do
    gamedata <- resourceGameData
    (swth, shth) <- screenSize
    (wth, hth) <- screenShape 
    io $ do

        glViewport 0 0 (fI swth) (fI shth)

        -- 3D --
        let proj = mat4Perspective valuePerspectiveFOVY
                                   (wth / hth)
                                   valuePerspectiveNear
                                   valuePerspectiveFar
            modv = mat4SceneCamera $ editGrid edit
            projmodv = proj `mappend` modv

        glEnable gl_DEPTH_TEST
        glEnable gl_CULL_FACE
        setProjModV proj modv

        let colormap = griddataColorMap $ gamedataGridData gamedata
            room = scontentRoom $ editSemiContent edit
        drawSpaceBoxColor $ colormapAt colormap room
       
        -- ground
        drawMyGround edit
        
        -- orig 
        drawOrigo 

        drawCurrentRoom gamedata (editSemiContent edit) room
        
        -- draw type
        drawTypeNode gamedata edit

        
        -- 2D --
        let proj = mat4Ortho2D 0 (fI swth) (- fI shth) 0
        glDisable gl_CULL_FACE
        glDisable gl_DEPTH_TEST
        setProjModV proj mempty
       
        drawEditText edit
    
    return (s, edit, b)







--------------------------------------------------------------------------------
--  origo

drawOrigo = do
    glPushMatrix
    glColor4f 0.0 0.8 0.8 0.7
    glLineWidth 2.0
    glBegin gl_LINES
    glVertex3f 0 0 0
    glVertex3f 0.5 0 0 
    glEnd
    glLineWidth 1.0

    glColor4f 1.0 1.0 1.0 0.7
    --glTranslatef 0.0 
    GL.scaleXYZ 0.05
    GL.drawSphere
    glPopMatrix
    
--------------------------------------------------------------------------------
--  drawMyGround

drawMyGround edit = do
    let Node x0 y0 z0 = cameraNode $ gridCamera $ editGrid edit
        Node x1 y1 z1 = cameraNodeIdeal $ gridCamera $ editGrid edit
        alpha = cameraNodeAlpha $ gridCamera $ editGrid edit
        x = smooth x0 x1 $ rTF alpha
        y = smooth y0 y1 $ rTF alpha
        z = smooth z0 z1 $ rTF alpha

-- tmp
    glColor4f 1 1 1 0.4
    glBegin gl_LINES
    forM_ [-radius..radius] $ \r -> do
        glVertex3f (x + fI r) y (z - fI radius) 
        glVertex3f (x + fI r) y (z + fI radius) 
    forM_ [-radius..radius] $ \r -> do
        glVertex3f (x - fI radius) y (z + fI r)
        glVertex3f (x + fI radius) y (z + fI r)
    glEnd
--
    where
      smooth x x' a =
          (1.0 - a) * (fI x) + a * (fI x') 
      radius = 5 :: Int



--------------------------------------------------------------------------------
--  drawCurrentRoom

drawCurrentRoom gd scontent room = do
    case find (\r -> sroomRoomIx r == room) (scontentRooms scontent) of
        Nothing -> return ()
        Just r  -> do
           mapM_ (drawWall 1.0 gd) $ sroomWall r
           mapM_ (drawDotPlain 1.0 gd) $ sroomDotPlain r
           mapM_ (drawDotBonus 1.0 gd) $ sroomDotBonus r
           mapM_ (drawDotTele 1.0 gd) $ sroomDotTele r
           mapM_ (drawDotFinish 1.0 gd) $ sroomDotFinish r
    


--------------------------------------------------------------------------------
--  drawEditText

drawEditText edit = io $ do
     
    glColor4f 1 1 1 1
    drawStrings $ [ "Message:            " ++ editMessage edit,
                    "PuzzleTag:          " ++ show (levelPuzzleTag $ editLevel edit),
                    "Segments:           " ++ show (levelSegments $ editLevel edit),
                    "Room:               " ++ show (scontentRoom $ editSemiContent edit),
                    "Node:               " ++ showNode (editNode edit) ] ++
                  strsType (editEditType edit)

    where
      drawStrings strs = do
          --foldM (\y str -> drawString 0 y str >> return (y + 20)) 20 strs
          foldM (\y str -> drawString 0 y str >> return (y - 20)) (-20) strs

      drawString x y str = do
          glPushMatrix
          glTranslatef (rTF x) (rTF y) 0.0
          renderString Fixed8x16 str
          glPopMatrix 


      strsType TypeDotPlain = do
          [ "TypeDotPlain", "Size: " ++ show (dotplainSize $ editDotPlain edit),
                            "Room:  " ++ show (dotplainRoom $ editDotPlain edit) ]

      strsType TypeDotBonus = do
          [ "TypeDotBonus", "Size:  " ++ show (dotbonusSize $ editDotBonus edit), 
                            "Add:   " ++ show (dotbonusAdd $ editDotBonus edit) ]

      strsType TypeDotTele = do
          [ "TypeDotTele",  "Size:  " ++ show (dotteleSize $ editDotTele edit) ]

      strsType TypeDotFinish = do
          [ "TypeDotFinish" ]

      strsType TypeWall = do
          [ "TypeWall",     "IsDouble: " ++ show (wallIsDouble $ editWall edit) ]

      strsType _        = do
          [ "(no type)" ]

      showNode (Node x y z) =
          show x ++ " " ++ show y ++ " " ++ show z
         


--------------------------------------------------------------------------------
--  drawTypeNode

drawTypeNode gd edit = case editEditType edit of
    TypeDotPlain    -> drawTypeDotPlain gd edit
    TypeDotBonus    -> drawTypeDotBonus gd edit
    TypeDotTele     -> drawTypeDotTele  gd edit
    TypeDotFinish   -> drawTypeDotFinish gd edit
    TypeWall        -> drawTypeWall gd edit
    _               -> return ()

    where
      alpha = 0.6

      drawTypeDotPlain gamedata edit = do
          let colormap = griddataColorMap $ gamedataGridData gamedata
          let Color r g b a = colormapAt colormap $ dotplainRoom $ editDotPlain edit
          let Node x y z = editNode edit
          glPushMatrix
          glColor4f r g b (alpha * a)
          glTranslatef (fI x) (fI y) (fI z) 
          GL.scaleXYZ 0.1
          GL.drawSphere
          glPopMatrix
          
      drawTypeDotBonus gamedata edit = do
          let Node x y z = editNode edit
          glPushMatrix
          glColor4f 0.5 0.5 0.7 alpha
          glTranslatef (fI x) (fI y) (fI z) 
          GL.scaleXYZ 0.1
          GL.drawCube
          glPopMatrix

      drawTypeDotTele gamedata edit = case editPushedNodes edit of
          (n:ns)  -> do
              let Node x y z = n
                  Node x' y' z' = editNode edit

              glPushMatrix
              glColor4f 0.1 0.1 0.8 1.0
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
          
          []      -> do
              let Node x y z = editNode edit
              glPushMatrix
              glColor4f 0.1 0.1 0.8 alpha
              glTranslatef (fI x) (fI y) (fI z) 
              GL.scaleXYZ 0.07
              GL.drawCube
              glPopMatrix

      
      drawTypeDotFinish gamedata edit = do
          let Node x y z = editNode edit
          glPushMatrix
          glColor4f 1.0 1.0 1.0 alpha
          glTranslatef (fI x) (fI y) (fI z) 
          GL.scaleXYZ 0.2
          GL.drawSphere
          glPopMatrix


      drawTypeWall gamedata edit = case editPushedNodes edit of
          (n0:n1:ns)  -> do
              let Node x0 y0 z0 = n0
                  Node x1 y1 z1 = n1
                  Node x2 y2 z2 = editNode edit
              glPushMatrix
              glColor4f 1.0 1.0 1.0 1.0
              glTranslatef (fI x0) (fI y0) (fI z0) 
              GL.scaleXYZ 0.05
              GL.drawSphere
              glPopMatrix
              glPushMatrix
              glColor4f 1.0 1.0 1.0 1.0
              glTranslatef (fI x1) (fI y1) (fI z1) 
              GL.scaleXYZ 0.05
              GL.drawSphere
              glPopMatrix
              glPushMatrix
              glColor4f 1.0 1.0 1.0 alpha
              glTranslatef (fI x2) (fI y2) (fI z2) 
              GL.scaleXYZ 0.05
              GL.drawSphere
              glPopMatrix

          (n0:ns)     -> do
              let Node x0 y0 z0 = n0
                  Node x1 y1 z1 = editNode edit
              glPushMatrix
              glColor4f 1.0 1.0 1.0 1.0
              glTranslatef (fI x0) (fI y0) (fI z0) 
              GL.scaleXYZ 0.05
              GL.drawSphere
              glPopMatrix
              glPushMatrix
              glColor4f 1.0 1.0 1.0 alpha
              glTranslatef (fI x1) (fI y1) (fI z1) 
              GL.scaleXYZ 0.05
              GL.drawSphere
              glPopMatrix

          []          -> do
              let Node x0 y0 z0 = editNode edit
              glPushMatrix
              glColor4f 1.0 1.0 1.0 alpha
              glTranslatef (fI x0) (fI y0) (fI z0) 
              GL.scaleXYZ 0.05
              GL.drawSphere
              glPopMatrix


 
{-
    -- setup scene
    (wth, hth) <- setupScreen
    let projection = mat4Perspective valueModePerspectiveFOVY
                                     (wth / hth)
                                     valueModePerspectiveNear 
                                     valueModePerspectiveFar
        modelview = sceneCamera $ editGrid edit

    -- 3D

    shade3D projection modelview
    
    drawGround mempty --(editNode edit)

-- tmp
    io $ do
        glColor4f 1 1 1 1
        glPushMatrix
        glScalef 0.08 0.08 0.08
        GL.drawSphere
        glPopMatrix
--

    forM_ (editLevelDots edit) $ drawDot modelview 

    shade3DModelview modelview
    forM_ (editLevelWalls edit) $ drawWall modelview



    -- draw pushed nodes
    forM_ (editPushedNodes edit) $ \(Node x y z) -> do
        shade3DModelview $ mat4Translate3 modelview (fromIntegral x)
                                                    (fromIntegral y)
                                                    (fromIntegral z)

-- tmp
        io $ do
          glPushMatrix
          GL.color4 0 0 1 1
          GL.scaleXYZ 0.11
          GL.drawSphere
          glPopMatrix
--

    -- draw node
    let Node x y z = editNode edit
    shade3DModelview $ mat4Translate3 modelview (fromIntegral x) 
                                                (fromIntegral y)
                                                (fromIntegral z)
-- tmp
    io $ do 
        glPushMatrix
        GL.color4 0 1 0 1
        GL.scaleXYZ 0.05
        GL.drawSphere
        glPopMatrix
--    
    -- 2D

    -- negate in y-direction, since GLFW draws from bottom and up
    (wth, hth) <- screenSize
    (projection, modelview) <- shade2D (fromIntegral wth) (-(fromIntegral hth))

    io $ glColor4f 1 1 1 1
    drawStrings modelview $ [ "name:          " ++ editLevelName edit,
                              "max segments:  " ++ show (editLevelMaxSegments edit),
                              "eat ord:       " ++ if editIsEatOrd edit then (show $ editEatOrd edit) 
                                                                        else "Nothing",
                              "type:          " ++ show (editType edit),
                              "type value:    " ++ show (editTypeValue edit),
                              "node:          " ++ show (editNode edit) ]

    return b



drawStrings :: Mat4 -> [String] -> MEnv' ()
drawStrings modelview strs = do
    foldM (\y str -> drawString modelview 0 y str >> return (y - 20)) (-20) strs
    return ()

drawString modelview x y str = do
    shade2DModelview $ mat4Translate2 modelview (realToFrac x) (realToFrac y)
    io $ renderString Fixed8x16 str



--------------------------------------------------------------------------------
--  
-- | draw wall into world
drawWall modelview wall = io $ do
    glDisable gl_CULL_FACE
    let Node n0 n1 n2 = wallNode wall
        Node x0 x1 x2 = wallX wall
        Node y0 y1 y2 = wallY wall

-- tmp
    glColor4f 0 0 1 0.15
    glBegin gl_TRIANGLE_STRIP
    glVertex3f (fromIntegral $ n0) (fromIntegral $ n1) (fromIntegral $ n2)
    glVertex3f (fromIntegral $ n0 + x0) (fromIntegral $ n1 + x1) (fromIntegral $ n2 + x2)
    glVertex3f (fromIntegral $ n0 + y0) (fromIntegral $ n1 + y1) (fromIntegral $ n2 + y2) 
    glVertex3f (fromIntegral $ n0 + x0 + y0) (fromIntegral $ n1 + x1 + y1) (fromIntegral $ n2 + x2 + y2)
    glEnd
--



-- | draw dot into world
drawDot modelview dot = do
    case dotSpeciality dot of
        DotSimple     -> drawSimpleDot modelview dot
        DotTele node' -> drawTeleDot modelview dot node'
        DotBonus n    -> drawBonusDot modelview dot n


drawSimpleDot modelview dot = do
    let Node x y z = dotNode dot
    shade3DModelview $ mat4Translate3 modelview (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral z)

-- tmp
    io $ do
      glPushMatrix
      glColor4f 1 0.9 0 1
      GL.scaleXYZ 0.1
      GL.drawSphere
      glPopMatrix
--

drawTeleDot modelview dot node' = do
    let Node x y z = dotNode dot
    shade3DModelview $ mat4Translate3 modelview (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral z)

-- tmp
    io $ do
      glPushMatrix
      GL.scaleXYZ (0.125)
      glColor4f 0.78125 0.6328125 0.78125 0.2
      GL.drawCube
      glPopMatrix
--
    let Node x' y' z' = node'
    shade3DModelview $ mat4Translate3 modelview (fromIntegral x') 
                                                (fromIntegral y')
                                                (fromIntegral z')

-- tmp
    io $ do
      glPushMatrix
      GL.scaleXYZ (0.100)
      GL.color4 0.88125 0.6328125 0.68125 0.2
      GL.drawCube
      glPopMatrix
--

drawBonusDot modelview dot n = do
    let Node x y z = dotNode dot
    shade3DModelview $ mat4Translate3 modelview (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral z)

-- tmp
    io $ do
      glPushMatrix
      GL.scaleXYZ 0.08
      case dotEatOrd dot of
          Nothing   -> GL.color4 1 0 1 0.2
          Just ord' -> GL.color4 1 0 0.6 1
      GL.drawSphere
      glPopMatrix
--

-}
