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
module Game.TestGUI
  (
    TestGUI (..),

    iterationTestGUI,
    makeTestGUI,

  ) where

import MyPrelude
import Game
import Game.GUI
import Game.GUI.Widget.Output
import Data.List

import OpenGL
import OpenGL.Helpers


import Game.GUI.Widget.TextWidget
import Game.GUI.Widget.ActiveButtonWidget
import Game.GUI.Widget.LabelWidget
import Game.GUI.Widget.ScreenWidget
import Game.GUI.Widget.ScaleWidget
import Game.GUI.Widget.MinWidget
import Game.GUI.Widget.MaxWidget
import Game.GUI.Widget.BoardWidget
--import Game.GUI.Widget.ButtonWidget
import Game.GUI.Widget.ScrollWidget
import Game.GUI.Widget.NumberWidget
import Game.GUI.Widget.SlideWidget

import Game.GUI.Widget.StaticLayout


data TestGUI =
    TestGUI
    {
        testAlpha :: !Float,
        testString :: !String,
        testValueFinish :: !Bool
    }


makeTestGUI :: MEnv' TestGUI
makeTestGUI = 
    return  TestGUI
            {
                testAlpha = 1.0,
                testString = "",
                testValueFinish = False
            }

testPushString :: TestGUI -> String -> TestGUI
testPushString test str =
    test
    {
        testString = testString test ++ str
    }

testSetAlpha :: TestGUI -> Float -> TestGUI
testSetAlpha test alpha =
    test { testAlpha = alpha }

testFinish :: TestGUI -> TestGUI
testFinish test =
    test { testValueFinish = True }

testClear :: TestGUI -> TestGUI
testClear test =
    test { testValueFinish = False }

testIsString :: TestGUI -> Bool
testIsString test =
    not $ null $ testString test

testIsFinish :: TestGUI -> Bool
testIsFinish = 
    testValueFinish


--------------------------------------------------------------------------------
--  iterationTestGUI

iterationTestGUI :: Iteration' TestGUI
iterationTestGUI = 
    makeIteration' $ \test -> do
        io $ putStrLn "iterationTestGUI"
        gd <- fmap gamedataGUIData resourceGet

        let label = makeLabelWidget gd "SpaceTribe"
            number = makeNumberWidget gd (-15) (15) 0 `onNewNumber` 
                      (\n test -> testPushString test ("NumberWidget number: " ++ show n))
            slide = makeSlideWidget gd wth 0.0 1.0 `onNewValue`
                      (\v test -> testSetAlpha test v)
            text = makeTextWidgetSplit gd 16 
                      "Haskell Game\n a good choice if you want to be bugfree."
            scroll = makeScrollWidget gd (GUIShape 0.3 0.2) text
            activebutton = (makeActiveButtonWidget gd $ makeLabelWidget gd "finish all")
                           `onPressA` (\test -> testFinish test)
            screen = makeScreenWidget gd 0.5 0.5 board
            
            wth = valueGUIBoardWth 
            board = makeBoardWidgetBackStaticLayout gd $ 
                    ygatherLeft4  ( 
                                    yfillTop 0.08 $ ygatherLeft2 
                                      (makeLabelWidget gd "Intensity")
                                      slide
                                  )
                                  ( 
                                    yfillTop 0.08 $ xapart wth 
                                      (makeLabelWidget gd "Follow Turn")
                                      number
                                  )
                                  ( 
                                    yfillTop 0.08 $ xapart wth
                                      (makeLabelWidget gd "LevelPuzzle")
                                      number
                                  )
                                  (
                                    yfillTop 0.08 $ xapart wth
                                      (makeLabelWidget gd "Memory")
                                      number
                                  )

        iteration' (iterationTestGUI' screen) test


iterationTestGUI' :: Widget w => w TestGUI -> Iteration' TestGUI
iterationTestGUI' w =
    testIteration $ \test -> do
        -- test font
        sh <- fmap gamedataFontShade resourceGet
        fd <- fmap gamedataFontData resourceGet
        io $ do
          fontShade sh 1.0 $ mat4Ortho 0 1.0 0.75 0 (-1) 1
          fontDrawDefault sh fd 0.1 (makeFontColorFloat 0.0 0.9 0.0 0.7)
          fontDraw2D sh fd 0.2 0.2 "Hei Font"

        iterateGUI (testAlpha test) w (testClear test) >>= \(w', test') -> do
            when (testIsString test') $ 
                io $ putStrLn $ testString test'
            
            if testIsFinish test' 
              then return (test', [])
              else return (test', [iterationTestGUI' w'])





--------------------------------------------------------------------------------
--  

testGUIOutput :: MEnv' ()
testGUIOutput = do
    -- 
    (wth, hth) <- screenShape
    gd <- fmap (gamedataGUIData) resourceGet
   
    
    -- query tick
    tick <- tickGet
    io $ do
        let alpha = 1.0
        -- default shader through widgetIterate
        projmodv <- shadeGUI (guidataShadeGUI gd) alpha wth hth
                             (guidataFillTexRepeatScale gd)

        let gs =  GUIState
                  {
                      guistateTick = tick,
                      guistateProjModv = projmodv,
                      guistateAlpha = alpha,
                      guistateWth = wth,
                      guistateHth = hth,
                      guistatePos = GUIPos 0.0 0.0,
                      guistateScaleX = 1.0,
                      guistateScaleY = 1.0,
                      guistateDepth = 0.0,
                      guistateFillTex = 0,
                      guistateTweak = 0.0
                  }

{-
        --
        -- shape
        gs' <- incDepth gd gs
        useNoStencil gd gs'
        gs'' <- plusPos gd gs' (GUIPos 0.4 0.2)
        gs''' <- useFillTexMid gd gs''
        draw4 gd gs''' (GUIShape 0.3 0.2)

        gs''' <- useFillTexBack gd gs''
        drawContour gd gs''' (GUIShape 0.3 0.2)
-}
       
        gs' <- incDepth gd gs
        gs'' <- useFillTexBack gd gs' 
        gs''' <- plusPos gd gs'' (GUIPos 0.1 0.1)
        outputTextWidget gd gs''' $ makeTextWidget gd
                           $ ["hallo alle mann", "hvordan er", "formene?"]

        setGUIState gd gs''
        gs''' <- plusPos gd gs'' (GUIPos 0.3 0.3)
        outputLabelWidget gd gs''' $ makeLabelWidget gd "dette er en label!"

        setGUIState gd gs''
        gs''' <- plusPos gd gs'' (GUIPos 0.2 0.4)
        outputNumberWidget gd gs''' $ makeNumberWidget gd 0 15 0
       
        setGUIState gd gs''
        gs''' <- plusPos gd gs'' (GUIPos 0.5 0.4)
        outputSlideWidget gd gs''' $ makeSlideWidget gd 0.3 0.0 0.5

        -- back
        setDepth gd gs
        let drawBack = True
        when drawBack $ do
          gs' <- useFillTexBack gd gs
          gs'' <- plusPos gd gs' (GUIPos 0.1 0.1)
          drawBorderShape gd gs'' (GUIShape 0.8 0.5)
        



--------------------------------------------------------------------------------
--  testIteration
testIteration :: (a -> MEnv' (a, IterationStack' a)) -> Iteration' a
testIteration iter = 
    makeIteration' $ \test ->  do
        fbo <- screenFBO 
        (wth, hth) <- screenSize
        io $ do
            glBindFramebuffer gl_FRAMEBUFFER fbo 
            glClear $ gl_COLOR_BUFFER_BIT   .|.
                      gl_DEPTH_BUFFER_BIT   .|.
                      gl_STENCIL_BUFFER_BIT 
            glViewport 0 0 (fI wth) (fI hth)

        iter test

