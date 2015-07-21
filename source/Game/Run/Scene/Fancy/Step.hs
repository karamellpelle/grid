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
module Game.Run.Scene.Fancy.Step
  (
    sceneStep,
    sceneStepBegin,
    sceneStepPause,
    sceneStepKonami,

  ) where

import MyPrelude
import Prelude hiding ((*>))
import Game

import Game.Font
import Game.Grid
import Game.Run.RunData
import Game.Run.RunWorld
import Linear
import System.Random

import OpenAL
import OpenAL.Helpers




-- | step default Scene
sceneStep :: GameData -> Tick -> Scene -> MEnv' Scene
sceneStep gamedata tick = \scene -> do

    -- noise
    noise' <- noiseStep gamedata (sceneTweak scene) tick $ sceneNoise scene

    -- tweak
    tweak' <- tweakStep gamedata tick $ sceneTweak scene

    return scene
           {
              sceneTweak = tweak', 
              sceneNoise = noise'
           }



-- | step begin Scene
sceneStepBegin :: GameData -> Tick -> Scene -> MEnv' Scene
sceneStepBegin gamedata tick = \scene -> do

    -- tweak
    tweak' <- tweakStepEmpty gamedata tick $ sceneTweak scene

    return scene
           {
              sceneTweak = tweak'
           }



-- | step pause Scene
sceneStepPause :: GameData -> Tick -> Scene -> MEnv' Scene
sceneStepPause gamedata tick = \scene -> do

    -- tweak
    tweak' <- tweakStep gamedata tick $ sceneTweak scene

    return scene
           {
              sceneTweak = tweak'
           }


-- | step konami Scene
sceneStepKonami :: GameData -> Tick -> Scene -> MEnv' Scene
sceneStepKonami gamedata tick = \scene -> do
    
    -- tweak
    tweak' <- tweakStep gamedata tick $ sceneTweak scene

    return scene
           {
              sceneTweak = tweak'
           }


--------------------------------------------------------------------------------
--  step Tweak


tweakStepEmpty :: GameData -> Tick -> Tweak -> MEnv' Tweak
tweakStepEmpty gamedata tick = \tweak -> do
    return tweak { tweakTick = tick } 


-- | default Tweak step
tweakStep :: GameData -> Tick -> Tweak -> MEnv' Tweak
tweakStep gamedata tick = \tweak -> do
    (ax, ay, az, gx, gy, gz) <- keysAcclGyro
    return $ stepDT ax ay az gx gy gz tick $ maxTick tweak tick

    where
      unitDT = 0.0625
      maxElaps = 1.0

      maxTick tweak tick = 
          if tweakTick tweak + maxElaps <= tick
            then tweak { tweakTick = tweakTick tweak - maxElaps }
            else tweak

      -- ideals:  fast acceleration, when acceleration stops, continue movement
      --          but not as fast. 
      -- fixme: * prevent numbers outside [-1, 1] and NaN
      --        * use Pulse on For/Rot/Osc
      stepDT ax ay az gx gy gz tick = \tweak ->
          let tick' = tweakTick tweak + unitDT
          in  if tick' <= tick
              then let tweak' = helper (rTF unitDT) tweak
                   in  stepDT ax ay az gx gy gz tick $ 
                       tweak' { tweakTick = tick' }
              else tweak

          where
            helper dt = \tweak ->   
              let fAnti = if 1.0 <= fPosAlpha 
                          then fPosAlpha - 1.0 
                          else 0.0
                  rAnti = if 1.0 <= rPosAlpha 
                          then rPosAlpha - 1.0 
                          else 0.0

                  -- For 
                  fAcc  = tweakForAcc tweak 
                  fVel  = tweakForVel tweak
                  fPos  = tweakForPos tweak
                  fAccAlpha = tweakForAccAlpha tweak
                  fVelAlpha = tweakForVelAlpha tweak
                  fPosAlpha = tweakForPosAlpha tweak
                  vecForAcc = Vec4 (ax)
                                   (ay)
                                   (az)
                                   (0.0)
                  fAcc' = (vForI *> vecForAcc)  <+> 
                          (vForJ *> fVel)
                  vecForVel = Vec4 (0.01 * osc0Pos)
                                   (0.01 * osc1Pos)
                                   (0.01 * osc2Pos)
                                   (0.0)
                  fVel' = fVel                  <+> 
                          (dt *> fAcc')         <+>
                          (vecForVel)           <+>
                          (vForK *> vec4Cross fVel fPos)
                  fPos' = fPos                  <+> 
                          (dt *> fVel')         <+>
                          ((vForL * fAnti * fAnti) *> fPos)

                  -- Rot
                  rAcc  = tweakRotAcc tweak 
                  rVel  = tweakRotVel tweak
                  rPos  = tweakRotPos tweak
                  rAccAlpha = tweakRotAccAlpha tweak
                  rVelAlpha = tweakRotVelAlpha tweak
                  rPosAlpha = tweakRotPosAlpha tweak
                  vecRotAcc = Vec4 (gx)
                                   (gy)
                                   (gz)
                                   (0.0)
                  rAcc' = (vRotI *> vecRotAcc)  <+>
                          (vRotJ *> rVel)

                  vecRotVel = Vec4 (0.01 * osc0Pos)
                                   (0.01 * osc1Pos)
                                   (0.01 * osc2Pos)
                                   (0.0)
                  rVel' = rVel                  <+> 
                          (dt *> rAcc')         <+> 
                          (vecRotVel)           <+>
                          (vRotK *> vec4Cross rVel rPos)
                  rPos' = rPos <+> dt *> rVel' <+>
                          ((vRotL * rAnti * rAnti) *> rPos)
                  
                  -- Osc0
                  osc0Acc = tweakOsc0Acc tweak
                  osc0Vel = tweakOsc0Vel tweak + vOsc0L * gx
                  osc0Pos = tweakOsc0Pos tweak
                  osc0Acc' | osc0Pos < (-vOsc0I)  = (vOsc0J)
                           | (vOsc0I) < osc0Pos   = (-vOsc0J)
                           | otherwise            = osc0Acc
                  osc0Vel' = keepInside (-vOsc0K) (vOsc0K) $ osc0Vel + dt * osc0Acc'
                  osc0Pos' = osc0Pos + dt * osc0Vel'

                  -- Osc1
                  osc1Acc = tweakOsc1Acc tweak
                  osc1Vel = tweakOsc1Vel tweak + vOsc1L * gy
                  osc1Pos = tweakOsc1Pos tweak
                  osc1Acc' | osc1Pos < (-vOsc1I)  = (vOsc1J)
                           | (vOsc1I) < osc1Pos   = (-vOsc1J)
                           | otherwise            = osc1Acc
                  osc1Vel' = keepInside (-vOsc1K) (vOsc1K) $ osc1Vel + dt * osc1Acc'
                  osc1Pos' = osc1Pos + dt * osc1Vel'

                  -- Osc2
                  osc2Acc = tweakOsc2Acc tweak
                  osc2Vel = tweakOsc2Vel tweak + vOsc2L * gz
                  osc2Pos = tweakOsc2Pos tweak
                  osc2Acc' | osc2Pos < (-vOsc2I)  = (vOsc2J)
                           | (vOsc2I) < osc2Pos   = (-vOsc2J)
                           | otherwise            = osc2Acc
                  osc2Vel' = keepInside (-vOsc2K) (vOsc2K) $ osc2Vel + dt * osc2Acc'
                  osc2Pos' = osc2Pos + dt * osc2Vel'

                  -- Pulse0
                  pulse0Vel = tweakPulse0Vel tweak
                  pulse0Pos = tweakPulse0Pos tweak
                  pulse0Vel' = max vPulse0I $ pulse0Vel + dt * vPulse0J
                  pulse0Pos' = max 0.0 $ pulse0Pos + dt * pulse0Vel'

                  -- Pulse1
                  pulse1Vel = tweakPulse1Vel tweak
                  pulse1Pos = tweakPulse1Pos tweak
                  pulse1Vel' = max vPulse1I $ pulse1Vel + dt * vPulse1J
                  pulse1Pos' = max 0.0 $ pulse1Pos + dt * pulse1Vel'



#ifdef GRID_TEST_FOR
                  vForI = tweakConstI tweak
                  vForJ = tweakConstJ tweak
                  vForK = tweakConstK tweak
                  vForL = tweakConstL tweak
#else
                  vForI = valueTweakForI
                  vForJ = valueTweakForJ
                  vForK = valueTweakForK
                  vForL = valueTweakForL
#endif
#ifdef GRID_TEST_ROT
                  vRotI = tweakConstI tweak
                  vRotJ = tweakConstJ tweak
                  vRotK = tweakConstK tweak
                  vRotL = tweakConstL tweak
#else
                  vRotI = valueTweakRotI
                  vRotJ = valueTweakRotJ
                  vRotK = valueTweakRotK
                  vRotL = valueTweakRotL
#endif

#ifdef GRID_TEST_OSC0
                  vOsc0I = tweakConstI tweak
                  vOsc0J = tweakConstJ tweak
                  vOsc0K = tweakConstK tweak
                  vOsc0L = tweakConstL tweak
#else
                  vOsc0I = valueTweakOsc0I
                  vOsc0J = valueTweakOsc0J
                  vOsc0K = valueTweakOsc0K
                  vOsc0L = valueTweakOsc0L
#endif
#ifdef GRID_TEST_OSC1
                  vOsc1I = tweakConstI tweak
                  vOsc1J = tweakConstJ tweak
                  vOsc1K = tweakConstK tweak
                  vOsc1L = tweakConstL tweak
#else
                  vOsc1I = 0.821
                  vOsc1J = 0.538
                  vOsc1K = 0.467
                  vOsc1L = 1.0
#endif
#ifdef GRID_TEST_OSC2
                  vOsc2I = tweakConstI tweak
                  vOsc2J = tweakConstJ tweak
                  vOsc2K = tweakConstK tweak
                  vOsc2L = tweakConstL tweak
#else
                  vOsc2I = 0.821
                  vOsc2J = 0.538
                  vOsc2K = 0.467
                  vOsc2L = 1.0
#endif
#ifdef GRID_TEST_PULSE0
                  vPulse0I = tweakConstI tweak
                  vPulse0J = tweakConstJ tweak
#else
                  vPulse0I = valueTweakPulse0I
                  vPulse0J = valueTweakPulse0J
#endif
#ifdef GRID_TEST_PULSE1
                  vPulse1I = tweakConstI tweak
                  vPulse1J = tweakConstJ tweak
#else
                  vPulse1I = valueTweakPulse1I
                  vPulse1J = valueTweakPulse1J
#endif



              in  tweak
                  {
                      tweakForAcc = fAcc',
                      tweakForVel = fVel',
                      tweakForPos = fPos',
                      tweakForAccAlpha = norm fAcc',
                      tweakForVelAlpha = norm fVel',
                      tweakForPosAlpha = norm fPos',

                      tweakRotAcc = rAcc',
                      tweakRotVel = rVel',
                      tweakRotPos = rPos',
                      tweakRotAccAlpha = norm rAcc',
                      tweakRotVelAlpha = norm rVel',
                      tweakRotPosAlpha = norm rPos',
                      
                      tweakOsc0Acc = osc0Acc',
                      tweakOsc0Vel = osc0Vel',
                      tweakOsc0Pos = osc0Pos',
                      tweakOsc1Acc = osc1Acc',
                      tweakOsc1Vel = osc1Vel',
                      tweakOsc1Pos = osc1Pos',
                      tweakOsc2Acc = osc2Acc',
                      tweakOsc2Vel = osc2Vel',
                      tweakOsc2Pos = osc2Pos',

                      tweakPulse0Vel = pulse0Vel',
                      tweakPulse0Pos = pulse0Pos',
                      tweakPulse1Vel = pulse1Vel',
                      tweakPulse1Pos = pulse1Pos
                  }

            norm (Vec4 x y z w) =
                x * x + y * y + z * z + w * w

--------------------------------------------------------------------------------
--  step Noise 



noiseStep :: GameData -> Tweak -> Tick -> Noise -> MEnv' Noise 
noiseStep gamedata tweak tick = \noise -> io $ do
    let sound = rundataSoundScene $ gamedataRunData gamedata
        Vec4 _ _ z _ = tweakForPos tweak
        pitch = pitchF (noisePitch noise) z
        ix = noiseIx noise
        node = noiseNode noise

    noise' <- if (noiseTick noise <= tick) 
              then do
                state <- getSrcState (soundSceneNoiseSrc sound)
                if state == al_PLAYING || state == al_PAUSED
                  then do
                      -- try again a little later
                      return noise { noiseTick = tick + 1.0 }       

                  else do
                      -- setup noise
                      alSourcei (soundSceneNoiseSrc sound) al_BUFFER $ 
                                fI $ noisearrayAt (soundSceneNoiseBufs sound) ix
                      alSourcef (soundSceneNoiseSrc sound) al_PITCH $ rTF pitch
                      let Node x y z = node
                      alSource3f (soundSceneNoiseSrc sound) al_POSITION (fI x) (fI y) (fI z)

                      -- play noise
                      alSourcePlay $ soundSceneNoiseSrc sound
                      
                      dt <- randomTick
                      node <- randomNode
                      ix <- randomIx
                      pitch <- randomPitch

                      return noise
                             {
                                noiseTick = tick + rTF dt,
                                noiseNode = node,
                                noiseIx = ix,
                                noisePitch = pitch
                             }

              else return noise


    -- tweak pitch
    alSourcef (soundSceneNoiseSrc sound) al_PITCH $ rTF pitch
    
    -- tweak pos
    let Node x0 y0 z0 = noiseNode noise
        Vec4 x1 y1 z1 _ = tweakRotPos tweak
        a = fI x0 * x1
        b = fI y0 * y1
        c = fI z0 * z1
    alSource3f (soundSceneNoiseSrc sound) al_POSITION (rTF a) (rTF b) (rTF c)
    
    return noise'

    where
      pitchF pitch = \x -> 
          let a = 0.125 * pitch
              b = 2.5 * pitch
          in  a + 0.5 * (b - a) * (x + 1.0)
      
      randomTick = do
          uni <- randomIO 
          return $ smooth valueSoundSceneNoiseTickMin valueSoundSceneNoiseTickMax uni

      randomNode = do
          x <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
          y <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
          z <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
          return (Node x y z)

      randomIx = do
          randomRIO (0, valueSoundSceneNoiseSize - 1)

      randomPitch = do
          uni <- randomIO
          return $ smooth valueSoundSceneNoisePitchMin valueSoundSceneNoisePitchMax uni
  

