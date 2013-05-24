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
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenAL.IOS.Values where
    
import OpenAL.IOS.Types

import Foreign.Ptr
import Foreign.C


al_NONE :: ALenum
al_NONE =                                 0

al_FALSE :: ALboolean
al_FALSE =                                0

al_TRUE :: ALboolean
al_TRUE =                                 1

al_SOURCE_RELATIVE :: ALenum                        
al_SOURCE_RELATIVE =                      0x202

al_CONE_INNER_ANGLE :: ALenum                    
al_CONE_INNER_ANGLE =                     0x1001

al_CONE_OUTER_ANGLE :: ALenum
al_CONE_OUTER_ANGLE =                     0x1002

al_PITCH :: ALenum
al_PITCH =                                0x1003
  
al_POSITION :: ALenum
al_POSITION =                             0x1004
  
al_DIRECTION :: ALenum
al_DIRECTION =                            0x1005
  
al_VELOCITY :: ALenum
al_VELOCITY =                             0x1006

al_LOOPING :: ALenum
al_LOOPING =                              0x1007

al_BUFFER :: ALenum
al_BUFFER =                               0x1009
  
al_GAIN :: ALenum
al_GAIN =                                 0x100A

al_MIN_GAIN :: ALenum
al_MIN_GAIN =                             0x100D

al_MAX_GAIN :: ALenum
al_MAX_GAIN =                             0x100E

al_ORIENTATION :: ALenum
al_ORIENTATION =                          0x100F

al_SOURCE_STATE :: ALenum
al_SOURCE_STATE =                         0x1010

al_INITIAL :: ALenum
al_INITIAL =                              0x1011

al_PLAYING :: ALenum
al_PLAYING =                              0x1012

al_PAUSED :: ALenum
al_PAUSED =                               0x1013

al_STOPPED :: ALenum
al_STOPPED =                              0x1014

al_BUFFERS_QUEUED :: ALenum
al_BUFFERS_QUEUED =                       0x1015

al_BUFFERS_PROCESSED :: ALenum
al_BUFFERS_PROCESSED =                    0x1016

al_SEC_OFFSET :: ALenum
al_SEC_OFFSET =                           0x1024

al_SAMPLE_OFFSET :: ALenum
al_SAMPLE_OFFSET =                        0x1025

al_BYTE_OFFSET :: ALenum
al_BYTE_OFFSET =                          0x1026

al_SOURCE_TYPE :: ALenum
al_SOURCE_TYPE =                          0x1027

al_STATIC :: ALenum
al_STATIC =                               0x1028

al_STREAMING :: ALenum
al_STREAMING =                            0x1029

al_UNDETERMINED :: ALenum
al_UNDETERMINED =                         0x1030

al_FORMAT_MONO8 :: ALenum
al_FORMAT_MONO8 =                         0x1100

al_FORMAT_MONO16 :: ALenum
al_FORMAT_MONO16 =                        0x1101

al_FORMAT_STEREO8 :: ALenum
al_FORMAT_STEREO8 =                       0x1102

al_FORMAT_STEREO16 :: ALenum
al_FORMAT_STEREO16 =                      0x1103

al_REFERENCE_DISTANCE :: ALenum
al_REFERENCE_DISTANCE =                   0x1020

al_ROLLOFF_FACTOR :: ALenum
al_ROLLOFF_FACTOR =                       0x1021

al_CONE_OUTER_GAIN :: ALenum
al_CONE_OUTER_GAIN =                      0x1022

al_MAX_DISTANCE :: ALenum
al_MAX_DISTANCE =                         0x1023

al_FREQUENCY :: ALenum
al_FREQUENCY =                            0x2001

al_BITS :: ALenum
al_BITS =                                 0x2002

al_CHANNELS :: ALenum
al_CHANNELS =                             0x2003

al_SIZE :: ALenum
al_SIZE =                                 0x2004

al_UNUSED :: ALenum
al_UNUSED =                               0x2010

al_PENDING :: ALenum
al_PENDING =                              0x2011

al_PROCESSED :: ALenum
al_PROCESSED =                            0x2012

al_NO_ERROR :: ALenum
al_NO_ERROR =                             0x0000

al_INVALID_NAME :: ALenum
al_INVALID_NAME =                         0xA001

al_INVALID_ENUM :: ALenum
al_INVALID_ENUM =                         0xA002

al_INVALID_VALUE :: ALenum
al_INVALID_VALUE =                        0xA003

al_INVALID_OPERATION :: ALenum
al_INVALID_OPERATION =                    0xA004

al_OUT_OF_MEMORY :: ALenum
al_OUT_OF_MEMORY =                        0xA005

al_VENDOR :: ALenum
al_VENDOR =                               0xB001

al_VERSION :: ALenum
al_VERSION =                              0xB002

al_RENDERER :: ALenum
al_RENDERER =                             0xB003

al_EXTENSIONS :: ALenum
al_EXTENSIONS =                           0xB004

al_DOPPLER_FACTOR :: ALenum
al_DOPPLER_FACTOR =                       0xC000

al_DOPPLER_VELOCITY :: ALenum
al_DOPPLER_VELOCITY =                     0xC001

al_SPEED_OF_SOUND :: ALenum
al_SPEED_OF_SOUND =                       0xC003

al_DISTANCE_MODEL :: ALenum
al_DISTANCE_MODEL =                       0xD000

al_INVERSE_DISTANCE :: ALenum
al_INVERSE_DISTANCE =                     0xD001

al_INVERSE_DISTANCE_CLAMPED :: ALenum
al_INVERSE_DISTANCE_CLAMPED =             0xD002

al_LINEAR_DISTANCE :: ALenum
al_LINEAR_DISTANCE =                      0xD003

al_LINEAR_DISTANCE_CLAMPED :: ALenum
al_LINEAR_DISTANCE_CLAMPED =              0xD004

al_EXPONENT_DISTANCE :: ALenum
al_EXPONENT_DISTANCE =                    0xD005

al_EXPONENT_DISTANCE_CLAMPED :: ALenum
al_EXPONENT_DISTANCE_CLAMPED =            0xD006
