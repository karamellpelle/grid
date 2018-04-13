/*
 *   grid is a game written in Haskell
 *   Copyright (C) 2018 karamellpelle@hotmail.com
 *
 *   This file is part of grid.
 *
 *   grid is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   grid is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with grid.  If not, see <http://www.gnu.org/licenses/>.
*/
#import <GameKit/GameKit.h>


void ios_initPlayers();

void ios_playersAuthenticateLocalPlayer();

//uint ios_playersHandleLocalPlayer(char* ptr_id, uint ptr_id_len, char* ptr_alias, uint ptr_alias_len);

void ios_playersSendAchievement(const char* cat, float alpha);

// types.h: typedef long long int64_t;
void ios_playersSendScore(const char* ach, long long score);





// this is a Objective-C class encapsulating archive functionality
@interface IOSPlayersArchive : NSObject
{
    NSString* path_;
    //NSMutableArray* array_achievement_;
    //NSMutableArray* array_score_;
    
}

-(id) init;

-(void) pushGKScore:(GKScore*) score;

-(void) pushGKAchievement:(GKAchievement*) achievement;

-(void) sendArchive;

-(void) clear;

@end

extern IOSPlayersArchive* theIOSPlayersArchive;

