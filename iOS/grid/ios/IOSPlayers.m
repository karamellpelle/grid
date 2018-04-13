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
#import "IOSPlayers.h"


uint ios_players_new_local_player = 0;

char* ios_players_local_player_id = 0;

uint ios_players_local_player_id_len = 0;

char* ios_players_local_player_alias = 0;

uint ios_players_local_player_alias_len = 0;


IOSPlayersArchive* theIOSPlayersArchive = nil;



void ios_initPlayers()
{
    //theIOSPlayersArchive = [[IOSPlayersArchive alloc] init];
}


void ios_playersAuthenticateLocalPlayer()
{
/*
    GKLocalPlayer* local_player = [GKLocalPlayer localPlayer];
    
    [local_player authenticateWithCompletionHandler:^(NSError *error)
        {
            if (local_player.isAuthenticated)
            {
                ios_new_local_player = true;
 
                free(ios_players_local_player_alias);
                free(ios_players_local_player_id);
                
                // copy playerID as C-string
                const char* c_id = [[local_player playerID] cStringUsingEncoding:NSUTF8StringEncoding];
                ios_players_local_player_id_len = strlen( c_id );
                ios_players_local_player_id = malloc( ios_players_local_player_id_len * sizeof(char) );
                for (uint i = 0; i != ios_players_local_player_id_len; ++i)
                {
                    ios_players_local_player_id[i] = c_id[i];
                }
                
                // copy alias as C-string
                const char* c_alias = [[local_player alias] cStringUsingEncoding:NSUTF8StringEncoding];
                ios_players_local_player_alias_len = strlen( c_alias );
                ios_players_local_player_alias = malloc( ios_players_local_player_alias_len * sizeof(char) );
                for (uint i = 0; i != ios_players_local_player_alias_len; ++i)
                {
                    ios_players_local_player_alias[i] = c_alias[i];
                }
                
                // since we now have a connection, resend archived data
                [theIOSPlayersArchive sendArchive];
                
            }
            else
            {
                free(ios_players_local_player_alias);
                free(ios_players_local_player_id);
                ios_players_local_player_alias = 0;
                ios_players_local_player_id = 0;
                ios_players_local_player_alias_len = 0;
                ios_players_local_player_id_len = 0;
            }
        
        }
    ];
*/
}
/*
uint ios_playersHandleLocalPlayer(char* ptr_id, uint ptr_id_len, char* ptr_alias, uint ptr_alias_len)
{

    if ( ios_new_local_player )
    {
        GKPlayer* local_player = [GKLocalPlayer localPlayer];
        const char* cstr_id = [local_player.playerID UTF8String];
        const char* cstr_alias = [local_player.alias UTF8String];
        
        // fixme!!
        // copy id
        size_t i = 0;
        while ( i != ptr_id_len )
        {
            ptr_id[i] = cstr_id[i];
            if ( cstr_id[i] == '\0' )
            {
                break;
            }
            
            ++i;
        }
        
        // copy alias
        i = 0;
        while ( i != ptr_alias_len )
        {
            ptr_alias[i] = cstr_alias[i];
            if ( cstr_alias[i] == '\0' )
            {
                break;
            }
            
            ++i;
        }
        
        ios_new_local_player = false;
        return true;
    }
    else
    {
        return false;
    }
return false;
}
*/


void ios_playersSendAchievement(const char* ach, float alpha)
{
/*
    GKLocalPlayer* local_player = [GKLocalPlayer localPlayer];
    if ( local_player.isAuthenticated )
    {
        NSString *ns_ach = [NSString stringWithUTF8String:ach]; // memory??
        GKAchievement* achievement = [[[GKAchievement alloc] initWithIdentifier:ns_ach] autorelease];
        achievement.showsCompletionBanner = YES;
        
        if (achievement)
        {
            achievement.percentComplete = 100.0f * alpha;
            
            [achievement reportAchievementWithCompletionHandler:^(NSError *error)
                {
                    if (error != nil)
                    {
                        // no success, save into archive
                        [theIOSPlayersArchive pushGKAchievement: achievement];
                    }
                }
            ];
        }
    }
*/
}

void ios_playersSendScore(const char* cat, long long score)
{
/*
    GKLocalPlayer* local_player = [GKLocalPlayer localPlayer];
    if ( local_player.isAuthenticated )
    {
        GKScore* gk_score = [[[GKScore alloc] initWithCategory:[NSString stringWithUTF8String:cat]] autorelease];
        gk_score.value = score;
        
        [gk_score reportScoreWithCompletionHandler:^(NSError *error)
            {
                if (error != nil)
                {
                     // no success, save into archive
                     [theIOSPlayersArchive pushGKScore: gk_score];
                     
                }
                 
            }
        ];        
    }
*/
}












@implementation IOSPlayersArchive


-(id) init
{
    // set path for archive
    NSFileManager* file_mgr = [NSFileManager defaultManager];
    NSArray* urls = [file_mgr URLsForDirectory:NSApplicationSupportDirectory inDomains:NSUserDomainMask];
    if ( [urls count] == 0 )
    {
        return nil;
    }
    NSURL* app_support_dir = [urls objectAtIndex:0];
    path_ = [[app_support_dir URLByAppendingPathComponent:@"IOSPlayersArchive.data"] path];
    
    // create empty archive, if not present
    if ( ![file_mgr fileExistsAtPath:path_] )
    {
        [self clear];
    }
    return self;

}

-(void) pushGKScore:(GKScore *)score
{
    // http://stackoverflow.com/questions/1650110/use-of-retain-in-initwithcoder
    
    // read both arrays
    NSData* ns_data = [NSData dataWithContentsOfFile:path_]; // release?
    NSKeyedUnarchiver* ns_keyed_unarchiver = [[NSKeyedUnarchiver alloc] initForReadingWithData:ns_data];
    NSMutableArray* array_score = [[ns_keyed_unarchiver decodeObjectForKey:@"array_score"] retain];
    NSMutableArray* array_achievement = [[ns_keyed_unarchiver decodeObjectForKey:@"array_achievement"] retain];
    [ns_keyed_unarchiver finishDecoding];
    [ns_keyed_unarchiver release];

    // modify score array
    size_t i = 0;
    size_t size = [array_score count];
    while (i != size)
    {
        GKScore* score_i = [array_score objectAtIndex:i];
        NSString* cat_i = [score_i category];
        NSString* cat = [score category];
        if ( [cat isEqualToString:cat_i] && score_i.value <= score.value )
        {
            break;
        }
        ++i;
    }
    if (i == size)
    {
        // push new
        [array_score addObject:score];
    }
    else
    {
        // replace
        [array_score replaceObjectAtIndex:i withObject:score];
    }

    
    // write both arrays
    NSMutableData* ns_mutable_data = [[NSMutableData alloc] init];
    NSKeyedArchiver* ns_keyed_archiver = [[NSKeyedArchiver alloc] initForWritingWithMutableData:ns_mutable_data];
    [ns_keyed_archiver encodeObject:array_score forKey:@"array_score"];
    [ns_keyed_archiver encodeObject:array_achievement forKey:@"array_achievement"];
    [ns_keyed_archiver finishEncoding];
    [ns_mutable_data writeToFile:path_ atomically:YES];
    [ns_keyed_archiver release];
    [ns_mutable_data release];
    [array_achievement release];
    [array_score release];
    
}

-(void) pushGKAchievement:(GKAchievement *)achievement
{
    // http://stackoverflow.com/questions/1650110/use-of-retain-in-initwithcoder
    
    // read both arrays
    NSData* ns_data = [NSData dataWithContentsOfFile:path_]; // release?
    NSKeyedUnarchiver* ns_keyed_unarchiver = [[NSKeyedUnarchiver alloc] initForReadingWithData:ns_data];
    NSMutableArray* array_score = [[ns_keyed_unarchiver decodeObjectForKey:@"array_score"] retain];
    NSMutableArray* array_achievement = [[ns_keyed_unarchiver decodeObjectForKey:@"array_achievement"] retain];
    [ns_keyed_unarchiver finishDecoding];
    [ns_keyed_unarchiver release];
    
    // modify score array
    size_t i = 0;
    size_t size = [array_achievement count];
    while (i != size)
    {
        GKAchievement* achievement_i = [array_achievement objectAtIndex:i];
        NSString* cat_i = [achievement_i identifier];
        NSString* cat = [achievement identifier];
        if ( [cat isEqualToString:cat_i] && achievement_i.percentComplete <= achievement.percentComplete )
        {
            break;
        }
        ++i;
    }
    if (i == size)
    {
        // push new
        [array_achievement addObject:achievement];
    }
    else
    {
        // replace
        [array_achievement replaceObjectAtIndex:i withObject:achievement];
    }
    
    
    // write both arrays
    NSMutableData* ns_mutable_data = [[NSMutableData alloc] init];
    NSKeyedArchiver* ns_keyed_archiver = [[NSKeyedArchiver alloc] initForWritingWithMutableData:ns_mutable_data];
    [ns_keyed_archiver encodeObject:array_score forKey:@"array_score"];
    [ns_keyed_archiver encodeObject:array_achievement forKey:@"array_achievement"];
    [ns_keyed_archiver finishEncoding];
    [ns_mutable_data writeToFile:path_ atomically:YES];
    [ns_keyed_archiver release];
    [ns_mutable_data release];
    [array_achievement release];
    [array_score release];

}

// resend scores and achievement in archive
-(void) sendArchive
{
    // read arrays
    NSData* ns_data = [NSData dataWithContentsOfFile:path_]; // fixme: release?
    NSKeyedUnarchiver* ns_keyed_unarchiver = [[NSKeyedUnarchiver alloc] initForReadingWithData:ns_data];
    NSMutableArray* array_score = [[ns_keyed_unarchiver decodeObjectForKey:@"array_score"] retain];
    NSMutableArray* array_achievement = [[ns_keyed_unarchiver decodeObjectForKey:@"array_achievement"] retain];
    [ns_keyed_unarchiver finishDecoding];
    [ns_keyed_unarchiver release];
    
    // fixme: push values to file + thread safety
    // resend scores
    for (GKScore* gk_score in array_score)
    {
         [gk_score reportScoreWithCompletionHandler:^(NSError *error)
         {
             if (error != nil)
             {
                 // no success, save into archive
                 [self pushGKScore: gk_score];
                 
             }
             
         }
         ];
    }
    // resend achievements 
    for (GKAchievement* gk_achievement in array_achievement)
    {
         [gk_achievement reportAchievementWithCompletionHandler:^(NSError *error)
         {
             if (error != nil)
             {
                 // no success, save into archive
                 [self pushGKAchievement: gk_achievement];
             }
         }
         ];        
    }

    
    // clear
    [self clear];
}


// clear scores and achievements
-(void) clear
{
    NSMutableArray* array_score = [NSMutableArray arrayWithCapacity:0];
    NSMutableArray* array_achievement = [NSMutableArray arrayWithCapacity:0];
    
    NSMutableData* ns_mutable_data = [[NSMutableData alloc] init];
    NSKeyedArchiver* ns_keyed_archiver = [[NSKeyedArchiver alloc] initForWritingWithMutableData:ns_mutable_data];
    [ns_keyed_archiver encodeObject:array_score forKey:@"array_score"];
    [ns_keyed_archiver encodeObject:array_achievement forKey:@"array_achievement"];
    [ns_keyed_archiver finishEncoding];
    [ns_mutable_data writeToFile:path_ atomically:YES];
    [ns_keyed_archiver release];
    [ns_mutable_data release];
    [array_achievement release];
    [array_score release];
}

@end
