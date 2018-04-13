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

#import "IOSViewController.h"
#import "IOSView.h"
#import "IOS.h"



// globals
IOSViewController* theIOSViewController = nil;



@implementation IOSViewController


-(id) init
{
    self = [super initWithNibName:nil bundle:nil];
    if (self)
    {
        theIOSViewController = self;
    }
    return self;
}


-(id) initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    NSAssert( NO, @"IOSViewController: use 'init' instead" );
    return [self init];
}


-(BOOL) shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation
{    
    return ios_shouldAutorotateToInterfaceOrientation( toInterfaceOrientation );
}


-(void) loadView
{

    [self setView: theIOSView];

}




@end
