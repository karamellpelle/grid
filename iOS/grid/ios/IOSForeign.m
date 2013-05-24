/*
 *   grid is a game written in Haskell
 *   Copyright (C) 2013 Carl Joachim Svenn
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
#import "IOSForeign.h"
#import "IOS.h"
#import "IOSAppDelegate.h"
#import "IOSViewController.h"
#import <UIKit/UIKit.h>




// locals
static IOSForeignStateT ios_foreign_state;

static IOSForeignStateT ios_foreign_state_next;




void ios_foreignBeginForeign()
{
    ios_foreign_state_next = ForeignStateForeign;

    [theIOSAppDelegate setHaskellIterate: (NO)];
    [theIOSAppDelegate setViewController: theForeignViewController];
    ios_foreign_state = ios_foreign_state_next;

}


// do not call this function from Haskell!
void ios_foreignEndForeign()
{
    // prevent call from foreignBeginForeign
    if ( ios_foreign_state == ForeignStateForeign )
    {
        ios_foreign_state_next = ForeignStateHaskell;
        [theIOSAppDelegate setViewController: theIOSViewController];
        haskell_iterate();
        ios_foreign_state = ios_foreign_state_next;
        [theIOSAppDelegate setHaskellIterate: (YES)];
    }
}


uint ios_foreignHandleForeignEnd()
{
    return  ios_foreign_state == ForeignStateForeign &&
            ios_foreign_state_next == ForeignStateHaskell;
        
}
