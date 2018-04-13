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
#include <stdint.h>
#import "IOSSystem.h"

// globals
IOSSystemStateT ios_system_state;

IOSSystemStateT ios_system_state_next;



uint ios_systemHandleBackBegin()
{
    return  ios_system_state != SystemStateBack      &&
            ios_system_state_next == SystemStateBack;
}

uint ios_systemHandleBackEnd()
{
    return  ios_system_state == SystemStateBack  &&
            ios_system_state_next != SystemStateBack;
}

uint ios_systemHandleFrontBegin()
{
    return  ios_system_state != SystemStateFront     &&
            ios_system_state_next == SystemStateFront;
}

uint ios_systemHandleFrontEnd()
{
    return  ios_system_state == SystemStateFront    &&
            ios_system_state_next != SystemStateFront;
}


/*
exit programmatically from iOS/ObjectiveC:
    ios_system_state_next = Empty;
    haskell_iterate();
    ios_system_state = ios_system_state_next;
    exit_UIApplicationMain();

exit programmatically from Haskell:
    exit_UIApplicationMain();
*/
