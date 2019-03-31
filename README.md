I had an idea of making a computer game in Haskell on one of these fancy new touch platforms. And there was a GHC compiler for iOS, so I started writing this game. It is actually working pretty well, but it needs more work, especially if it should be released as a game, and I don't have enough time to use on this project anymore. Unfornately, the Apple world is not an open world. However, it should be easy to port this to other platforms too, I actually started writing this code on Linux :) It is licensed under GPLv3.

Update 2019
----------------
In 2018 I was able to build this game on Linux (at least Arch Linux) and started porting the rest. Much has been done, but more work needs to be done. To build and run:

    $ cd GLFW
    $ stack build
    $ stack exec grid


Folders
----------------

    GLFW/               : GLFW build
    data_fancy/         : data files for Fancy style
    data_plain/         : data files for Plain style
    designer/           : level designer (very old and is probably not working at all)
    dist/               : (empty)
    iOS/                : iOS build
    readme/             : files to read
    source/             : the game source code
    test/               : test build 


Gameplay
----------------

The game is controlled by a box which can be thought of as the main menu. This is used to jump in and out of the different modes. The box also controls game settings and game information. There are 3 game modes: LevelMode, PuzzleMode, MemoryMode. There is also possible to jump into foreign Objective-C code, the intention is to show score and achievements for players in GameCenter on iOS.

### LevelMode/PuzzleMode ###
The idea is to control a line through different levels. This line moves inside a grid. The line has a limited number of moves to finish each level. Levels are finished by eating FinishDot's, and there are other objects the line can use in order to get to a FinishDot. Currently, there are DotPlain, DotBonus, DotTele. And the line movement can be constrained by Wall's. DotPlain lets the line move into Room's, so a DotPlain is a door into a (new) Room. DotBonus adds more moves to the line. DotTele are teleports (inside current Room). Unfornately, there is currently a bug with the coding, se below. A defined sequence of levels makes up a world, and this world is complete when the line has gone through all levels. Some levels should be easy and some difficult. The easy levels should be made for LevelMode, so the gameplay for the player is to control the line. The difficult levels should be made for PuzzleMode, so the gameplay for the player is to find its way out of the level. Hence the gameplay for the player changes. 

### MemoryMode ###
The idea is to get high score by completing levels. At each level, the player is presented by a line movement, and should then do the same line movement in order to complete the level. The lines typically gets longer when more levels are completed, but also continues with shorter lines, so the gameplay for a player is not totally stalled. The score should be posted to GameCenter.

### Controls ###
One finger sequences controls the box and the line. Drag and drop to turn the box, make a short touch to jump into modes. Drag and drop to move the line up/down/left/right. Touch and release in the middle of the screen to move the line forward, in PuzzleMode. Touch and release in the left/right sides of the screen to roll the line. Two finger sequences controls camera view.


Coding
----------------

The game should hopefully build on several platforms by using macros `GRID_PLATFORM_*`, but only the platform iOS is implemented. I think it should be easy to build it on desktop computers by using GLFW and OpenGL 4.0+, since OpenGL 4.0 is similar to OpenGL ES 2.0.
The game code runs inside a monad MEnv, which is actually just IO + GameData, where GameData consists of the resources to be used (which are currently read-only). We represent the game by a world RunWorld, and use a IterationStack to iterate the game. RunWorld has subworlds for each game mode (LevelPuzzleWorld/MemoryWorld).

### Output ###
RunWorld has a Scene object describing how the game should be presented to the player. By being creative, a game can be presented in several ways, so there are macros `GRID_STYLE_*` to build the game with different output styles. Currently there are two styles Plain and Fancy. They are not very different, but the intention of the style Plain was to start with simple output. Not sure if the style Plain is still working. Fancy was intended to have nice output, but I am not satisfied with its current status. All the graphical output is rendered to an offscreen buffer, inside the Scene object. The idea was here to use the Scene object to twist and tweak graphics and sound into colorful output.

### GameCenter ###
On iOS, there is a GameCenter API that can post high scores and achievements. This has been implemented on the Haskell side, but the Objective-C side has not been tested and is commented out. The game creates and use dynamic data for each player on a iOS device. 

### Easter Eggs ###
Easter eggs are fun! The Konami sequence left -> left -> right -> right -> up -> down -> up -> down -> B -> A should do some fun. 


Building on iOS
----------------

In the folder `iOS/grid`, build the Haskell code into an archive `iOS/grid/Main.a`:  

    $ cd iOS/grid
    $ ./ghc_build_debug

Open the Xcode project in the folder `iOS`, build and run on a connected iOS device.

There are two different build scripts: `ghc_build_debug` and `ghc_build_release`. They call the script `ghc_build_` with different parameters. They also accept command line parameters, for GHC. The script `ghc_build_` calls GHC. It relies on ghc-ios (<https://github.com/ghc-ios/ghc/wiki>), and needs to be edited for installation specific paths. The code depend on the following packages:

* parsec        (I have 3.1.3)
* bytestring    (I have 0.10.0.2)
* mtl           (I have 2.1.2)
* random        (I have 1.0.1.1)

Hopefully, the game will work on your computer too.


Bugs
----------------

* The game suffer from sudden termination, when eating dots in LevelMode/PuzzleMode (?). Unfornately, the provisional profile expired before I had a chance to fix this. 
* Not sure if the level designer is working properly.
* There is a (potential) bug in Game.LevelPuzzle.File.Read, see the source.

Contact
----------------

<karamellpelle@hotmail.com>
