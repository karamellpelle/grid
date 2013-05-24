COMPILE AND RUN
run ./compile_it inside source, run ./design inside dist

CONTROLS
General:
  F1                          DotPlain
  F2                          DotBonus
  F3                          DotTele
  F4                          DotFinish
  F10                         Wall
  A,D,S,W,E,C                 move Camera
  Up,Dn,Lt,Rt,PgUp,PgDn       move Node
  0..9                        rooms 0..9
  R,T                         -/+ Room
  Q                           toggle IsPuzzle
  Y,U                         -/+ Segments
  M                           change object at Node
  Tab                         toggle play
  Ctrl-Tab                    restart play
  ESC                         exit and save
  
DotPlain:
  Enter                       push Node
  L,O                         -/+ Size
  K,I                         -/+ Room

DotBonus:
  Enter                       push Node
  L,O                         -/+ Size
  K,I                         -/+ Add

DotTele:
  Enter                       push Node
  L,O                         -/+ Size

DotFinish:
  Enter                       push Node

Wall:
  Enter                       push Node
  P                           toggle IsDouble


Play:
  Tab                         toggle play
  Space                       toggle IsPuzzle
  X,C                         control path roll
  up,down,left,right,lshift   control path direction     
