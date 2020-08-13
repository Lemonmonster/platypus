# platypus
This was my attempt to create a fully functional reactive game engine in Haskell.  The idea behind this engine was that it would consist of functional units
that would automatically be stitched together according to their dependencies by some template haskell metacode.  
These dependcies would be encoded in the type of the units.  The functional units I used were netwire wires.  These wires can hold state and apply monadic
transformations to said state.  So for example, I might have a player wire that takes as input events from the keyboard and outputs a sprite and a physics object.
The metacode would compose all the wires visible to it to create a single wire reprenting the entire game as a reactive object.  The player wires position in that 
composition would be determined by having to be downstream of the keyboard and upstream of the physics and graphics.  This has the advantage of having an 
unambiguous update order.  Additionally, since reactive programming can make for far more elegent ui code than conventional methods I thought the same might be true
for game code.  Sadly this project did not work out.  I abandoned it due to the difficulties of debugging memory leaks in haskell that were caused by thunk chains
building up over repeated iterations.  Additionally the structure of the types involved became increasing complex over time and reactive programming did not seem
to be suitable for dealing with the subtleties of a physics simulation.

If you want to get it running just do "stack build" in the project directory.  In order for stack to properly install 
the dependencies you must have sdl2 and sdl2_ttf installed on the system.


