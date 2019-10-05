# LudumDare45

This is a fork of PlayingWithProlog/cookingwithsnap


## Install (assumes you have a recent SWI-Prolog)

1. Download the latest Snap.zip from 
https://github.com/jmoenig/Snap/releases/tag/v5.1.0

2. unzip in the cookingwithsnap directory, you'll get a directory Snap-5.1.0

## run

1. cd cookingwithsnap/prolog

2. swipl cooking.pl

3. ?- go.

4. load Snap-5.1.0/index.html into browser to get a default snap project

5. Load the file cookingwithsnap/cookingwithsnap.xml via the document icon in UI,
or by drag and drop onto Snap UI.

6. click the green flag

7. Drag  ingredients (for now just the egg) into the pan

## Architecture

Snap! running in the browser sends HTTP requests to a SWI-Prolog server. The server formulates a pair of goals. The action goal to initiate change, and a return goal
whose last argument is bound with the information to be returned to Snap!

These are passed via message queues to a dedicated thread that handles the CHR productions.

The result is passed back via another message queue and bound to the last argument.

## Derived from

CHR server lifted from

https://github.com/fnogatz/CHR-Constraint-Server


