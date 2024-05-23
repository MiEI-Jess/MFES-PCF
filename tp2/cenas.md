# Context
4 adventurers. They take: 1 min, 2 min, 5 and 10 min to cross.

Max. 2 crossing at the same time

1 flashlight that has to be carried while crossing

- one says they cant all cross in less than 19 min
- other says that it can be done in 17 min

## Task 1
Verify these claims using Haskell.
- model the system above using what we learned about monads, in particular the duration
and non-deterministic ones;
- show that it is possible to do it in 17 minutes;
- show that it is impossible to do it in less than 17 minutes.
Do this by completing Adventurers.hs, by adding a definition to the functions that lack a definition, following the comments present in
the code. 

Write a report that explains your code and the conclusions obtained.
Recall the duration monad from the slides and the Haskell
code that was previously provided. Analyse in detail the code concerning the Knight’s quest and in particular the monad LogList.

## Task 2
Compare UPPAAL and Haskell in the adventurer’s problem. 
Provide strong and weak points of the two approaches
- (dis)advantages of UPPAAL and Haskell for this problem
- draw connections to other modules we learned during the course.

### Valorisation (*)
Add new functionalities to the code: 
- for example present the sequence of states respective to the
movements of the adventurers from the initial state to the final goal.