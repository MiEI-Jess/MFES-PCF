{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

allAdvs :: [Objects]
allAdvs = [(Left P1), (Left P2), (Left P5), (Left P10)]

getAdv :: Objects -> Adventurer
getAdv (Left a) = a

toObjects :: Adventurer -> Objects
toObjects a = (Left a)

onLeft :: State -> [Objects] -> [Objects]
onLeft s (o:os) | o == Right () = onLeft s os
                | s o = onLeft s os
                | otherwise = o : onLeft s os   

onRight :: State -> [Objects] -> [Objects]
onRight s (o:os) | o == Right () = onRight s os
                 | s o = o : onRight s os
                 | otherwise = onRight s os 

-- The time that each adventurer needs to cross the bridge
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state of the
game, with all adventurers and the lantern on the left side of the bridge.
Similarly, the function (const True) represents the end state of the game, with
all adventurers and the lantern on the right side of the bridge.  
--}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]


-- The initial state of the game
gInit :: State
gInit = const False

-- Final state of the game
gFinal :: State
gFinal = const True

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game for a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
                               
{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
allValidPlays :: State -> ListDur State
allValidPlays s | s == gFinal = LD [] -- if all have crossed
                | s (Right ()) = LD $ toRightStates 
                | otherwise =  LD $ toLeftStates
  where 
    toRightPlays = (toLists $ onLeft s allAdvs) ++ (rmPairs $ makePairs $ onLeft s allAdvs) -- plays on left to right
    toLeftPlays = (toLists $ onRight s allAdvs) ++ (rmPairs $ makePairs $ onRight s allAdvs) -- plays on right to left
    toRightStates = allValidPlaysAux s toRightPlays
    toLeftStates = allValidPlaysAux s toLeftPlays

allValidPlaysAux :: State -> [[Objects]] -> [Duration State]
allValidPlaysAux s (o:os) = Duration(maxDuration o, mChangeState o s) : allValidPlaysAux s os     
  
nextState :: State -> (Objects, Objects) -> State
nextState s (a1,a2) = mChangeState [a1,a2] s 

maxDuration :: [Objects] -> Int
maxDuration [] = 0
maxDuration [a] = getTimeAdv $ getAdv a 
maxDuration (a1:a2:as) = max (getTimeAdv $ getAdv a1) (maxDuration (a2:as))

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}

-- First calculate all next valid plays for the current state
-- Then repeat it n times to reach all possible states after n sequences
exec :: Int -> State -> ListDur State
exec 0 s = LD [Duration (0,s)]
exec n s = concatMap (\next -> exec (n-1) next) validNextStates 
  where 
    validNextStates = map (\state -> getValue state) validNextPlays -- [State]
    validNextPlays = remLD $ allValidPlays s -- [Duration a]

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
leq17 :: Bool
leq17 = any (\s -> getValue s == gFinal && getDuration s <= 17) validSequences
  where
    validSequences = remLD $ exec 5 gInit

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = undefined

--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = let f' = (fmap f) in
     LD . (map f') . remLD

instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ do f <- x; a <- y; return (f a)

instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                       g(Duration (i,x)) = let u = (remLD (k x))
                          in map (\(Duration (i',x)) -> Duration (i + i', x)) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

--------- List Utils ----------

-- Returns all elements as lists --
toLists :: [a] -> [[a]]
toLists [] = [[]]
toLists as = map (\a -> [a]) as

-- Returns list of pairs as list of lists --
rmPairs :: [(a,a)] -> [[a]]
rmPairs [] = [[]]
rmPairs ((a1,a2) : ps) = [a1,a2] : rmPairs ps

-- Returns all possible combinations of pairs in a list of elements, except pairs where the elements are equal --
makePairs :: (Eq a) => [a] -> [(a,a)]
makePairs as = normalize $ do a1 <- as; a2 <- as; [(a1,a2)]

-- Keeps only the pairs where elements are different --
normalize :: (Eq a) => [(a,a)] -> [(a,a)]
normalize l = removeSw $ filter p1 l where
  p1 (x,y) = if x /= y then True else False

-- Removes duplicate pairs with switched order of appearance --
removeSw :: (Eq a) => [(a,a)] -> [(a,a)]
removeSw [] = []
removeSw ((a,b):xs) = if elem (b,a) xs then removeSw xs else (a,b):(removeSw xs)
