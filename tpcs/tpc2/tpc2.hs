module TPC2 where

---------------------------------------------------------------------
---------------------------- Declarations ---------------------------
---------------------------------------------------------------------

-- Declared Variables
type Variables = [(String, Int)]

-- Type Declarations of a Linear Term.
data LTerm a = Const Int    -- number
             | Var String    -- variable
             | Mult Int (LTerm a)    -- multiplication of Int number with a LTerm
             | Sum (LTerm a) (LTerm a)    -- sum of two LTerms
             deriving Show

-- Type Declarations of BoolExpressions for conditions
data BoolExpr a = BoolConst Bool  -- bool const (true or false)
              | Equals (LTerm a) (LTerm a) -- equality between two linear terms
              | LessThan (LTerm a) (LTerm a)   -- one linear term is less than other
              | GreatThan (LTerm a) (LTerm a)  -- one linear term is greater than other
              | Not (BoolExpr a)    -- negation
              | And (BoolExpr a) (BoolExpr a)   -- conjunction
              deriving Show

-- Type Declarations of Programs
data Program a = Assignment String (LTerm a)   -- (asg)
             | Wait Int (Program a)  -- (wait)
             | Sequence (Program a) (Program a)    -- (seq)
             | If (BoolExpr a) (Program a) (Program a)   -- (if)
             | While (BoolExpr a) (Program a)    -- (wh)
             deriving Show


---------------------------------------------------------------------
-------------------- Calculations and Executions --------------------
---------------------------------------------------------------------


-- Calculate value of a LTerm
valueLTerm :: LTerm a    -- linear term that must be calculated
            -> Variables    -- variables currently stored and their values
            -> Int    -- the value
valueLTerm (Const n) _ = n
valueLTerm (Mult num lt) vars = num * (valueLTerm lt vars)
valueLTerm (Sum lt1 lt2) vars = (valueLTerm lt1 vars) + (valueLTerm lt2 vars)
valueLTerm (Var x) vars = case lookup x vars of
                          Just val -> val
                          Nothing  -> error "Variável não definida"



-- Calculate value of a Boolean Expression
valueBoolExpr :: BoolExpr a    -- boolean expression that must be calculated
                -> Variables    -- variables currently stored and their values
                -> Bool    -- the value
valueBoolExpr (BoolConst b) _ = b
valueBoolExpr (Not be) vars = not (valueBoolExpr be vars)
valueBoolExpr (Equals lt1 lt2) vars = valueLTerm lt1 vars == valueLTerm lt2 vars
valueBoolExpr (And be1 be2) vars = valueBoolExpr be1 vars && valueBoolExpr be2 vars
valueBoolExpr (LessThan lt1 lt2) vars = valueLTerm lt1 vars < valueLTerm lt2 vars
valueBoolExpr (GreatThan lt1 lt2) vars = valueLTerm lt1 vars > valueLTerm lt2 vars



-- Execute program to produce new state
execProgram :: Program a  -- program to be executed
                -> Variables    -- variables currently stored and their values
                -> (Int, Variables)    -- new state produced after execution
execProgram (Assignment x lt) vars = 
    case lookup x vars of
        Just _ -> let val = valueLTerm lt vars in (0, map (\(y, v) -> if y == x then (y, val) else (y, v)) vars)
        Nothing -> let val = valueLTerm lt vars in (0, (x, val) : vars)


execProgram (Wait wt p) vars = let (t, vars') = execProgram p vars in (wt + t, vars')

execProgram (Sequence p1 p2) vars = let (t1, vars') = execProgram p1 vars
                                        (t2, vars'') = execProgram p2 vars'
                                    in (t1 + t2, vars'')

execProgram (If cond p1 p2) vars = if valueBoolExpr cond vars
                                   then execProgram p1 vars
                                   else execProgram p2 vars

execProgram loop@(While cond p) vars = if valueBoolExpr cond vars
                                          then let (n, vars') = execProgram p vars
                                                   (n', vars'') = execProgram loop vars'
                                               in (n + n', vars'')

                                          else (0, vars)



---------------------------------------------------------------------
------------------------------- Tests -------------------------------
---------------------------------------------------------------------


-- Variables
vars :: Variables 
vars = [("x", 0)]

-- Programs
progAsg1 :: Program a
progAsg1 = (Assignment "y" (Const 5))

progAsg2 :: Program a
progAsg2 = (Assignment "z" (Const 2))

progWait :: Program a
progWait = (Wait 3 progAsg1)

progSeq :: Program a 
progSeq = (Sequence (progAsg2) (progWait))

progIfTrue :: Program a
progIfTrue = (If (LessThan (Var "x") (Const 20)) (Assignment "y" (Const 1)) (Assignment "y" (Const 2)))

progIfFalse :: Program a
progIfFalse = (If (GreatThan (Var "x") (Const 20)) (Assignment "x" (Const 5)) (Assignment "x" (Const 1)))

progWhileTrue :: Program a
progWhileTrue = (While (LessThan (Var "x") (Const 5)) (Assignment "x" (Sum (Var "x") (Const 1))))

progWhileFalse :: Program a
progWhileFalse = (While (LessThan (Var "x") (Const 5)) (Assignment "x" (Sum (Var "x") (Const 1)))) 


-- Tests --
test1 = execProgram progAsg1 vars
-- output esperado: test1 = (0, [("y", 5), ("x", 0)]) --

test2 = execProgram progAsg2 vars
-- output esperado: test2 = (0, [("z", 2), ("x", 0)]) --

test3 = execProgram progWait vars
-- output esperado: test3 = (3, [("y", 5), ("x", 0)]) -- 

test4 = execProgram progSeq vars
-- output esperado: test4 = (3, [("y", 5), ("z", 2), ("x", 0)]) -- 

test5 = execProgram progIfTrue vars
-- output esperado: test5 = (0, [("y", 1), ("x", 0)]) -- 

test6 = execProgram progIfFalse vars
-- output esperado: test6 = (0, [("x", 1)]) -- 

test7 = execProgram progWhileTrue vars
-- output esperado: test7 = (0, [("x", 5)]) -- 

test8 = execProgram progWhileFalse vars
-- output esperado: test8 = (0, [("x", 5)]) -- 

