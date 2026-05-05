{-
    Program Name: EECS 662 Final Project
    Author: Sophia Jacob, Anna Lin, Kusuma Murthy
    Creation Date: 4/30/2026
    Last modified: 5/4/2026
    Test Cases: Test Cases are at the bottom with a list of them called: allTests and the ones provided in the Project 5 template file.
                These are different test cases for the different new functionalities.
                When you call the allTests variable, it should return all True for the test cases for the expected and actual answers to match.
    Story Line: Use the getClue function to get clues 1-3. With these clues, figure out the right
                TripleThreatExt expression and pass it in to the reveal function.
                The reveal function will either give you the answer back or if you type in the right expression,
                it will give you the corresponding key to the clue.
                Solve all the clues to escape.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PostfixOperators #-}

import Control.Monad

-- Feature 3: Pattern Matching
data PatternMatching where
  PNum :: Int -> PatternMatching
  PBool :: Bool -> PatternMatching
  PString :: String -> PatternMatching
  PCatcher :: PatternMatching -- This is to catch patterns and match to anything.
  deriving (Show, Eq)

data TripleThreatType where
  TNum :: TripleThreatType
  TBool :: TripleThreatType
  (:->:) :: TripleThreatType -> TripleThreatType -> TripleThreatType
  TLoc :: TripleThreatType -> TripleThreatType
  TString :: TripleThreatType
  deriving (Show, Eq)

data TripleThreat where
  Num :: Int -> TripleThreat
  Boolean :: Bool -> TripleThreat
  Id :: String -> TripleThreat
  Plus :: TripleThreat -> TripleThreat -> TripleThreat
  Minus :: TripleThreat -> TripleThreat -> TripleThreat
  Mult :: TripleThreat -> TripleThreat -> TripleThreat
  Div :: TripleThreat -> TripleThreat -> TripleThreat
  Exp :: TripleThreat -> TripleThreat -> TripleThreat
  Between :: TripleThreat -> TripleThreat -> TripleThreat -> TripleThreat
  Lambda :: String -> TripleThreatType -> TripleThreat -> TripleThreat
  App :: TripleThreat -> TripleThreat -> TripleThreat
  If :: TripleThreat -> TripleThreat -> TripleThreat -> TripleThreat
  And :: TripleThreat -> TripleThreat -> TripleThreat
  Or :: TripleThreat -> TripleThreat -> TripleThreat
  Leq :: TripleThreat -> TripleThreat -> TripleThreat
  IsZero :: TripleThreat -> TripleThreat
  Fix :: TripleThreat -> TripleThreat
  -- Feature 1: Storage.
  New :: TripleThreat -> TripleThreat
  Deref :: TripleThreat -> TripleThreat
  Set :: TripleThreat -> TripleThreat -> TripleThreat
  -- Feature  2: Concatenation.
  Concat :: TripleThreat -> TripleThreat -> TripleThreat
  StringLang :: String -> TripleThreat
  -- Feature 3: Pattern Matching.
  MatchPattern :: TripleThreat -> [(PatternMatching, TripleThreat)] -> TripleThreat
  deriving (Show, Eq)

data TripleThreatExt where
  NumX :: Int -> TripleThreatExt
  BooleanX :: Bool -> TripleThreatExt
  IdX :: String -> TripleThreatExt
  PlusX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  MinusX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  MultX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  DivX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  ExpX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  BetweenX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  LambdaX :: String -> TripleThreatType -> TripleThreatExt -> TripleThreatExt
  AppX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  BindX :: String -> TripleThreatType -> TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  IfX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  AndX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  OrX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  LeqX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  IsZeroX :: TripleThreatExt -> TripleThreatExt
  FixX :: TripleThreatExt -> TripleThreatExt
  -- Feature 1: Storage.
  NewX :: TripleThreatExt -> TripleThreatExt
  DerefX :: TripleThreatExt -> TripleThreatExt
  SetX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  -- Feature 2: Concatenation.
  ConcatX :: TripleThreatExt -> TripleThreatExt -> TripleThreatExt
  StringLangX :: String -> TripleThreatExt
  -- Feature 3: Pattern Matching.
  MatchPatternX :: TripleThreatExt -> [(PatternMatching, TripleThreatExt)] -> TripleThreatExt
  deriving (Show, Eq)

data TripleThreatVal where
  NumV :: Int -> TripleThreatVal
  BooleanV :: Bool -> TripleThreatVal
  ClosureV :: String -> TripleThreat -> EnvVal -> TripleThreatVal
  LocV :: Int -> TripleThreatVal -- New feature with location/storage.
  StringV :: String -> TripleThreatVal -- New feature for Concatenation.
  deriving (Show, Eq)

type EnvVal = [(String, TripleThreatVal)]

type Cont = [(String, TripleThreatType)]

-- Helpers for Feature 1: Storage.
type Loc = Int

type Store = (Loc, StoreFunc)

type StoreFunc = Loc -> Maybe TripleThreatVal

initStore :: Store
initStore = (0, \_ -> Nothing)

setStore :: Store -> Loc -> TripleThreatVal -> Store
setStore (i, s) l v = (i, \m -> if m == l then Just v else s m)

derefStore :: Store -> Loc -> Maybe TripleThreatVal
derefStore (_, s) l = s l

newStore :: Store -> TripleThreatVal -> (Store, Loc)
newStore (i, s) v = ((i + 1, \m -> if m == i then Just v else s m), i)

-- Helpers for Feature 3: Pattern Matching.
patternMatchType :: PatternMatching -> TripleThreatType -> Bool
patternMatchType (PNum _) TNum = True -- Given a PNum and a TNum, they are of the same type. Return True.
patternMatchType (PBool _) TBool = True -- Given a PBool and a TBool, they are of the same type. Return True.
patternMatchType (PString _) TString = True -- Given a PString and a TString, they are of the same type. Return True.
patternMatchType PCatcher _ = True -- Given a PCatcher, then return True since the type doesn't matter.
patternMatchType _ _ = False -- Otherwise, return False.

checkBranchType :: Cont -> TripleThreatType -> (PatternMatching, TripleThreat) -> Maybe TripleThreatType
checkBranchType g val_type (p, body) =
  -- Checks that the value and the pattern to match is of the same type. If not return Nothing.
  if patternMatchType p val_type then typeof g body else fail "Fail: Pattern and branch types do not match"

-- Helps determine if a pattern and a value are a match
patternMatchEval :: PatternMatching -> TripleThreatVal -> Bool
patternMatchEval (PNum l) (NumV r) = l == r -- Given a PNum and a NumV, check if the contents are the same.
patternMatchEval (PBool l) (BooleanV r) = l == r -- Given a PBool and a BooleanV, check if the contents are the same.
patternMatchEval (PString l) (StringV r) = l == r -- Given a PString and a StringV, check if the contents are the same.
patternMatchEval PCatcher _ = True -- Given a PCatcher, return True since the value doesn't matter.
patternMatchEval _ _ = False -- Otherwise, return False.

-- Helper function that iterates through all of the branches to find a match
checkBranchEval :: TripleThreatVal -> EnvVal -> Store -> [(PatternMatching, TripleThreat)] -> Maybe (Store, TripleThreatVal)
checkBranchEval _ _ _ [] = fail "Fail: No pattern match found"
checkBranchEval val env sto ((p, body) : restBranchList) =
  -- Check a branch and see if there is a match, then evaluate the body. Otherwise, check the rest of the branches.
  if patternMatchEval p val then eval env sto body else checkBranchEval val env sto restBranchList

-- ========== Project Exercises ========== --

-- Helper function for direct substitution. sub id=val in body. This function shouldn't fail.
substitution :: String -> TripleThreat -> TripleThreat -> TripleThreat -- Identifier, Value, Body.
substitution i v (Num x) = Num x -- Ex.: sub x=5 in 7. No subsititution, so just return the body.
-- x and y could be Id's or numbers or other so do substitution of both sides. Ex.: sub x=5 in 5+7 or sub x=5 in x+2.
substitution i v (Plus x y) = Plus (substitution i v x) (substitution i v y)
-- x and y could be Id's or numbers or other so do substitution of both sides. Ex.: sub x=5 in 7-5 or sub x=5 in x-2.
substitution i v (Minus x y) = Minus (substitution i v x) (substitution i v y)
-- x and y could be Id's or numbers or other so do substitution of both sides. Ex.: sub x=5 in 5*7 or sub x=5 in x*2.
substitution i v (Mult x y) = Mult (substitution i v x) (substitution i v y)
-- x and y could be Id's or numbers or other so do substitution of both sides. Ex.: sub x=5 in 4/2 or sub x=6 in x/2.
substitution i v (Div x y) = Div (substitution i v x) (substitution i v y)
-- x and y could be Id's or numbers or other so do substitution of both sides. Ex.: sub x=5 in 4**2 or sub x=6 in x**2.
substitution i v (Exp x y) = Exp (substitution i v x) (substitution i v y)
-- Ex.: sub x=True in True. No subsititution, so just return the body.
substitution i v (Boolean x) = Boolean x
-- l and r could be Id's or Booleans or other so do substitution of both sides. Ex.: sub x=True in True && False or sub x=True in x && False.
substitution i v (And l r) = And (substitution i v l) (substitution i v r)
-- l and r could be Id's or Booleans or other so do substitution of both sides. Ex.: sub x=True in True or False || sub x=True in x || False.
substitution i v (Or l r) = Or (substitution i v l) (substitution i v r)
-- l and r could be Id's or Nums or other so do substitution of both sides. Ex.: sub x=5 in 5<7 or sub x=5 in x<7.
substitution i v (Leq l r) = Leq (substitution i v l) (substitution i v r)
-- x could be a Num or Id, or other. Ex.: sub x=5 in isZero 7 or sub x=5 in isZero x.
substitution i v (IsZero x) = IsZero (substitution i v x)
-- c, t, or e could be Id's or other. Ex.: sub x=5 in (if True then 5 else 7) or sub x=5 in (if True then x else 7)
substitution i v (If c t e) = If (substitution i v c) (substitution i v t) (substitution i v e)
-- n1, n2, n3 could be Id's or Nums, or other. Ex.: sub x=5 in (5<2<3) or sub x=5 in (x<7<8).
substitution i v (Between n1 n2 n3) = Between (substitution i v n1) (substitution i v n2) (substitution i v n3)
substitution i v (Id iden) =
  if i == iden -- If the given variable is the same as the i variable, then we will substitute with the value. Sub x=5 in x changes x to be that value.
    then v
    -- Otherwise, just return the given variable. Ex.: sub x=5 in y doesn't change y.
    else Id iden
-- f and a could be Id's or numbers or other so do substitution of both sides. Ex.: sub x=5 in (inc 3) or sub x=5 in (inc x)
substitution i v (App f a) = App (substitution i v f) (substitution i v a) -- Try to substitute the function and the application.
substitution i v (Lambda x ty body) =
  if x == i
    -- Ex.: sub x=5 in (lambda x:TNum x+1). If the variable given and the i variable are the same, then the variable is shadowed and no longer needed.
    then Lambda x ty body
    -- Ex.: sub x=5 in (lambda y:TNum y+1). If the variable given and the i variable aren't the same, then the variable could be substituted in the body.
    else Lambda x ty (substitution i v body)
-- For fix, see if the variable can be substituted into the function.
substitution i v (Fix f) = Fix (substitution i v f)
-- val could be a Num or Id, or other. Ex.: sub x=5 in (New x).
substitution i v (New val) = New (substitution i v val)
-- l and val could be a Num or Id, or other. Ex.: sub x=5 in (Set l x).
substitution i v (Set l val) = Set (substitution i v l) (substitution i v val)
-- l could be a Num or Id, or other. Ex.: sub x=5 in (Deref l), where l has an expression with x in it.
substitution i v (Deref l) = Deref (substitution i v l)
-- l could be a StringLang or Id, or other. Ex.: sub x="A" in Concat (x "B")
substitution i v (Concat l r) = Concat (substitution i v l) (substitution i v r)
substitution i v (StringLang str) = StringLang str
substitution i v (MatchPattern val branches) = MatchPattern (substitution i v val) (map (\(p, b) -> (p, substitution i v b)) branches)

elabTerm :: TripleThreatExt -> TripleThreat
elabTerm (NumX n) = Num n -- Return the KULang equivalent.
elabTerm (BooleanX b) = Boolean b -- Return the KULang equivalent.
elabTerm (IdX i) = Id i -- Return the KULang equivalent.
elabTerm (PlusX l r) = Plus (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (MinusX l r) = Minus (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (MultX l r) = Mult (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (DivX l r) = Div (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (ExpX base exponent) = Exp (elabTerm base) (elabTerm exponent) -- Return the KULang equivalent.
elabTerm (BetweenX first second third) = Between (elabTerm first) (elabTerm second) (elabTerm third) -- Return the KULang equivalent.
elabTerm (LambdaX i ty b) = Lambda i ty (elabTerm b) -- Return the KULang equivalent.
elabTerm (AppX f a) = App (elabTerm f) (elabTerm a) -- Return the KULang equivalent.
elabTerm (BindX i ty v b) = App (Lambda i ty (elabTerm b)) (elabTerm v) -- Apply existing KULang functions. bind x = t1 in t2 == (lambda x in t2) t1
elabTerm (IfX c t e) = If (elabTerm c) (elabTerm t) (elabTerm e) -- Return the KULang equivalent.
elabTerm (AndX l r) = And (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (OrX l r) = Or (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (LeqX l r) = Leq (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (IsZeroX n) = IsZero (elabTerm n) -- Return the KULang equivalent.
elabTerm (FixX e) = Fix (elabTerm e) -- Return the KULang equivalent.
elabTerm (NewX v) = New (elabTerm v) -- Return the KULang equivalent.
elabTerm (SetX l v) = Set (elabTerm l) (elabTerm v) -- Return the KULang equivalent.
elabTerm (DerefX l) = Deref (elabTerm l) -- Return the KULang equivalent.
elabTerm (ConcatX l r) = Concat (elabTerm l) (elabTerm r) -- Return the KULang equivalent.
elabTerm (StringLangX str) = StringLang str -- Return the KULang equivalent.
-- We will elaborate on the value to match. We will also elaborate on the body of the branch, but keep the pattern the same, no elaboration needed.
elabTerm (MatchPatternX val branches) = MatchPattern (elabTerm val) (map (\(p, b) -> (p, elabTerm b)) branches)

-- Part 1 - Type Inference
typeof :: Cont -> TripleThreat -> (Maybe TripleThreatType)
-- Check if n is a positive number, then return Maybe TNum, else Nothing.
typeof g (Num n) = if n >= 0 then return TNum else fail "Fail: Negative Number"
-- Return a TBool.
typeof g (Boolean b) = return TBool
typeof g (Id i) = (lookup i g)
typeof g (Plus l r) = do
  TNum <- typeof g l -- Check if the left is a type of Num as the result.
  TNum <- typeof g r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof g (Minus l r) = do
  TNum <- typeof g l -- Check if the left is a type of Num as the result.
  TNum <- typeof g r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof g (Mult l r) = do
  TNum <- typeof g l -- Check if the left is a type of Num as the result.
  TNum <- typeof g r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof g (Div l r) = do
  TNum <- typeof g l -- Check if the left is a type of Num as the result.
  TNum <- typeof g r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof g (Exp l r) = do
  TNum <- typeof g l -- Check if the left is a type of Num as the result.
  TNum <- typeof g r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof g (Between first second third) = do
  TNum <- typeof g first -- Check if first is a type of Num.
  TNum <- typeof g second -- Check if second is a type of Num.
  TNum <- typeof g third -- Check if third is a type of Num.
  -- If it has evaluated here, then return TBool.
  return TBool
typeof g (Lambda i d b) = do
  r <- typeof ((i, d) : g) b
  return (d :->: r)
typeof g (App f a) = do
  apply <- typeof g a -- Check that the application to the function is a type in our language.
  (d :->: r) <- typeof g f -- The f parameter should be a function, meaning that it should have a D:->:R type.
  -- If the application type is the same as the domain, then this parameter is accepted into the function and the output type of the function is the range.
  -- Otherwise, there's a type mismatch and the function can't be evaluated with this application.
  if apply == d then return r else fail "Fail: The type of the application and the domain of the function do not match."
typeof g (If c t e) = do
  TBool <- typeof g c -- Check if c is a type of Bool.
  t' <- typeof g t -- Check if t is a type of TripleThreatType.
  e' <- typeof g e -- Check if e is a type of TripleThreatType.
  -- If it has evaluated here, then check if the branch types are the same for then and else and return that type, else Nothing.
  (if t' == e' then return t' else fail "Fail: The branches don't have the same type.")
typeof g (And l r) = do
  TBool <- typeof g l -- Check if the left is a type of Boolean.
  TBool <- typeof g r -- Check if the right is a type of Boolean.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof g (Or l r) = do
  TBool <- typeof g l -- Check if the left is a type of Boolean.
  TBool <- typeof g r -- Check if the right is a type of Boolean.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof g (Leq l r) = do
  TNum <- typeof g l -- Check if the left is a type of Num.
  TNum <- typeof g r -- Check if the right is a type of Num.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof g (IsZero n) = do
  TNum <- typeof g n -- Check if n is a type of Num.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof g (Fix f) = do
  (d :->: r) <- typeof g f -- Need to ensure that the given parameter is a function, which has a type of D:->:R.
  return r -- The type of the Fix is the range of the function.
typeof g (New v) = do
  val_type <- typeof g v -- Check the type of the value t.
  return (TLoc val_type) -- Returns TLoc and the type of t.
typeof g (Set l v) = do
  TLoc cur_val_type <- typeof g l -- First, we check that the type of the location is a TLoc.
  val_type <- typeof g v -- Ensure that v is in the type of our language.
  return val_type -- Return the type of the new value.
typeof g (Deref l) = do
  TLoc val_type <- typeof g l -- Checks if the type of t is a TLoc.
  return val_type -- Returns the type of the value in the location.
typeof g (Concat l r) = do
  TString <- typeof g l -- Checks if the left is a type of String.
  TString <- typeof g r -- Checks if the right is a type of String.
  return TString -- If it has evaluated here, then return Maybe TString as the result.
typeof g (StringLang str) = return TString -- Return a TString if given a String.
typeof g (MatchPattern val branches) = do
  val_type <- typeof g val -- Check the type of the value to be patterned matched.
  branch_types <- mapM (checkBranchType g val_type) branches -- Get the types for all the branch bodies.
  case branch_types of
    [] -> fail "Fail: No branches found in the match statement." -- If empty list is returned, then no branches are found in the match statement.
    -- Otherwise, takes the head of the list to get the first branch's type. Then, checks that it is equal to the rest of the branch types.
    (first_type : rest_type) ->
      if all (== first_type) rest_type then return first_type else fail "Fail: Types of branch bodies do not match."

-- Part 2 - Evaluation
eval :: EnvVal -> Store -> TripleThreat -> Maybe (Store, TripleThreatVal)
eval env sto (Num x) = if x < 0 then fail "Fail: Negative Number" else return (sto, (NumV x))
eval env sto (Boolean b) = return (sto, (BooleanV b)) -- Return the Boolean type.
eval env sto (Id i) = do
  val <- lookup i env -- Once we hit an identifier, then look it up from the Env list and return that value.
  return (sto, val)
eval env sto (Plus l r) = do
  -- Evaluate the left and right-hand side and try to do x+y. If error, Monad returns Nothing.
  (sto1, (NumV x)) <- eval env sto l -- Type cast to NumV to check if l is NumV.
  (sto2, (NumV y)) <- eval env sto1 r -- Type cast to NumV to check if r is NumV.
  return (sto2, (NumV (x + y))) -- Once both are checked, return x + y and type-cast as Maybe NumV.
eval env sto (Minus l r) = do
  -- Evaluate the left and right-hand side and try to do x-y. If error, Monad returns Nothing.
  (sto1, (NumV x)) <- eval env sto l -- Type cast to NumV to check if l is NumV.
  (sto2, (NumV y)) <- eval env sto1 r -- Type cast to NumV to check if r is NumV.
  let sub = x - y -- Once both are checked, return x - y and type-cast as Maybe Num if not less than 0.
   in if sub < 0
        then fail "Fail: Negative Number from Minus"
        else
          return (sto2, (NumV sub))
eval env sto (Mult l r) = do
  -- Evaluate the left and right-hand side and try to do x-y. If error, Monad returns Nothing.
  (sto1, (NumV x)) <- eval env sto l -- Type cast to NumV to check if l is NumV.
  (sto2, (NumV y)) <- eval env sto1 r -- Type cast to NumV to check if r is NumV.
  return (sto2, (NumV (x * y))) -- Once both are checked, return x - y and type-cast as Maybe NumV.
eval env sto (Div l r) = do
  -- Evaluate the left and right-hand side and try to do x/y. If error, Monad returns Nothing.
  (sto1, (NumV x)) <- eval env sto l
  (sto2, (NumV y)) <- eval env sto1 r
  if y == 0 -- Check if the denominator returned as 0, then do Nothing.
    then fail "Fail: Division by zero"
    else
      return (sto2, (NumV (x `div` y)))
eval env sto (Exp l r) = do
  -- Evaluate the left and right-hand side and try to do x-y. If error, Monad returns Nothing.
  (sto1, (NumV x)) <- eval env sto l -- Type cast to NumV to check if l is NumV.
  (sto2, (NumV y)) <- eval env sto1 r -- Type cast to NumV to check if r is NumV.
  return (sto2, (NumV (x ^ y))) -- Once both are checked, return x - y and type-cast as Maybe NumV.
eval env sto (Between first second third) = do
  -- First, we need to check that each argument returns as a Num.
  (sto1, (NumV n1)) <- eval env sto first -- Type cast to Num to check if first is Num.
  (sto2, (NumV n2)) <- eval env sto1 second -- Type cast to Num to check if second is Num.
  (sto3, (NumV n3)) <- eval env sto2 third -- Type cast to Num to check if third is Num.
  return (sto3, (BooleanV ((n2 > n1) && (n3 > n2)))) -- Return if n2 > n1 and n3 > n2.
eval env sto (Lambda i _ body) = do
  return (sto, (ClosureV i body env)) -- Then, create the Closure of this lambda function with the environment.
eval env sto (App f a) = do
  (sto1, (ClosureV i b ce)) <- eval env sto f -- Check if the function returns a Closure with its corresponding closure environment.
  (sto2, val) <- eval env sto1 a -- Check that the application returns something in the language.
  -- Use the local closure of the ClosureV by adding the new binding and then evaluate the body with that env.
  eval ((i, val) : ce) sto2 b
eval env sto (If c t e) = do
  -- First, we need to check that the condition returns as a Boolean.
  (sto1, BooleanV cond) <- eval env sto c -- Type cast to Boolean to check if c is Boolean.
  if cond then eval env sto1 t else eval env sto1 e -- If the condition is True, return t' else return e'.
eval env sto (And l r) = do
  -- First, we need to check that both the left and right side are Booleans to evaluate this expression.
  (sto1, (BooleanV x)) <- eval env sto l -- Type cast to Boolean to check if l is Boolean.
  (sto2, (BooleanV y)) <- eval env sto1 r -- Type cast to Boolean to check if r is Boolean.
  return (sto2, (BooleanV (x && y))) -- Once both are checked, return x && y and type-cast as Maybe Boolean.
eval env sto (Or l r) = do
  -- First, we need to check that both the left and right side are Booleans to evaluate this expression.
  (sto1, (BooleanV x)) <- eval env sto l -- Type cast to Boolean to check if l is Boolean.
  (sto2, (BooleanV y)) <- eval env sto1 r -- Type cast to Boolean to check if r is Boolean.
  return (sto2, (BooleanV (x || y))) -- Once both are checked, return x || y and type-cast as Maybe Boolean.
eval env sto (Leq l r) = do
  -- First, we need to check that both the left and right side are numbers to check which is less.
  (sto1, (NumV x)) <- eval env sto l -- Type cast to Num to check if l is Num.
  (sto2, (NumV y)) <- eval env sto1 r -- Type cast to Num to check if r is Num.
  return (sto2, (BooleanV (x <= y))) -- Once both are checked, return if x < y and type-cast as Maybe Boolean.
eval env sto (IsZero n) = do
  -- First, we need to check that the given input is a Num in our language.
  (sto1, (NumV x)) <- eval env sto n -- Type cast to Num to check if n is Num.
  return (sto1, (BooleanV (x == 0))) -- Once both are checked, return if x == 0 and type-cast as Maybe Boolean.
eval env sto (Fix f) = do
  (sto1, (ClosureV i b ce)) <- eval env sto f -- Make sure that the function given returns a ClosureV with the environment.
  -- Get the local environment and we need to keep the function in scope by substituting to keep the Lambda. The type of Lambda no longer matters since we've done typeof.
  -- Then, we need to do eval on this to evaluate the recursion and the body.
  (eval ce sto1 (substitution i (Fix (Lambda i TNum b)) b))
eval env sto (New v) = do
  (sto1, v') <- eval env sto v -- Evaluate the value given to store in the next fresh location.
  let (sto2, loc) = newStore sto1 v' -- Call the newStore function to set the value at the fresh location and increment it.
  return (sto2, LocV loc) -- Then, return the updated store and the location that the value was just added to.
eval env sto (Deref l) = do
  (sto1, LocV l') <- eval env sto l -- Evaluate the given location to obtain a location.
  v <- derefStore sto1 l' -- Dereference the value at that location.
  return (sto1, v) -- Then, return the value.
eval env sto (Set l v) = do
  (sto1, LocV loc) <- eval env sto l -- Evaluate the given location to obtain a location.
  (sto2, val) <- eval env sto1 v -- Evaluate the value given the updated store.
  let sto3 = setStore sto2 loc val -- Update the store to the value at the location.
  return (sto3, val) -- Update the store along with the assigned value.
eval env sto (Concat l r) = do
  (sto1, (StringV x)) <- eval env sto l -- Type cast to StringV to check if l is a string.
  (sto2, (StringV y)) <- eval env sto r -- Type cast to StringV to check if r is a string.
  return (sto2, StringV (x ++ y)) -- Return the concatenate the strings together.
eval env sto (StringLang str) = do
  return (sto, (StringV str)) -- Return the StringV type.
eval env sto (MatchPattern val branches) = do
  (sto1, v) <- eval env sto val -- Evaluate the value that we are matching
  checkBranchEval v env sto1 branches -- Try and match the value against the branches list.

-- Part 3 - Interpretation
interpret :: TripleThreatExt -> Maybe TripleThreatVal
interpret expr =
  let elab_expr = elabTerm expr -- First elaborate the expression given.
  -- Then apply the typeOf function to this elaborated expression.
   in case (typeof [] elab_expr) of
        Nothing -> Nothing -- The first case is if the types are not correct, return Nothing.
        -- Otherwise, if something is returned, then run evaluation on elaborated expression.
        Just _ -> case eval [] initStore elab_expr of
          Nothing -> Nothing -- If eval had an error, then return Nothing.
          Just (_, v) -> Just v -- Otherwise, just return the value and not the Store to the use

-- Additional Feature to Get a Clue To Unlock the Key.
getClue :: Int -> IO ()
getClue 1 = putStrLn "Create and return a storage of our favorite EECS class."
getClue 2 = putStrLn "Write an arithmetic expression whose answer is Gen-Z slang."
getClue 3 = putStrLn "Concatenate the clues together."
getClue _ = putStrLn "EH EHHHH EHHHHHHH! Try again by inputting numbers 1-3."

-- Additional Helper to Show Output Value.
showVal :: TripleThreatVal -> String
showVal (NumV n) = show n
showVal (BooleanV b) = show b
showVal (StringV s) = s
showVal (LocV l) = "Location " ++ show l

-- Additional Feature to Reveal the Key.
reveal :: TripleThreatExt -> IO ()
reveal expr =
  case interpret expr of
    Nothing -> putStrLn "Error in your expression. Try again to reveal the key."
    Just x -> case x of
      NumV 662 -> putStrLn "Grad"
      NumV 67 -> putStrLn "UAte ;)"
      StringV "GradUAte ;)" -> putStrLn "Success! You have unlocked the door. TripleThreat out ✌️!"
      _ -> putStrLn (showVal x)

-- Test Cases

-- Result should be NumV 1 for fibonnaci of 2.
testFib =
  interpret
    ( BindX
        "fib"
        (TNum :->: TNum)
        ( FixX
            ( LambdaX
                "g"
                (TNum :->: TNum)
                ( LambdaX
                    "x"
                    TNum
                    ( IfX
                        (LeqX (IdX "x") (NumX 1))
                        (IdX "x")
                        ( PlusX
                            (AppX (IdX "g") (MinusX (IdX "x") (NumX 1)))
                            (AppX (IdX "g") (MinusX (IdX "x") (NumX 2)))
                        )
                    )
                )
            )
        )
        (AppX (IdX "fib") (NumX 2))
    )
    == Just (NumV 1)

-- This is testing if a number is even by subtracting 2 until it becomes less than 1 or 0. Result should be False.
testIsEven =
  interpret
    ( BindX
        "isEven"
        (TNum :->: TBool)
        ( FixX
            ( LambdaX
                "g"
                (TNum :->: TBool)
                ( LambdaX
                    "x"
                    TNum
                    ( IfX
                        (IsZeroX (IdX "x"))
                        (BooleanX True)
                        ( IfX
                            (LeqX (IdX "x") (NumX 1))
                            (BooleanX False)
                            (AppX (IdX "g") (MinusX (IdX "x") (NumX 2)))
                        )
                    )
                )
            )
        )
        (AppX (IdX "isEven") (NumX 67))
    )
    == Just (BooleanV False)

-- STORAGE TEST CASES
testNewDeref =
  interpret (DerefX (NewX (NumX 3)))
    == Just (NumV 3)

testSetSimple =
  interpret (SetX (NewX (NumX 3)) (NumX 6))
    == Just (NumV 6)

testSetThenDeref =
  interpret
    ( BindX
        "l"
        (TLoc TNum)
        (NewX (NumX 3))
        ( BindX
            "_"
            TNum
            (SetX (IdX "l") (NumX 6))
            (DerefX (IdX "l"))
        )
    )
    == Just (NumV 6)

testIncrement =
  interpret
    ( BindX
        "l"
        (TLoc TNum)
        (NewX (NumX 5))
        ( BindX
            "_"
            TNum
            ( SetX
                (IdX "l")
                (PlusX (DerefX (IdX "l")) (NumX 1))
            )
            (DerefX (IdX "l"))
        )
    )
    == Just (NumV 6)

testAliasing =
  interpret
    ( BindX
        "m"
        (TLoc TNum)
        (NewX (NumX 5))
        ( BindX
            "n"
            (TLoc TNum)
            (IdX "m")
            ( BindX
                "_"
                TNum
                (SetX (IdX "m") (NumX 6))
                (DerefX (IdX "n"))
            )
        )
    )
    == Just (NumV 6)

testShadowing =
  interpret
    ( BindX
        "m"
        (TLoc TNum)
        (NewX (NumX 5))
        ( BindX
            "n"
            (TLoc TNum)
            (IdX "m")
            ( BindX
                "n"
                (TLoc TNum)
                (NewX (NumX 7))
                (DerefX (IdX "n"))
            )
        )
    )
    == Just (NumV 7)

testFunctionMutation =
  interpret
    ( BindX
        "inc"
        (TLoc TNum :->: TNum)
        ( LambdaX
            "l"
            (TLoc TNum)
            ( BindX
                "_"
                TNum
                ( SetX
                    (IdX "l")
                    (PlusX (DerefX (IdX "l")) (NumX 1))
                )
                (DerefX (IdX "l"))
            )
        )
        ( BindX
            "n"
            (TLoc TNum)
            (NewX (NumX 5))
            (AppX (IdX "inc") (IdX "n"))
        )
    )
    == Just (NumV 6)

testNestedMutation =
  interpret
    ( BindX
        "l"
        (TLoc TNum)
        (NewX (NumX 1))
        ( BindX
            "_"
            TNum
            ( SetX
                (IdX "l")
                ( PlusX
                    ( BindX
                        "_"
                        TNum
                        (SetX (IdX "l") (NumX 5))
                        (DerefX (IdX "l"))
                    )
                    (NumX 1)
                )
            )
            (DerefX (IdX "l"))
        )
    )
    == Just (NumV 6)

-- CONCAT TEST CASES

testConcatSimple =
  interpret
    (ConcatX (StringLangX "Hello ") (StringLangX "World"))
    == Just (StringV "Hello World")

testConcatEmpty =
  interpret
    (ConcatX (StringLangX "") (StringLangX "abc"))
    == Just (StringV "abc")

testConcatNested =
  interpret
    ( ConcatX
        (ConcatX (StringLangX "A") (StringLangX "B"))
        (StringLangX "C")
    )
    == Just (StringV "ABC")

testConcatWithBind =
  interpret
    ( BindX
        "s"
        TString
        (StringLangX "Hi")
        (ConcatX (IdX "s") (StringLangX "!"))
    )
    == Just (StringV "Hi!")

testConcatShadowing =
  interpret
    ( BindX
        "s"
        TString
        (StringLangX "Hello")
        ( BindX
            "s"
            TString
            (StringLangX "World")
            (ConcatX (IdX "s") (StringLangX "!"))
        )
    )
    == Just (StringV "World!")

testConcatTypeFail =
  interpret
    (ConcatX (StringLangX "A") (NumX 3))

-- expected: Nothing

-- TEST CASES FOR PATTERN MATCHING
testMatchNumExact =
  interpret
    ( MatchPatternX
        (NumX 5)
        [ (PNum 5, StringLangX "five"),
          (PCatcher, StringLangX "other")
        ]
    )
    == Just (StringV "five")

testMatchNumFallback =
  interpret
    ( MatchPatternX
        (NumX 10)
        [ (PNum 5, StringLangX "five"),
          (PCatcher, StringLangX "other")
        ]
    )
    == Just (StringV "other")

testMatchString =
  interpret
    ( MatchPatternX
        (StringLangX "hi")
        [ (PString "bye", NumX 0),
          (PString "hi", NumX 42)
        ]
    )
    == Just (NumV 42)

testMatchNoMatch =
  interpret
    ( MatchPatternX
        (NumX 7)
        [ (PNum 1, NumX 10),
          (PNum 2, NumX 20)
        ]
    )
    == Nothing

testMatchOrder =
  interpret
    ( MatchPatternX
        (NumX 3)
        [ (PNum 3, NumX 100),
          (PCatcher, NumX 999)
        ]
    )
    == Just (NumV 100)

testMatchOnlyCatcher =
  interpret
    ( MatchPatternX
        (BooleanX False)
        [ (PCatcher, StringLangX "always")
        ]
    )
    == Just (StringV "always")

testMatchTypeConsistency =
  interpret
    ( MatchPatternX
        (NumX 1)
        [ (PNum 1, NumX 10),
          (PCatcher, BooleanX True) -- different type
        ]
    )
    == Nothing

allTests =
  [ testNewDeref,
    testSetSimple,
    testSetThenDeref,
    testIncrement,
    testAliasing,
    testShadowing,
    testFunctionMutation,
    testNestedMutation,
    -- Concat tests
    testConcatSimple,
    testConcatEmpty,
    testConcatNested,
    testConcatWithBind,
    testConcatShadowing,
    -- Pattern Matching tests
    testMatchNumExact,
    testMatchNumFallback,
    testMatchString,
    testMatchNoMatch,
    testMatchOrder,
    testMatchOnlyCatcher,
    testMatchTypeConsistency
  ]