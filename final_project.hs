{-
    Comment
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PostfixOperators #-}

import Control.Monad

data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  (:->:) :: KUTypeLang -> KUTypeLang -> KUTypeLang
  TClosure :: String -> KUTypeLang -> Cont -> KUTypeLang
  deriving (Show, Eq)

data KULang where
  Num :: Int -> KULang
  Boolean :: Bool -> KULang
  Id :: String -> KULang
  Plus :: KULang -> KULang -> KULang
  Minus :: KULang -> KULang -> KULang
  Mult :: KULang -> KULang -> KULang
  Div :: KULang -> KULang -> KULang
  Exp :: KULang -> KULang -> KULang
  Between :: KULang -> KULang -> KULang -> KULang
  Lambda :: String -> KUTypeLang -> KULang -> KULang
  App :: KULang -> KULang -> KULang
  If :: KULang -> KULang -> KULang -> KULang
  And :: KULang -> KULang -> KULang
  Or :: KULang -> KULang -> KULang
  Leq :: KULang -> KULang -> KULang
  IsZero :: KULang -> KULang
  Fix :: KULang -> KULang -- Added fix.
  deriving (Show, Eq)

data KULangExt where
  NumX :: Int -> KULangExt
  BooleanX :: Bool -> KULangExt
  IdX :: String -> KULangExt
  PlusX :: KULangExt -> KULangExt -> KULangExt
  MinusX :: KULangExt -> KULangExt -> KULangExt
  MultX :: KULangExt -> KULangExt -> KULangExt
  DivX :: KULangExt -> KULangExt -> KULangExt
  ExpX :: KULangExt -> KULangExt -> KULangExt
  BetweenX :: KULangExt -> KULangExt -> KULangExt -> KULangExt
  LambdaX :: String -> KUTypeLang -> KULangExt -> KULangExt
  AppX :: KULangExt -> KULangExt -> KULangExt
  BindX :: String -> KUTypeLang -> KULangExt -> KULangExt -> KULangExt
  IfX :: KULangExt -> KULangExt -> KULangExt -> KULangExt
  AndX :: KULangExt -> KULangExt -> KULangExt
  OrX :: KULangExt -> KULangExt -> KULangExt
  LeqX :: KULangExt -> KULangExt -> KULangExt
  IsZeroX :: KULangExt -> KULangExt
  FixX :: KULangExt -> KULangExt -- Added fix.
  deriving (Show, Eq)

data KULangVal where
  NumV :: Int -> KULangVal
  BooleanV :: Bool -> KULangVal
  ClosureV :: String -> KULang -> EnvVal -> KULangVal
  deriving (Show, Eq)

type EnvVal = [(String, KULangVal)]

type Cont = [(String, KUTypeLang)]

-- Reader & Helper Methods
data Reader e a = Reader (e -> Maybe a)

ask :: Reader a a
ask = Reader $ \e -> Just e

runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e

local :: (e -> t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> runR r (f e)

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i, v) : e

-- Helper function I created for adding to the Context.
addTypeToContext :: String -> KUTypeLang -> Cont -> Cont
addTypeToContext i v e = (i, v) : e

instance Monad (Reader e) where
  g >>= f = Reader $ \e ->
    case runR g e of
      Nothing -> Nothing
      Just v -> runR (f v) e

instance Functor (Reader e) where
  fmap f (Reader g) = Reader $ \e ->
    case g e of
      Nothing -> Nothing
      Just v -> Just (f v)

instance Applicative (Reader e) where
  pure x = Reader $ \e -> Just x
  (Reader f) <*> (Reader g) = Reader $ \e ->
    case f e of
      Nothing -> Nothing
      Just h ->
        case g e of
          Nothing -> Nothing
          Just x -> Just (h x)

instance MonadFail (Reader e) where
  fail _ = Reader $ \_ -> Nothing

-- ========== Project Exercises ========== --

-- Helper function for direct substitution. sub id=val in body. This function shouldn't fail.
substitution :: String -> KULang -> KULang -> KULang -- Identifier, Value, Body.
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

elabTerm :: KULangExt -> KULang
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

-- Part 1 - Type Inference
typeof :: KULang -> Reader Cont KUTypeLang
-- Check if n is a positive number, then return Maybe TNum, else Nothing.
typeof (Num n) = if n >= 0 then return TNum else fail "Fail: Negative Number"
-- Return a TBool.
typeof (Boolean b) = return TBool
typeof (Id i) = do
  context_env <- ask -- Get the current environment from Reader Monad via ask.
  case lookup i context_env of -- Then, lookup this variable in the environment.
    Just ty -> return ty -- If the variable is found, then return its type.
    Nothing -> fail "Fail: Unbound variable in typeof function" -- Otherwise if not found, the variable is not bound.
typeof (Plus l r) = do
  TNum <- typeof l -- Check if the left is a type of Num as the result.
  TNum <- typeof r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof (Minus l r) = do
  TNum <- typeof l -- Check if the left is a type of Num as the result.
  TNum <- typeof r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof (Mult l r) = do
  TNum <- typeof l -- Check if the left is a type of Num as the result.
  TNum <- typeof r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof (Div l r) = do
  TNum <- typeof l -- Check if the left is a type of Num as the result.
  TNum <- typeof r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof (Exp l r) = do
  TNum <- typeof l -- Check if the left is a type of Num as the result.
  TNum <- typeof r -- Check if the right is a type of Num as the result.
  return TNum -- If it has evaluated here, then return Maybe Num as the result.
typeof (Between first second third) = do
  TNum <- typeof first -- Check if first is a type of Num.
  TNum <- typeof second -- Check if second is a type of Num.
  TNum <- typeof third -- Check if third is a type of Num.
  -- If it has evaluated here, then return TBool.
  return TBool
typeof (Lambda i d b) = do
  -- Get the local context env, but before that, make sure to add the new pair: (i, d) to the context.
  -- Then, get the type of the body to know what the range is.
  r <- local (addTypeToContext i d) (typeof b)
  -- Once we know the range, make the mapping of the function's type be d:->:r.
  return (d :->: r)
typeof (App f a) = do
  apply <- typeof a -- Check that the application to the function is a type in our language.
  (d :->: r) <- typeof f -- The f parameter should be a function, meaning that it should have a D:->:R type.
  -- If the application type is the same as the domain, then this parameter is accepted into the function and the output type of the function is the range.
  -- Otherwise, there's a type mismatch and the function can't be evaluated with this application.
  if apply == d then return r else fail "Fail: The type of the application and the domain of the function do not match."
typeof (If c t e) = do
  TBool <- typeof c -- Check if c is a type of Bool.
  t' <- typeof t -- Check if t is a type of KUTypeLang.
  e' <- typeof e -- Check if e is a type of KUTypeLang.
  -- If it has evaluated here, then check if the branch types are the same for then and else and return that type, else Nothing.
  (if t' == e' then return t' else fail "Fail: The branches don't have the same type.")
typeof (And l r) = do
  TBool <- typeof l -- Check if the left is a type of Boolean.
  TBool <- typeof r -- Check if the right is a type of Boolean.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof (Or l r) = do
  TBool <- typeof l -- Check if the left is a type of Boolean.
  TBool <- typeof r -- Check if the right is a type of Boolean.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof (Leq l r) = do
  TNum <- typeof l -- Check if the left is a type of Num.
  TNum <- typeof r -- Check if the right is a type of Num.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof (IsZero n) = do
  TNum <- typeof n -- Check if n is a type of Num.
  return TBool -- If it has evaluated here, then return Maybe TBool as the result.
typeof (Fix f) = do
  (d :->: r) <- typeof f -- Need to ensure that the given parameter is a function, which has a type of D:->:R.
  return r -- The type of the Fix is the range of the function.

-- Part 2 - Evaluation
eval :: KULang -> Reader EnvVal KULangVal
eval (Num x) = if x < 0 then fail "Fail: Negative Number" else return (NumV x)
eval (Boolean b) = return (BooleanV b) -- Return the Boolean type.
eval (Id i) = do
  env <- ask -- First, get the environment.
  case lookup i env of -- Try to lookup the identifier in the environment.
    Just x -> return x -- If we find it, return it.
    Nothing -> fail "Fail: Unbound variable." -- If we don't find it, then fail.
eval (Plus l r) = do
  -- Evaluate the left and right-hand side and try to do x+y. If error, Monad returns Nothing.
  (NumV x) <- eval l -- Type cast to NumV to check if l is NumV.
  (NumV y) <- eval r -- Type cast to NumV to check if r is NumV.
  return (NumV (x + y)) -- Once both are checked, return x + y and type-cast as Maybe NumV.
eval (Minus l r) = do
  -- Evaluate the left and right-hand side and try to do x-y. If error, Monad returns Nothing.
  (NumV x) <- eval l -- Type cast to NumV to check if l is NumV.
  (NumV y) <- eval r -- Type cast to NumV to check if r is NumV.
  let sub = x - y -- Once both are checked, return x - y and type-cast as Maybe Num if not less than 0.
   in if sub < 0
        then fail "Fail: Negative Number from Minus"
        else
          return (NumV sub)
eval (Mult l r) = do
  -- Evaluate the left and right-hand side and try to do x-y. If error, Monad returns Nothing.
  (NumV x) <- eval l -- Type cast to NumV to check if l is NumV.
  (NumV y) <- eval r -- Type cast to NumV to check if r is NumV.
  return (NumV (x * y)) -- Once both are checked, return x - y and type-cast as Maybe NumV.
eval (Div l r) = do
  -- Evaluate the left and right-hand side and try to do x/y. If error, Monad returns Nothing.
  (NumV x) <- eval l
  (NumV y) <- eval r
  if y == 0 -- Check if the denominator returned as 0, then do Nothing.
    then fail "Fail: Division by zero"
    else
      return (NumV (x `div` y))
eval (Exp l r) = do
  -- Evaluate the left and right-hand side and try to do x-y. If error, Monad returns Nothing.
  (NumV x) <- eval l -- Type cast to NumV to check if l is NumV.
  (NumV y) <- eval r -- Type cast to NumV to check if r is NumV.
  return (NumV (x ^ y)) -- Once both are checked, return x - y and type-cast as Maybe NumV.
eval (Between first second third) = do
  -- First, we need to check that each argument returns as a Num.
  (NumV n1) <- eval first -- Type cast to Num to check if first is Num.
  (NumV n2) <- eval second -- Type cast to Num to check if second is Num.
  (NumV n3) <- eval third -- Type cast to Num to check if third is Num.
  return (BooleanV ((n2 > n1) && (n3 > n2))) -- Return if n2 > n1 and n3 > n2.
eval (Lambda i _ body) = do
  env <- ask -- Get the current global environment via ask.
  return (ClosureV i body env) -- Then, create the Closure of this lambda function with the environment.
eval (App f a) = do
  (ClosureV i b ce) <- eval f -- Check if the function returns a Closure with its corresponding closure environment.
  val <- eval a -- Check that the application returns something in the language.
  -- Use the local closure of the ClosureV by adding the new binding and then evaluate the body with that env.
  local (useClosure i val ce) (eval b)
eval (If c t e) = do
  -- First, we need to check that the condition returns as a Boolean.
  (BooleanV cond) <- eval c -- Type cast to Boolean to check if c is Boolean.
  if cond then eval t else eval e -- If the condition is True, return t' else return e'.
eval (And l r) = do
  -- First, we need to check that both the left and right side are Booleans to evaluate this expression.
  (BooleanV x) <- eval l -- Type cast to Boolean to check if l is Boolean.
  (BooleanV y) <- eval r -- Type cast to Boolean to check if r is Boolean.
  return (BooleanV (x && y)) -- Once both are checked, return x && y and type-cast as Maybe Boolean.
eval (Or l r) = do
  -- First, we need to check that both the left and right side are Booleans to evaluate this expression.
  (BooleanV x) <- eval l -- Type cast to Boolean to check if l is Boolean.
  (BooleanV y) <- eval r -- Type cast to Boolean to check if r is Boolean.
  return (BooleanV (x || y)) -- Once both are checked, return x || y and type-cast as Maybe Boolean.
eval (Leq l r) = do
  -- First, we need to check that both the left and right side are numbers to check which is less.
  (NumV x) <- eval l -- Type cast to Num to check if l is Num.
  (NumV y) <- eval r -- Type cast to Num to check if r is Num.
  return (BooleanV (x <= y)) -- Once both are checked, return if x < y and type-cast as Maybe Boolean.
eval (IsZero n) = do
  -- First, we need to check that the given input is a Num in our language.
  (NumV x) <- eval n -- Type cast to Num to check if n is Num.
  return (BooleanV (x == 0)) -- Once both are checked, return if x == 0 and type-cast as Maybe Boolean.
eval (Fix f) = do
  (ClosureV i b env) <- eval f -- Make sure that the function given returns a ClosureV with the environment.
  -- Get the local environment and we need to keep the function in scope by substituting to keep the Lambda. The type of Lambda no longer matters since we've done typeof.
  -- Then, we need to do eval on this to evaluate the recursion and the body.
  local (const env) (eval (substitution i (Fix (Lambda i TNum b)) b))

-- Part 3 - Add the Fixed Point Operator

-- Part 4 - Interpretation
interpret :: KULangExt -> Maybe KULangVal
interpret expr =
  let elab_expr = elabTerm expr -- First elaborate the expression given.
  -- Then apply the typeOf function to this elaborated expression.
   in case runR (typeof elab_expr) [] of
        Nothing -> Nothing -- The first case is if the types are not correct, return Nothing.
        -- Otherwise, if something is returned, then run evaluation on elaborated expression.
        Just _ -> runR (eval elab_expr) []

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
