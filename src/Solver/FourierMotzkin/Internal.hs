-- | The guts of the solver. You probably shouldn't need to look in
-- here, and it's all underdocumented.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Solver.FourierMotzkin.Internal where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Ord
import Data.Ratio
import qualified Data.Set as Set
import Data.Set(Set)
import Text.PrettyPrint.HughesPJClass hiding (empty)

-- | A term is a linear combination of variables plus a constant.
--
-- Terms have a 'Num' instance which supports only 'fromInteger',
-- '+' and 'negate'. Use 'var' to make a variable term,
-- 'fromInteger' or 'scalar' to make a constant term,
-- '+' to add terms and '^*' to multiply by a scalar.
data Term a =
  Term {
    constant :: Rational,
    -- Invariant: no coefficient is zero
    vars :: Map a Rational }
  deriving (Eq, Ord)

instance Pretty a => Pretty (Term a) where pPrint = pPrintTerm pPrint
instance Show a => Show (Term a) where show = show . pPrintTerm (text . show)

pPrintTerm :: (a -> Doc) -> Term a -> Doc
pPrintTerm pp (Term a vs)
  | Map.null vs = pPrintRat a
  | a == 0 = pPrintVars vs
  | otherwise = pPrintRat a <+> text "+" <+> pPrintVars vs
  where
    pPrintVars vs = sep (punctuate (text " +") [ pPrint' a <> text "*" <> pp x | (x, a) <- Map.toList vs ])
    pPrint' 1 = text ""
    pPrint' (-1) = text "-"
    pPrint' x = pPrintRat x

-- | A less ugly show function for rationals.
showRat :: Rational -> String
showRat a
  | denominator a == 1 = show (numerator a)
  | otherwise = "(" ++ show (numerator a) ++ "/" ++ show (denominator a) ++ ")"

-- | A pretty-printer for rationals.
pPrintRat :: Rational -> Doc
pPrintRat = text . showRat

-- | A constant term.
scalar :: Rational -> Term a
scalar a = Term a Map.empty

-- | A variable term.
var :: a -> Term a
var x = Term 0 (Map.singleton x 1)

-- | Map a function on rationals over a term.
-- Precondition: @f x /= 0@ if @x /= 0@.
mapTerm :: (Rational -> Rational) -> Term a -> Term a
mapTerm f x =
  Term {
    constant = f (constant x),
    vars = fmap f (vars x) }

instance Ord a => Num (Term a) where
  fromInteger n = scalar (fromInteger n)
  x + y =
    Term {
      constant = constant x + constant y,
      vars = Map.filter (/= 0) (Map.unionWith (+) (vars x) (vars y)) }
  negate = mapTerm negate
  (*) = error "Solver.FourierMotzkin: * not implemented"
  abs = error "Solver.FourierMotzkin: abs not implemented"
  signum = error "Solver.FourierMotzkin: signum not implemented"

-- | Multiply a term by a scalar.
(^*) :: Rational -> Term a -> Term a
0 ^* y = scalar 0
x ^* y = mapTerm (x*) y

-- | Evaluate a term.
eval :: Ord a => Map a Rational -> Term a -> Rational
eval m t =
  constant t +
  sum [ a * Map.findWithDefault err x m | (x, a) <- Map.toList (vars t) ]
  where
    err = error "eval: variable not bound"

-- | A single constraint of the form @t >= 0@ or @t > 0@.
data Bound a =
    -- | @t >= 0@
    Closed { bound :: a }
    -- | @t > 0@
  | Open { bound :: a }
  deriving (Eq, Ord, Functor)

instance Applicative Bound where
  pure = return
  (<*>) = liftM2 ($)

instance Monad Bound where
  return = Closed
  Closed x >>= f = f x
  Open   x >>= f = Open (bound (f x))

instance Pretty a => Pretty (Bound a) where pPrint = pPrintBound pPrint
instance Show a => Show (Bound a) where show = show . pPrintBound (text . show)

pPrintBound :: (a -> Doc) -> Bound a -> Doc
pPrintBound pp (Closed x) = pp x <+> text ">= 0"
pPrintBound pp (Open x) = pp x <+> text "> 0"

pPrintLower :: Doc -> (a -> Doc) -> Bound a -> Doc
pPrintLower x pp (Closed a) = x <+> text ">=" <+> pp a
pPrintLower x pp (Open a) = x <+> text ">" <+> pp a

pPrintUpper :: Doc -> (a -> Doc) -> Bound a -> Doc
pPrintUpper x pp (Closed a) = x <+> text "<=" <+> pp a
pPrintUpper x pp (Open a) = x <+> text "<" <+> pp a

-- | Check if a constant bound is true.
boundTrue :: (Ord a, Num a) => Bound a -> Bool
boundTrue (Closed x) = x >= 0
boundTrue (Open x) = x > 0

-- | A system of inequalities.
data Problem a =
  -- | A problem which is trivially unsolvable.
  Unsolvable | 
  -- | A problem which may or may not be solvable.
  Problem {
    pos    :: Set (Bound (Term a)),   -- ^ Constraints of the form @t >= 0@ where @t@ is a term
    lower  :: Map a (Bound Rational), -- ^ Constraints of the form @x >= k@ or @x > k@ where @x@ is a variable
    upper  :: Map a (Bound Rational), -- ^ Constraints of the form @x <= k@ or @x < k@ where @x@ is a variable
    pvars  :: Set a }                 -- ^ The set of variables in the problem
  deriving (Eq, Ord)

instance Pretty a => Pretty (Problem a) where pPrint = pPrintProblem pPrint
instance Show a => Show (Problem a) where show = show . pPrintProblem (text . show)

pPrintProblem :: (a -> Doc) -> Problem a -> Doc
pPrintProblem pp Unsolvable = text "Unsolvable"
pPrintProblem pp p =
  brackets (sep (punctuate (text ",") xs))
  where
    xs =
      [pPrintBound (pPrintTerm pp) t | t <- Set.toList (pos p)] ++
      [pPrintLower (pp x) pPrintRat b | (x, b) <- Map.toList (lower p)] ++
      [pPrintUpper (pp x) pPrintRat b | (x, b) <- Map.toList (upper p)]

-- | Construct a problem from a list of constraints.
problem :: Ord a => [Constraint a] -> Problem a
problem ts = addConstraints ts empty

-- | The empty problem.
empty :: Problem a
empty = Problem Set.empty Map.empty Map.empty Set.empty

-- | A single constraint.
type Constraint a = Bound (Term a)

infix 4 <==, >==, </=, >/=
(<==), (>==), (</=), (>/=) :: Ord a => Term a -> Term a -> Constraint a

-- | Less than or equal.
t <== u = Closed (u - t)

-- | Greater than or equal.
t >== u = u <== t

-- | Strictly less than.
t </= u = Open (u - t)

-- | Strictly greater than.
t >/= u = u </= t

-- | Invert a single constraint.
negateBound :: Ord a => Bound (Term a) -> Bound (Term a)
negateBound (Closed t) = Open (-t)
negateBound (Open t) = Closed (-t)

-- | Add a list of constraints to an existing problem.
addConstraints :: Ord a => [Constraint a] -> Problem a -> Problem a
addConstraints _ Unsolvable = Unsolvable
addConstraints ts p =
  addDerivedTerms ts p { pvars = Set.union vs (pvars p) }
  where
    vs = Set.unions (map (Set.fromAscList . Map.keys . vars . bound) ts)

-- | A slightly more efficient version of 'addConstraints',
-- which assumes that the new constraints don't add any variables
-- that haven't appeared before.
addDerivedTerms :: Ord a => [Bound (Term a)] -> Problem a -> Problem a
addDerivedTerms _ Unsolvable = Unsolvable
addDerivedTerms ts p = foldr addTerm (addBounds bs p) us
  where
    (bs, us) = partition ((== 1) . Map.size . vars . bound) ts

-- | Internal function to add a single constraint to a problem.
addTerm :: Ord a => Bound (Term a) -> Problem a -> Problem a
addTerm _ Unsolvable = Unsolvable
addTerm t p
  | Map.null (vars (bound t)) =
    if boundTrue (fmap constant t) then p else Unsolvable
  | t `Set.member` pos p || redundant p t = p
  | otherwise =
    p { pos = Set.insert t (Set.filter (not . implies p t) (pos p)) }

-- | Internal function to add constraints that only contain one
-- variable to the problem.
addBounds :: Ord a => [Bound (Term a)] -> Problem a -> Problem a
addBounds [] p = p
addBounds bs p =
  prune p { lower = Map.unionWith bmax (lower p) lower',
            upper = Map.unionWith bmin (upper p) upper' }
  where
    (lower', upper') = foldr op (Map.empty, Map.empty) bs
    op t (l, u)
      | a > 0 = (Map.insertWith bmax x b l, u)
      | a < 0 = (l, Map.insertWith bmin x b u)
      where
        (x, a) = Map.findMin (vars (bound t))
        b = fmap (\t -> negate (constant t) / a) t

-- | Take the conjunction or disjunction of two bounds.
bmax, bmin :: (Ord a, Num a) => Bound a -> Bound a -> Bound a
bmax x y
  | bound x > bound y = x
  | bound y > bound x = y
  | otherwise = liftM2 const x y
bmin x y = fmap negate (bmax (fmap negate x) (fmap negate y))

-- | Remove redundant constraints from a problem.
prune :: Ord a => Problem a -> Problem a
prune p =
  p { pos = Set.filter (not . redundant p) (pos p) }

-- | Is a constraint redundant in a rpoblem?
redundant p t =
  trivial p t ||
  or [ implies p u t && (t < u || not (implies p t u)) | u <- Set.toList (pos p), t /= u ]

-- | Does one constraint imply another in a problem?
implies :: Ord a => Problem a -> Bound (Term a) -> Bound (Term a) -> Bool
-- a1x1+...+anxn + b >= 0 ==> c1x1+...+cnxn + d >= 0
-- <=>
-- (c1-a1)x1+...+(cn-an)x2 + d - b >= 0
implies p t u = trivial p (b t u (bound u - bound t))
  where
    b Closed{} Open{} = Open
    b _        _      = Closed

-- | Is a constraint trivial in a problem?
trivial :: Ord a => Problem a -> Bound (Term a) -> Bool
trivial p t =
  case minValue p (bound t) of
   Just a -> check a t
   _ -> False
  where
    check (Closed x) Open{} = x > 0
    check x _ = bound x >= 0

-- | Attempt to put a lower bound on the value of a term in a problem.
-- N.B. normally returns @Nothing@, not to be used for optimising an
-- objective function.
minValue :: Ord a => Problem a -> Term a -> Maybe (Bound Rational)
minValue p t = do
  as <- mapM varValue (Map.toList (vars t))
  return (fmap (constant t +) (fmap sum (sequence as)))
  where
    varValue (x, a) =
      fmap (fmap (a*))
        (Map.lookup x (if a > 0 then lower p else upper p))

-- | One step in solving a problem.
data Step a =
    -- | The problem couldn't be solved.
    StopUnsolvable
    -- | The problem is tautological.
  | StopSolved
    -- | @Eliminate x ls us p@ means that we eliminate the variable
    --   @x@ to get the problem @p@. The solution is then constrained
    --   to have @x <= l@ for all @l@ in @ls@, and @x >= u@ for all
    --   @u@ in @us@.
  | Eliminate a [Constraint a] [Constraint a] (Problem a)

instance Pretty a => Pretty (Step a) where pPrint = pPrintStep pPrint
instance Show a => Show (Step a) where show = show . pPrintStep (text . show)

pPrintStep :: (a -> Doc) -> Step a -> Doc
pPrintStep pp StopUnsolvable = text "Stop, unsolvable"
pPrintStep pp StopSolved = text "Stop, solved"
pPrintStep pp (Eliminate x ls us p) =
  sep [
    text "Eliminate" <+> pp x <+> text "with bounds",
    nest 2 $
      pPrintList $
        [ pPrintLower (pp x) (pPrintTerm pp) l | l <- ls ] ++
        [ pPrintUpper (pp x) (pPrintTerm pp) u | u <- us ],
    text "giving",
    nest 2 (pPrintProblem pp p)]
  where
    pPrintList = brackets . fsep . punctuate comma

-- | calculate the possible elimination steps for a problem.
eliminations :: Ord a => Problem a -> [Step a]
eliminations p =
  map snd .
  sortBy (comparing fst) $
    [ eliminate x p | x <- Set.toList (pvars p) ]

eliminate :: Ord a => a -> Problem a -> (Int, Step a)
eliminate x p =
  -- Number of terms added by the elimination
  (length ls * length us - length ls - length us,
   -- If we have c >= x >= c, eliminate x by doing ls >= c, c >= rs,
   -- otherwise generate ls >= rs
   case nontrivial ls && nontrivial us && any ((== 0) . bound) ts of
     False ->
       Eliminate x ls us (addDerivedTerms ts p')
     True ->
       case sortBy (comparing (Map.size . vars . bound)) (intersect ls us) of
        Closed c:_ ->
          let ts = [fmap (subtract c) t | t <- us] ++ [fmap (c -) u | u <- ls] in
          Eliminate x [Closed c] [Closed c] (addDerivedTerms ts p')
        _ ->
          -- c > x > c or similar
          Eliminate x ls us Unsolvable)
  where
    (ls, us, p') = focus x p
    ts = [ liftM2 (-) t u | t <- us, u <- ls ]
    nontrivial (_:_:_) = True
    nontrivial _ = False

focus :: Ord a => a -> Problem a -> ([Bound (Term a)], [Bound (Term a)], Problem a)
focus x p = (ls', us', p' { pos = pos' })
  where
    p' = p {
      lower = Map.delete x (lower p),
      upper = Map.delete x (upper p),
      pvars = Set.delete x (pvars p) }
    ((ls', us'), pos') = foldDelete op (ls, us) (pos p')
    (ls, us) = (boundFor (lower p), boundFor (upper p))
    boundFor s = maybeToList (fmap (fmap scalar) (Map.lookup x s))
    op t (ls, us) = do
      let vs = vars (bound t)
      a <- Map.lookup x vs
      let b = negate (recip a) ^* (bound t) { vars = Map.delete x vs }
      if a > 0
        then return (t { bound = b }:ls, us)
        else return (ls, t { bound = b }:us)

foldDelete :: Ord a => (a -> b -> Maybe b) -> b -> Set a -> (b, Set a)
foldDelete op e s = Set.foldr op' (e, s) s
  where
    op' x (y, s) =
      case op x y of
        Nothing -> (y, s)
        Just y' -> (y', Set.delete x s)

solve :: Ord a => Problem a -> Maybe (Map a Rational)
solve Unsolvable = Nothing
solve p | Set.null (pos p) =
  fmap Map.fromList $
    forM (Set.toList (pvars p)) $ \x -> do
      let l = Map.lookup x (lower p)
          u = Map.lookup x (upper p)
      a <- solveBounds (l, u)
      return (x, a)
solve p = do
  m <- solve p'
  let Just a = solveBounds (try (foldr1 bmax) (map (fmap (eval m)) ls),
                            try (foldr1 bmin) (map (fmap (eval m)) us))
  return (Map.insert x a m)
  where
    Eliminate x ls us p':_ = eliminations p
    try f [] = Nothing
    try f xs = Just (f xs)

solveBounds :: (Maybe (Bound Rational), Maybe (Bound Rational)) -> Maybe Rational
solveBounds (Just x, Just y)
  | empty x y = Nothing
    -- Try to give an integer solution if possible.
  | boundTrue (fmap (subtract a) y) = Just a
  | otherwise = Just ((bound x+bound y)/2)
  where
    empty (Closed x) (Closed y) = x > y
    empty x y = bound x >= bound y
    a = fromInteger (floor (bound x+1))
solveBounds (x, y) =
  fmap solveBound x `mplus`
  fmap (negate . solveBound . fmap negate) y `mplus`
  return 0
  where
    solveBound (Closed x) = x
    solveBound (Open x) = fromInteger (floor (x+1))

-- Debugging function
trace :: Ord a => Problem a -> [Step a]
trace Unsolvable = [StopUnsolvable]
trace p | Set.null (pos p) = [StopSolved]
trace p = s:trace p'
  where
    s@(Eliminate _ _ _ p'):_ = eliminations p

x = var 'x'
y = var 'y'
z = var 'z'
w = var 'w'

cs0 = [
  x >== 1,
  y >== 1,
  z >== 1,
  w >== 1,
  (-1) ^* x + 1 ^* y <== -1,
  x + y - w + 1 <== -1,
  x - z - w - 1 <== -1,
  x + y - z + w + 2 >== 0,
  y - z + w + 1 >== 0 ]
cs1 = cs0 ++ [x - y <== -1]
cs2 = cs0 ++ [x - y >== 1]
cs3 = cs0 ++ [x - y <== 1, x - y >== 1]

prob0 = problem cs0
prob1 = problem cs1
prob2 = problem cs2
prob3 = problem cs3

prob4 =
  addConstraints cs' $
  addConstraints cs $
  empty
  where
    cs = [x + y >== 0, x + 2^*y >== 0]
    cs' = [y >== 0, y <== 0]

cs5 =
  concat [
    x - 3^*y + 2^*z + w === -4,
    2^*x - 6^*y + z + 4^*w === 1,
    -1^*x + 2^*y + 3^*z + 4^*w === 12,
    -1^*y + z + w === 0 ]
  where
    infix 4 ===
    x === y = [x >== y, x <== y]

prob5 = problem cs5

-- Should be unsatisfiable
prob6 =
  problem [
    -1 + x - y - z >== 0,
    y >== 1,
    z >== 1,
    x <== 2 ]

prob7 =
  problem [
    1 <== x + y,
    2 <== 0 ]

-- Satisfiable.
prob8 =
  problem [
    -1 - x + y - z >== 0,
    -w + x >== 0,
    1 + x - y + z >== 0,
    1 + w + x - y >/= 0,
    x >== 1,
    y >== 1,
    z >== 1,
    w >== 1,
    8 - w - x - y - z >== 0]

-- Satisfiable.
prob9 =
  problem [
    z >== x + y + 1,
    x + y + 1 >== z,
    x >== 1,
    y >== 1,
    z >== 1,
    z >/= 1 ]

--main =
-- defaultMain [
--   bench "prob0" (whnf solve prob0),
--   bench "prob1" (whnf solve prob1),
--   bench "prob2" (whnf solve prob2),
--   bench "prob3" (whnf solve prob3),
--   bench "prob5" (whnf solve prob5),
--   bench "cs0" (whnf (solve . problem) cs0),
--   bench "cs1" (whnf (solve . problem) cs1),
--   bench "cs2" (whnf (solve . problem) cs2),
--   bench "cs3" (whnf (solve . problem) cs3),
--   bench "cs5" (whnf (solve . problem) cs5)]

-- {-# NOINLINE go #-}
-- go :: (a -> b) -> a -> c -> IO ()
-- go f x _ = f x `seq` return ()

-- main =
--   forM_ [1..100000] (go (solve . problem) cs)
