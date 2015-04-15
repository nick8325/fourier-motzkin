-- Some simple examples ofusing the solver.
-- Try with e.g.: solve prob2.

import Solver.FourierMotzkin
import Criterion.Main

x = var 'x'
y = var 'y'
z = var 'z'
w = var 'w'

-- Some sets of constraints to try out.
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

-- Problems from the constraints.
prob0 = problem cs0
prob1 = problem cs1
prob2 = problem cs2
prob3 = problem cs3

-- Extra tests.
prob4 =
  -- Building a problem incrementally.
  addConstraints cs' $
  addConstraints cs $
  empty
  where
    cs = [x + y >== 0, x + 2^*y >== 0]
    cs' = [y >== 0, y <== 0]

-- Solving systems of equations.
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

-- Correctness checks.

-- Should be unsatisfiable
prob6 =
  problem [
    -1 + x - y - z >== 0,
    y >== 1,
    z >== 1,
    x <== 2 ]

-- Unsatisfiable 
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

main = do
  mapM_ (print . solve) [prob0, prob1, prob2, prob3, prob4, prob5, prob6, prob7, prob8, prob9]
  defaultMain [
    bench "prob0" (whnf solve prob0),
    bench "prob1" (whnf solve prob1),
    bench "prob2" (whnf solve prob2),
    bench "prob3" (whnf solve prob3),
    bench "prob5" (whnf solve prob5),
    bench "cs0" (whnf (solve . problem) cs0),
    bench "cs1" (whnf (solve . problem) cs1),
    bench "cs2" (whnf (solve . problem) cs2),
    bench "cs3" (whnf (solve . problem) cs3),
    bench "cs5" (whnf (solve . problem) cs5)]
