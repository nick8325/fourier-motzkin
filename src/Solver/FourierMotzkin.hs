-- | An implementation of Fourier-Motzkin elimination.
--
-- This module defines a solver for systems of linear inequalities
-- over rational numbers. Given a problem it will either give you back
-- "unsatisfiable" or a solution to the problem.
--
-- The implementation is tuned for solving simple problems quickly,
-- rather than solving big problems. It is also quite experimental:
-- expect bugs and API changes!
--
-- See @Example.hs@ for some examples.
module Solver.FourierMotzkin(
  -- * Problems
  Problem, problem, addConstraints, empty, solve,

  -- * Constraints
  --
  -- The solver only supports inequalities; you should encode an
  -- equation @t = u@ as two constraints @t >== u@ and @t <== u@.

  Constraint, (<==), (>==), (</=), (>/=), negateConstraint,

  -- * Linear terms over variables
  Term, var, scalar, (^*), eval, evalSolution,

  -- * Tracing the solution of a problem
  Steps, trace,

  -- * Pretty-printing
  pPrintProblem, pPrintTerm, pPrintConstraint, pPrintSteps) where

import Solver.FourierMotzkin.Internal
