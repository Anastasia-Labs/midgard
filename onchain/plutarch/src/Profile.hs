{-# LANGUAGE ImpredicativeTypes #-}

module Profile where

import Data.Either
import Data.Text
import Plutarch.Evaluate
import Plutarch.Internal.Other (printTerm)
import Plutarch.Internal.Term
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

-- | Returns the trace log emitted when evaluating the supplied term.
-- | Evaluates a term with tracing enabled and returns the collected trace log.
getTracesExUnits :: ClosedTerm a -> [Text]
getTracesExUnits term =
  let (_, _, traces) = fromRight (error "") (evalTerm (Tracing LogInfo DoTracingAndBinds) term)
   in traces
-- | Returns the execution budget consumed by the supplied term.

-- | Evaluates a term without tracing and returns the execution budget.
getExUnits :: ClosedTerm a -> ExBudget
getExUnits term =
  let (_, budget, _) = fromRight (error "") (evalTerm NoTracing term)
-- | Pretty-prints the compiled form of the supplied term.
   in budget

-- | Pretty-prints the compiled term or the compilation error if evaluation fails.
getShowTerm :: ClosedTerm a -> String
getShowTerm term =
  let (t, _, _) = fromRight (error "") (evalTerm NoTracing term)
   in case t of
        Right t' -> printTerm (Tracing LogInfo DoTracingAndBinds) t'
        Left err -> show err
