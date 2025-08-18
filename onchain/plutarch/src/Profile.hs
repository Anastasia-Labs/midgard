{-# LANGUAGE ImpredicativeTypes #-}

module Profile where 

import Plutarch.Internal
import Plutarch
import Plutarch.Evaluate
import Plutarch.ByteString 
import Data.Either
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import Data.Text 

getTracesExUnits :: ClosedTerm a -> [Text]
getTracesExUnits term = 
  let (_, _, traces) = fromRight (error "") (evalTerm (Tracing LogInfo DoTracingAndBinds) term)
  in traces 

getExUnits :: ClosedTerm a -> ExBudget
getExUnits term = 
  let (_, budget, _) = fromRight (error "") (evalTerm NoTracing term)
  in budget 

getShowTerm :: ClosedTerm a -> String  
getShowTerm term = 
  let (t, _, _) = fromRight (error "") (evalTerm NoTracing term)
  in 
    case t of 
      Right t' -> printTerm (Tracing LogInfo DoTracingAndBinds) t' 
      Left err -> show err 
