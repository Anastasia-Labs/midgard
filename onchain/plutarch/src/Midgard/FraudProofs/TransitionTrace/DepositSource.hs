module Midgard.FraudProofs.TransitionTrace.DepositSource (popen) where

import Aiken.Cbor (pdeserialise)
import Midgard.FraudProofs.TransitionTrace.FinalYield (PArgs (..), PState (..))
import Midgard.FraudProofs.TransitionTrace.Proof (PAuthenticatedDepositReference (..), pgetAuthenticatedDepositReference)
import Plutarch.LedgerApi.V3 (PCurrencySymbol (..), PTokenName (..), PTxInInfo, PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

popen :: forall s. Term s (PState :--> PArgs :--> PBuiltinList (PAsData PTxInInfo) :--> PPair PAuthenticatedDepositReference (PPair PByteString PByteString))
popen = phoistAcyclic $ plam $ \state args references -> P.do
  PState{pstate'depositSourceCbor} <- pmatch state
  PArgs{pargs'depositEventRefIndex} <- pmatch args
  dat <- plet $ pmatch (pdeserialise # pfromData pstate'depositSourceCbor) $ \case
    PJust value -> value
    PNothing -> perror
  fields <- plet $ pasList # dat
  pif
    (plength # fields #== 3)
    ( P.do
        outRef <- plet $ pfromData $ punsafeCoerce @(PAsData PTxOutRef) $ phead # fields
        policy <- plet $ pasByteStr # (pelemAt # 1 # fields)
        asset <- plet $ pasByteStr # (pelemAt # 2 # fields)
        reference <- plet $ pgetAuthenticatedDepositReference # references # pdata (pcon $ PCurrencySymbol policy) # pdata (pcon $ PTokenName asset) # pfromData pargs'depositEventRefIndex
        PAuthenticatedDepositReference{pauthDeposit'outRef} <- pmatch reference
        pif
          (pfromData pauthDeposit'outRef #== outRef)
          (pcon $ PPair reference $ pcon $ PPair policy asset)
          perror
    )
    perror
