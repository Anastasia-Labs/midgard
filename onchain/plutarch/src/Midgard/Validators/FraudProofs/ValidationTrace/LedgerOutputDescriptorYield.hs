module Midgard.Validators.FraudProofs.ValidationTrace.LedgerOutputDescriptorYield (
    scanFactsValidator,
    valueSummaryValidator,
    datumSummaryValidator,
    referenceScriptValidator,
) where

import Midgard.LedgerOutputDescriptorYield qualified as Yield
import Midgard.LedgerOutputProofDescriptor qualified as Descriptor
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

pwithClaim :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash))) -> Term s PScriptContext -> (Yield.PDescriptorClaim s -> Term s PBool) -> Term s PUnit
pwithClaim dispatchers ctx predicate = pmatch ctx $ \PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} ->
    pmatch pscriptContext'scriptInfo $ \case
        PRewardingScript _ -> pmatch (Yield.pdispatch # pfromData dispatchers # pscriptContext'txInfo) $ \claim -> pif (predicate claim) (pconstant ()) perror
        _ -> perror

scanFactsValidator, valueSummaryValidator, datumSummaryValidator, referenceScriptValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
scanFactsValidator = plam $ \dispatchers ctx -> pwithClaim dispatchers ctx $ \c ->
    Descriptor.pscanFactsArePinned # Yield.pclaim'control c # Yield.pclaim'descriptorCbor c # Yield.pclaim'valueSummary c # Yield.pclaim'datumSummary c
valueSummaryValidator = plam $ \dispatchers ctx -> pwithClaim dispatchers ctx $ \c ->
    Descriptor.pvalueSummaryIsPinned # Yield.pclaim'control c # Yield.pclaim'valueSummary c
datumSummaryValidator = plam $ \dispatchers ctx -> pwithClaim dispatchers ctx $ \c ->
    Descriptor.pdatumSummaryIsPinned # Yield.pclaim'control c # Yield.pclaim'datumSummary c
referenceScriptValidator = plam $ \dispatchers ctx -> pwithClaim dispatchers ctx $ \c ->
    Descriptor.preferenceScriptIsPinned # Yield.pclaim'control c # Yield.pclaim'descriptorCbor c
