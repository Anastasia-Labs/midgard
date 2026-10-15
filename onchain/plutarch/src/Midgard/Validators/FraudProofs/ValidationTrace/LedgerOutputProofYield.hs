-- | Rewarding validators for the shared output-proof stage and scalar roles.
module Midgard.Validators.FraudProofs.ValidationTrace.LedgerOutputProofYield (
    datumAdvanceBytesValidator,
    datumAdvanceIntegerValidator,
    datumAttachBytesValidator,
    datumAttachIntegerValidator,
    datumCloseValidator,
    datumFinalizeFrameValidator,
    datumFinishValidator,
    datumFoldListValidator,
    datumFoldMapValidator,
    datumHeadLargeConstructorValidator,
    datumHeadMapValidator,
    datumHeadScalarValidator,
    datumHeadSequenceValidator,
    datumLargeConstructorValidator,
    datumLargeFieldsValidator,
    nativeScriptValidator,
    referenceScriptValidator,
    scalarBytesValidator,
    scalarIntegerValidator,
    scriptHashValidator,
    spanValidator,
    structureAssetsValidator,
    structureFinishValidator,
    structureOptionalValidator,
    structureValidator,
    valueValidator,
) where

import Midgard.LedgerOutputProofDatum qualified as Datum
import Midgard.LedgerOutputProofStages qualified as Stages
import Midgard.LedgerOutputProofYield qualified as Yield
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

pwithClaim :: forall s. Maybe Integer -> Term s (PAsData (PBuiltinList (PAsData PScriptHash))) -> Term s PScriptContext -> (Term s Yield.PClaim -> Term s PBool) -> Term s PUnit
pwithClaim role dispatchers ctx predicate = P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    pmatch pscriptContext'scriptInfo $ \case
        PRewardingScript _ -> P.do
            PTxInfo{ptxInfo'inputs, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
            claim <- plet $ case role of
                Just expected -> Yield.pdispatch # pfromData dispatchers # pconstant expected # pfromData ptxInfo'inputs # (pto $ pto $ pfromData ptxInfo'redeemers)
                Nothing -> Yield.pdispatchAttestation # pfromData dispatchers # pfromData ptxInfo'inputs # (pto $ pto $ pfromData ptxInfo'redeemers)
            pif (predicate claim) (pconstant ()) perror
        _ -> perror

datumAdvanceBytesValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumAdvanceBytesValidator = plam $ \dispatchers ctx -> pwithClaim (Just 18) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.padvanceBytes # Yield.pclaim'control c # Yield.pclaim'witness c # Yield.pclaim'claimedScalar c)

datumAdvanceIntegerValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumAdvanceIntegerValidator = plam $ \dispatchers ctx -> pwithClaim (Just 7) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.padvanceInteger # Yield.pclaim'control c # Yield.pclaim'witness c # Yield.pclaim'claimedScalar c)

datumAttachBytesValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumAttachBytesValidator = plam $ \dispatchers ctx -> pwithClaim (Just 17) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pattachBytes # Yield.pclaim'control c # Yield.pclaim'witness c # Yield.pclaim'claimedScalar c)

datumAttachIntegerValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumAttachIntegerValidator = plam $ \dispatchers ctx -> pwithClaim (Just 5) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pattachInteger # Yield.pclaim'control c # Yield.pclaim'witness c # Yield.pclaim'claimedScalar c)

datumCloseValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumCloseValidator = plam $ \dispatchers ctx -> pwithClaim (Just 22) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pclose # Yield.pclaim'control c # Yield.pclaim'witness c)

datumFinalizeFrameValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumFinalizeFrameValidator = plam $ \dispatchers ctx -> pwithClaim (Just 3) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pfinalizeFrame # Yield.pclaim'control c # Yield.pclaim'witness c)

datumFinishValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumFinishValidator = plam $ \dispatchers ctx -> pwithClaim (Just 19) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pfinish # Yield.pclaim'control c # Yield.pclaim'witness c)

datumFoldListValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumFoldListValidator = plam $ \dispatchers ctx -> pwithClaim (Just 6) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pfoldList # Yield.pclaim'control c # Yield.pclaim'witness c)

datumFoldMapValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumFoldMapValidator = plam $ \dispatchers ctx -> pwithClaim (Just 2) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pfoldMap # Yield.pclaim'control c # Yield.pclaim'witness c)

datumHeadLargeConstructorValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumHeadLargeConstructorValidator = plam $ \dispatchers ctx -> pwithClaim (Just 16) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pheadLargeConstructor # Yield.pclaim'control c # Yield.pclaim'witness c)

datumHeadMapValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumHeadMapValidator = plam $ \dispatchers ctx -> pwithClaim (Just 15) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pheadMap # Yield.pclaim'control c # Yield.pclaim'witness c)

datumHeadScalarValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumHeadScalarValidator = plam $ \dispatchers ctx -> pwithClaim (Just 4) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pheadScalar # Yield.pclaim'control c # Yield.pclaim'witness c)

datumHeadSequenceValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumHeadSequenceValidator = plam $ \dispatchers ctx -> pwithClaim (Just 14) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.pheadSequence # Yield.pclaim'control c # Yield.pclaim'witness c)

datumLargeConstructorValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumLargeConstructorValidator = plam $ \dispatchers ctx -> pwithClaim (Just 20) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.padvanceLargeConstructor # Yield.pclaim'control c # Yield.pclaim'witness c)

datumLargeFieldsValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
datumLargeFieldsValidator = plam $ \dispatchers ctx -> pwithClaim (Just 21) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Datum.padvanceLargeFields # Yield.pclaim'control c # Yield.pclaim'witness c)

nativeScriptValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
nativeScriptValidator = plam $ \dispatchers ctx -> pwithClaim (Just 10) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pnativeScript # Yield.pclaim'control c # (Yield.pnativeWitness # Yield.pclaim'witness c))

referenceScriptValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
referenceScriptValidator = plam $ \dispatchers ctx -> pwithClaim (Just 8) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.preferenceScript # Yield.pclaim'control c # (Yield.pwindowWitness # Yield.pclaim'witness c))

scalarBytesValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
scalarBytesValidator = plam $ \dispatchers ctx -> pwithClaim Nothing dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Datum.pbytesScalarClaimIsExact # Yield.pclaim'control c # Yield.pclaim'claimedScalar c

scalarIntegerValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
scalarIntegerValidator = plam $ \dispatchers ctx -> pwithClaim Nothing dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Datum.pintegerScalarClaimIsExact # Yield.pclaim'control c # Yield.pclaim'claimedScalar c

scriptHashValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
scriptHashValidator = plam $ \dispatchers ctx -> pwithClaim (Just 9) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pscriptHash # Yield.pclaim'control c # (Yield.pwindowWitness # Yield.pclaim'witness c))

spanValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
spanValidator = plam $ \dispatchers ctx -> pwithClaim (Just 23) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pspanAttach # Yield.pclaim'control c # (Yield.pspanAttachWitness # Yield.pclaim'witness c))

structureAssetsValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
structureAssetsValidator = plam $ \dispatchers ctx -> pwithClaim (Just 11) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pstructureAssets # Yield.pclaim'control c # (Yield.pchunkWitness # Yield.pclaim'witness c))

structureFinishValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
structureFinishValidator = plam $ \dispatchers ctx -> pwithClaim (Just 13) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pstructureFinish # Yield.pclaim'control c # (Yield.pchunkWitness # Yield.pclaim'witness c))

structureOptionalValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
structureOptionalValidator = plam $ \dispatchers ctx -> pwithClaim (Just 12) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pstructureOptional # Yield.pclaim'control c # (Yield.pchunkWitness # Yield.pclaim'witness c))

structureValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
structureValidator = plam $ \dispatchers ctx -> pwithClaim (Just 0) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pstructureHeaders # Yield.pclaim'control c # (Yield.pchunkWitness # Yield.pclaim'witness c))

valueValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
valueValidator = plam $ \dispatchers ctx -> pwithClaim (Just 1) dispatchers ctx $ \claim -> pmatch claim $ \c ->
    Yield.pattest # claim # (Stages.pvalueFold # Yield.pclaim'control c # (Yield.pvalueWitness # Yield.pclaim'witness c))
