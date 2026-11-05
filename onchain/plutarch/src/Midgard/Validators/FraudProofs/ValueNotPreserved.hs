module Midgard.Validators.FraudProofs.ValueNotPreserved (
    valueNotPreservedStep01Validator,
    valueNotPreservedStep02Validator,
    valueNotPreservedStep03Validator,
    valueNotPreservedStep04Validator,
) where

import Midgard.FraudProofs.ValueUnion (PConservationClaim (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
    PAddress,
    PCurrencySymbol,
    PScriptContext,
    PScriptHash,
    PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStep)
import Midgard.FraudProofs.FieldOpening (
    PNativeTxAnchorV1 (..),
    PNativeTxOpeningV1 (..),
    panchoredFieldWalk,
    panchoredNativeTx,
    pfoldOpenedField,
    pmintFieldIndex,
    popenedFieldView,
    poutputsFieldIndex,
    pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Types (
    PNativeTxBodyCompact (..),
    PNativeTxCompact (..),
    PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ValueNotPreserved (
    PClaimedAssetV1 (..),
    PStep01Args (..),
    PStep02Args (..),
    PStep02FoldArgs (..),
    PStep02State (..),
    PStep03Args (..),
    PStep03State (..),
    PStep04Args (..),
    PStep04State (..),
    pclaimedAssetIsWellFormedV1,
    pfraudCategoryIsValueNotPreservedV1,
    pmintItemClaimedQuantityV1,
    poutputClaimedQuantityV1,
    pspentInputClaimedQuantityV1,
    pvalueNotPreservedFaultIsEstablishedV1,
 )
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.NativeTxMachineWalk (pspendInputAt, pspendInputCount)
import Midgard.Validators.FraudProofs.Step (
    pdispatch,
    pexpectDatum,
    pexpectStateAs,
    pexpecting,
    pstateIsAbsent,
    pstep,
 )

valueNotPreservedStep01Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
valueNotPreservedStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle acceptedSourceHash forcedSourceHash ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
            pmatch args $ \case
                PLaunchUnion inputIndex outputIndex claim -> P.do
                    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
                    pcontinue computationThreadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownOutRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ prior outputHash outputState ->
                        pstateIsAbsent prior
                            #&& outputState
                            #== claim
                            #&& pmatch
                                (punsafeCoerce @PConservationClaim claim)
                                ( \case
                                    PAcceptedImbalance asset _ -> pclaimedAssetIsWellFormedV1 # pfromData asset #&& outputHash #== acceptedSourceHash
                                    PForcedConservation -> outputHash #== forcedSourceHash
                                )
                PStep01Args{pstep01Args'txInclusion, pstep01Args'claimedAsset, pstep01Args'claimedDirection} -> P.do
                    PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
                    claimedAsset <- plet $ pfromData pstep01Args'claimedAsset
                    ppassNativeTxToNextStep
                        computationThreadPolicy
                        hubOracle
                        datum
                        (pfromData pstep01Args'txInclusion)
                        ownOutRef
                        (pfromData ptxInfo'inputs)
                        (pfromData ptxInfo'referenceInputs)
                        (pfromData ptxInfo'outputs)
                        (pto $ pto $ pfromData ptxInfo'redeemers)
                        $ \_ownScriptHash _threadName _prover _inputState outputScriptHash outputStateData header badTxId badTxView -> P.do
                            PHeaderV1{pheader'prevUtxosRoot} <- pmatch $ pfromData header
                            PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch badTxView
                            PNativeTxCompact{pcompact'body, pcompact'validityCode} <- pmatch pverified'txCompact
                            PNativeTxBodyCompact{pbodyCompact'fee} <- pmatch pcompact'body
                            expected <-
                                plet $
                                    pcon $
                                        PStep02State
                                            (pdata badTxId)
                                            pstep01Args'claimedAsset
                                            pstep01Args'claimedDirection
                                            (pdata pbodyCompact'fee)
                                            pheader'prevUtxosRoot
                                            (pdata 0)
                                            (pdata 0)
                            pexpecting (pcompact'validityCode #== 0) $
                                pexpecting (pclaimedAssetIsWellFormedV1 # claimedAsset) $
                                    pexpecting (outputScriptHash #== step02ScriptHash) $
                                        pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

valueNotPreservedStep02Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueNotPreservedStep02Validator = plam $ \step03ScriptHash computationThreadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action ->
            pmatch action $ \case
                PFoldInput foldArgsD -> P.do
                    PStep02FoldArgs
                        { pstep02FoldArgs'inputIndex
                        , pstep02FoldArgs'outputIndex
                        , pstep02FoldArgs'spendInputsOpening
                        , pstep02FoldArgs'valueWitness
                        } <-
                        pmatch $ pfromData foldArgsD
                    PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
                    pcontinue
                        computationThreadPolicy
                        (pexpectDatum datum)
                        (pfromData pstep02FoldArgs'inputIndex)
                        (pfromData pstep02FoldArgs'outputIndex)
                        ownOutRef
                        (pfromData ptxInfo'inputs)
                        (pfromData ptxInfo'outputs)
                        $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                            state <- plet $ pexpectStateAs @PStep02State inputState
                            PStep02State
                                { pstep02State'badTxId
                                , pstep02State'claimedAsset
                                , pstep02State'claimedDirection
                                , pstep02State'committedFee
                                , pstep02State'prevUtxosRoot
                                , pstep02State'inputCursor
                                , pstep02State'claimedDelta
                                } <-
                                pmatch state
                            view <-
                                plet $
                                    popenedFieldView
                                        # pfromData pstep02FoldArgs'spendInputsOpening
                                        # pcon (PBodyAnchor pstep02State'badTxId)
                                        # pspendInputsFieldIndex
                                        # pfromData ptxInfo'referenceInputs
                                        # certificatePolicy
                            spendInput <- plet $ pspendInputAt # view # pfromData pstep02State'inputCursor
                            claimedQuantity <-
                                plet $
                                    pspentInputClaimedQuantityV1
                                        # pfromData pstep02State'prevUtxosRoot
                                        # spendInput
                                        # pfromData pstep02FoldArgs'valueWitness
                                        # pfromData pstep02State'claimedAsset
                            expected <-
                                plet $
                                    pcon $
                                        PStep02State
                                            pstep02State'badTxId
                                            pstep02State'claimedAsset
                                            pstep02State'claimedDirection
                                            pstep02State'committedFee
                                            pstep02State'prevUtxosRoot
                                            (pdata $ pfromData pstep02State'inputCursor + 1)
                                            (pdata $ pfromData pstep02State'claimedDelta + claimedQuantity)
                            pexpecting (outputScriptHash #== ownScriptHash) $
                                pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)
                PFinishInputs inputIndexD outputIndexD spendInputsOpeningD -> P.do
                    PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
                    pcontinue
                        computationThreadPolicy
                        (pexpectDatum datum)
                        (pfromData inputIndexD)
                        (pfromData outputIndexD)
                        ownOutRef
                        (pfromData ptxInfo'inputs)
                        (pfromData ptxInfo'outputs)
                        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                            state <- plet $ pexpectStateAs @PStep02State inputState
                            PStep02State
                                { pstep02State'badTxId
                                , pstep02State'claimedAsset
                                , pstep02State'claimedDirection
                                , pstep02State'committedFee
                                , pstep02State'inputCursor
                                , pstep02State'claimedDelta
                                } <-
                                pmatch state
                            view <-
                                plet $
                                    popenedFieldView
                                        # pfromData spendInputsOpeningD
                                        # pcon (PBodyAnchor pstep02State'badTxId)
                                        # pspendInputsFieldIndex
                                        # pfromData ptxInfo'referenceInputs
                                        # certificatePolicy
                            expected <-
                                plet $
                                    pcon $
                                        PStep03State
                                            pstep02State'badTxId
                                            pstep02State'claimedAsset
                                            pstep02State'claimedDirection
                                            pstep02State'committedFee
                                            pstep02State'claimedDelta
                            pexpecting (pfromData pstep02State'inputCursor #== pspendInputCount # view) $
                                pexpecting (outputScriptHash #== step03ScriptHash) $
                                    pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

valueNotPreservedStep03Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueNotPreservedStep03Validator = plam $ \step04ScriptHash computationThreadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep03Args
                { pstep03Args'inputIndex
                , pstep03Args'outputIndex
                , pstep03Args'nativeTxCompactCbor
                , pstep03Args'outputsCarriage
                , pstep03Args'mintCarriage
                } <-
                pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep03Args'inputIndex)
                (pfromData pstep03Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    state <- plet $ pexpectStateAs @PStep03State inputState
                    PStep03State
                        { pstep03State'badTxId
                        , pstep03State'claimedAsset
                        , pstep03State'claimedDirection
                        , pstep03State'committedFee
                        , pstep03State'claimedDelta
                        } <-
                        pmatch state
                    claimedAsset <- plet $ pfromData pstep03State'claimedAsset
                    anchored <-
                        plet $
                            panchoredNativeTx
                                # pcon (PBodyTxOpening $ pfromData pstep03Args'nativeTxCompactCbor)
                                # pcon (PBodyAnchor pstep03State'badTxId)
                    outputsWalk <-
                        plet $
                            panchoredFieldWalk
                                # anchored
                                # poutputsFieldIndex
                                # pfromData pstep03Args'outputsCarriage
                                # pfromData ptxInfo'referenceInputs
                                # certificatePolicy
                    deltaAfterOutputs <-
                        plet $
                            pfoldOpenedField @PInteger
                                # outputsWalk
                                # pfromData pstep03State'claimedDelta
                                # plam (\accumulated _index outputItem -> accumulated - poutputClaimedQuantityV1 # outputItem # claimedAsset)
                    finalDelta <-
                        plet $
                            pmatch claimedAsset $ \case
                                PAdaAsset ->
                                    pmatch pstep03Args'mintCarriage $ \case
                                        PDNothing -> deltaAfterOutputs - pfromData pstep03State'committedFee
                                        PDJust _ -> perror
                                PTokenAsset _ _ ->
                                    pmatch pstep03Args'mintCarriage $ \case
                                        PDNothing -> perror
                                        PDJust mintCarriageD ->
                                            pfoldOpenedField @PInteger
                                                # ( panchoredFieldWalk
                                                        # anchored
                                                        # pmintFieldIndex
                                                        # pfromData mintCarriageD
                                                        # pfromData ptxInfo'referenceInputs
                                                        # certificatePolicy
                                                  )
                                                # deltaAfterOutputs
                                                # plam (\accumulated _index mintItem -> accumulated + pmintItemClaimedQuantityV1 # mintItem # claimedAsset)
                    expected <-
                        plet $
                            pcon $
                                PStep04State
                                    pstep03State'badTxId
                                    pstep03State'claimedAsset
                                    pstep03State'claimedDirection
                                    (pdata finalDelta)
                    pexpecting (outputScriptHash #== step04ScriptHash) $
                        pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

valueNotPreservedStep04Validator ::
    forall s.
    Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueNotPreservedStep04Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep04Args{pstep04Args'inputIndex, pstep04Args'outputIndex, pstep04Args'fraudProofMintRedeemerIndex} <- pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            pfinalize
                computationThreadPolicy
                fraudProofPolicy
                fraudProofAddress
                (pexpectDatum datum)
                (pfromData pstep04Args'inputIndex)
                (pfromData pstep04Args'outputIndex)
                (pfromData pstep04Args'fraudProofMintRedeemerIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                (pto $ pto $ pfromData ptxInfo'redeemers)
                $ \_ownScriptHash threadName _prover inputState -> P.do
                    state <- plet $ pexpectStateAs @PStep04State inputState
                    pexpecting (pfraudCategoryIsValueNotPreservedV1 # pfromData threadName) $
                        pexpecting (pvalueNotPreservedFaultIsEstablishedV1 # state) (pconstant True)
