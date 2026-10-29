module Midgard.Validators.FraudProofs.WitnessScriptDecoding (
  witnessScriptDecodingStep01Validator,
  witnessScriptDecodingStep02Validator,
  witnessScriptDecodingStep03Validator,
  witnessScriptDecodingStep04Validator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (PWitnessAnchor),
  popenedFieldView,
  pscriptWitnessesFieldIndex,
 )
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.WitnessScriptDecoding
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeScriptScan qualified as Scan
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

witnessScriptDecodingStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
witnessScriptDecodingStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{pstep01Args'source, pstep01Args'scriptIndex} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState _ _ verified -> P.do
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          let expected = pbindSubjectV1 # (Subject.pbindAcceptedSubject # verified) # pcompact'witnessSetHash # pfromData pstep01Args'scriptIndex
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
          subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
          PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
          PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
          PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
          PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          let expected = pbindSubjectV1 # subject # pcompact'witnessSetHash # pfromData pstep01Args'scriptIndex
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

witnessScriptDecodingStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
witnessScriptDecodingStep02Validator = plam $ \step03Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      bound@PBoundWitnessScriptV1{pboundWitness'subject, pboundWitness'witnessSetHash, pboundWitness'scriptIndex} <- pmatch $ pexpectStateAs @PBoundWitnessScriptV1 inputState
      Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundWitness'subject
      view <- plet $ popenedFieldView # pfromData pstep02Args'opening # pcon (PWitnessAnchor psubject'transactionId pboundWitness'witnessSetHash) # pscriptWitnessesFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
      let expected = pauthenticateItemV1 # pcon bound # (pfieldItemAt # view # pfromData pboundWitness'scriptIndex) # pto (pfromData step03Hash)
      outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

witnessScriptDecodingStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
witnessScriptDecodingStep03Validator = plam $ \step04Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep03Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PWitnessScriptScanStateV1 inputState
      PWitnessScriptScanStateV1{..} <- pmatch state
      pexpecting (pstateIsAuthenticV1 # state #&& pfromData pwitnessScan'nextExpectedScriptHash #== pto (pfromData ownHash)) $
        let closeWith resultClass =
              pexpecting (outputHash #== step04Hash) $
                pclosedStateV1 # state # resultClass # pto (pfromData step04Hash)
            expected =
              pif
                (pfromData pwitnessScan'resultClass #/= presultPending)
                ( pexpecting
                    ( pfromData pstep03Args'controlCbor
                        #== pconstant ""
                        #&& pfromData pstep03Args'chunkProof
                        #== pcon PDNothing
                        #&& pfromData pstep03Args'nextChunkProof
                        #== pcon PDNothing
                        #&& pnull
                        # pfromData pstep03Args'frames
                        #&& pfromData pstep03Args'stepBudget
                        #== 0
                    )
                    (closeWith $ pfromData pwitnessScan'resultClass)
                )
                ( pexpecting (pfromData pstep03Args'stepBudget #> 0 #&& pfromData pstep03Args'controlCbor #== pfromData pwitnessScan'controlCbor) $
                    let control = Scan.pdecodeStructureControlV1 # pfromData pstep03Args'controlCbor
                        window = pmatch (pfromData pstep03Args'chunkProof) $ \case
                          PDNothing -> pexpecting (pfromData pstep03Args'nextChunkProof #== pcon PDNothing) (pcon PNothing)
                          PDJust proof -> pcon $ PJust $ pauthenticatedWindowV1 # state # control # pfromData proof # pfromData pstep03Args'nextChunkProof
                     in pmatch (Engine.pbudgetedScanV1 # control # window # pfromData pstep03Args'frames # pfromData pstep03Args'stepBudget) $ \case
                          Engine.PScanRefusedV1 refusalClass -> closeWith $ pmappedRefusalClassV1 # pfromData refusalClass
                          Engine.PScanAdvancedV1 nextControl ->
                            let next = pfromData nextControl
                             in pif
                                  (Scan.pstructureTerminalIsExactV1 # next)
                                  (closeWith presultNoFault)
                                  ( pexpecting (outputHash #== ownHash) $
                                      let advanced = padvancedStateV1 # state # next # pto (pfromData ownHash)
                                       in pexpecting
                                            ( pmatch advanced $ \PWitnessScriptScanStateV1{pwitnessScan'checkpointHash = nextCheckpointHash} ->
                                                nextCheckpointHash #/= pwitnessScan'checkpointHash
                                            )
                                            advanced
                                  )
                )
         in outputState #== pforgetData (pdata expected)

witnessScriptDecodingStep04Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
witnessScriptDecodingStep04Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) (pfromData pstep04Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState -> pterminalContradictionV1 # pexpectStateAs @PWitnessScriptScanStateV1 inputState
