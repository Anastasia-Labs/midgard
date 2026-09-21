module Midgard.Validators.FraudProofs.ProtectedOutputSignerMissing (
  protectedOutputSignerMissingStep01Validator,
  protectedOutputSignerMissingStep02Validator,
  protectedOutputSignerMissingStep03Validator,
  protectedOutputSignerMissingStep04Validator,
  protectedOutputSignerMissingStep05Validator,
) where

import Plutarch.Builtin.Crypto (pblake2b_224, pverifyEd25519Signature)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (..), popenedFieldView, popenedFieldWalk, presumeOpenedFieldWalk)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor, pdecodeMidgardTxOutputCbor)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardCredential (..),
  PMidgardTxOutput (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ProtectedOutputSignerMissing
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemAt, pfieldItemCount)
import Midgard.NativeTxMachineWalk (pfieldWalkCheckpointHash, pwalkFold, pwalkIsComplete, pwalkRemaining)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

protectedOutputSignerMissingStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
protectedOutputSignerMissingStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{pstep01Args'source, pstep01Args'outputIndex} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState _ _ verified -> P.do
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          bound <- plet $ pbindOutputV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pstep01Args'outputIndex
          let expected = pcon $ PStep02State (pdata bound) (pdata pcompact'witnessSetHash)
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
          bound <- plet $ pbindOutputV1 # subject # pfromData pstep01Args'outputIndex
          let expected = pcon $ PStep02State (pdata bound) (pdata pcompact'witnessSetHash)
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

protectedOutputSignerMissingStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
protectedOutputSignerMissingStep02Validator = plam $ \step03Hash step05Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      PStep02State{pstep02State'bound, pstep02State'witnessSetHash} <- pmatch $ pexpectStateAs @PStep02State inputState
      bound@PBoundOutputV1{pboundOutput'subject, pboundOutput'outputIndex} <- pmatch $ pfromData pstep02State'bound
      Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundOutput'subject
      view <- plet $ popenedFieldView # pfromData pstep02Args'opening # pcon (PBodyAnchor psubject'transactionId) # poutputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
      pif
        (pfromData pstep02Args'coordinateOutOfRange)
        ( let expected = pdirectVerdictV1 # pcon bound
           in pfromData pboundOutput'outputIndex
                #>= pfieldItemCount
                # view
                #&& outputHash
                #== step05Hash
                #&& outputState
                #== pforgetData (pdata expected)
        )
        ( pmatch (pdecodeMidgardTxOutputCbor # (pfieldItemAt # view # pfromData pboundOutput'outputIndex)) $ \PMidgardTxOutput{ptxOutput'address} ->
            pmatch (pfromData ptxOutput'address) $ \PMidgardAddress{paddress'protected, paddress'paymentCredential} ->
              pif
                (pfromData paddress'protected)
                ( pmatch (pfromData paddress'paymentCredential) $ \case
                    PMidgardPubKeyCredential paymentCredential ->
                      let expected =
                            pcon $
                              PProtectedCredentialV1
                                pboundOutput'subject
                                psubject'transactionId
                                pstep02State'witnessSetHash
                                pboundOutput'outputIndex
                                paymentCredential
                       in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)
                    PMidgardScriptCredential _ ->
                      let expected = pdirectVerdictV1 # pcon bound
                       in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
                )
                ( let expected = pdirectVerdictV1 # pcon bound
                   in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
                )
        )

protectedOutputSignerMissingStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
protectedOutputSignerMissingStep03Validator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep03Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      protected@PProtectedCredentialV1{pprotected'transactionId, pprotected'witnessSetHash} <- pmatch $ pexpectStateAs @PProtectedCredentialV1 inputState
      PPair _ checkpoint <- pmatch $ popenedFieldWalk # pfromData pstep03Args'opening # pcon (PWitnessAnchor pprotected'transactionId pprotected'witnessSetHash) # paddressWitnessesFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
      let expected = pcon $ PWitnessScanV1 (pdata $ pcon protected) (pdata $ pfieldWalkCheckpointHash # checkpoint) (pdata $ pconstant False)
      outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)

protectedOutputSignerMissingStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
protectedOutputSignerMissingStep04Validator = plam $ \step05Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
      PWitnessScanV1{pwitnessScan'protected, pwitnessScan'checkpointHash, pwitnessScan'signerPresent} <- pmatch $ pexpectStateAs @PWitnessScanV1 inputState
      protected@PProtectedCredentialV1{pprotected'subject, pprotected'transactionId, pprotected'witnessSetHash, pprotected'paymentCredential} <- pmatch $ pfromData pwitnessScan'protected
      PPair view checkpoint <- pmatch $ presumeOpenedFieldWalk # pfromData pstep04Args'opening # pcon (PWitnessAnchor pprotected'transactionId pprotected'witnessSetHash) # paddressWitnessesFieldIndex # pfromData pwitnessScan'checkpointHash # pfromData pstep04Args'checkpointCbor # pfromData ptxInfo'referenceInputs # certificatePolicy
      let remaining = pwalkRemaining # checkpoint
          batch = pif (remaining #< pwitnessScanBatchSize) remaining pwitnessScanBatchSize
      PPair found next <-
        pmatch $
          pwalkFold @PBool
            # view
            # checkpoint
            # batch
            # pfromData pwitnessScan'signerPresent
            # ( plam $ \present _ item ->
                  pmatch (pdecodeMidgardAddressWitnessCbor # item) $ \PMidgardAddressWitness{paddressWitness'verificationKey, paddressWitness'signature} ->
                    present
                      #|| ( pblake2b_224
                              # pfromData paddressWitness'verificationKey
                              #== pfromData pprotected'paymentCredential
                              #&& pverifyEd25519Signature
                              # pfromData paddressWitness'verificationKey
                              # pfromData pprotected'transactionId
                              # pfromData paddressWitness'signature
                          )
              )
      pif
        (pwalkIsComplete # next)
        ( let expected = pcon $ PVerdictV1 pprotected'subject (pdata $ pconstant True) (pdata found)
           in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
        )
        ( let expected = pcon $ PWitnessScanV1 (pdata $ pcon protected) (pdata $ pfieldWalkCheckpointHash # next) (pdata found)
           in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
        )

protectedOutputSignerMissingStep05Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
protectedOutputSignerMissingStep05Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) (pfromData pstep05Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState -> pterminalV1 # pexpectStateAs @PVerdictV1 inputState
