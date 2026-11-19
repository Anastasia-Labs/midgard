module Midgard.Validators.FraudProofs.SpendInputSignerMissing (
  spendInputSignerMissingStep01Validator,
  spendInputSignerMissingStep02Validator,
  spendInputSignerMissingStep03Validator,
  spendInputSignerMissingStep04Validator,
  spendInputSignerMissingStep05Validator,
) where

import Plutarch.Builtin.Crypto (pblake2b_224, pverifyEd25519Signature)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried, pverifyMembershipCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (..), popenedFieldView, popenedFieldWalk, presumeOpenedFieldWalk, pspendInputsFieldIndex)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardCredential (..),
  PMidgardTxInput (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.SpendInputSignerMissing
import Midgard.FraudProofs.TransitionTrace.Proof qualified as Proof
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pdecodeLedgerOutputCommitment)
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldCountRequiresCertification, pfieldItemCount)
import Midgard.NativeTxMachineWalk (pfieldWalkCheckpointHash, pspendInputAt, pwalkFold, pwalkIsComplete, pwalkRemaining)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

spendInputSignerMissingStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
spendInputSignerMissingStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{pstep01Args'source, pstep01Args'inputIndex} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
          PHeaderV1{pheader'prevUtxosRoot} <- pmatch $ pfromData header
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          let expected = pbindSpendInputV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pstep01Args'inputIndex # pfromData pheader'prevUtxosRoot # pcompact'witnessSetHash
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
          subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
          PHeaderV1{pheader'prevUtxosRoot} <- pmatch $ pfromData header
          PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
          PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
          PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
          PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          let expected = pbindSpendInputV1 # subject # pfromData pstep01Args'inputIndex # pfromData pheader'prevUtxosRoot # pcompact'witnessSetHash
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

spendInputSignerMissingStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
spendInputSignerMissingStep02Validator = plam $ \step03Hash step05Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      bound@PBoundSpendInputV1{..} <- pmatch $ pexpectStateAs @PBoundSpendInputV1 inputState
      Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundSpendInput'subject
      view <- plet $ popenedFieldView # pfromData pstep02Args'spendInputsOpening # pcon (PBodyAnchor psubject'transactionId) # pspendInputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
      pif
        (pfromData pboundSpendInput'inputIndex #>= pfieldItemCount # view)
        ( let expected = pdirectVerdictV1 # pcon bound
           in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
        )
        ( P.do
            selected <- plet $ pspendInputAt # view # pfromData pboundSpendInput'inputIndex
            let key = Proof.pledgerOutrefKey (pforgetData $ pdata selected)
            descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData pstep02Args'descriptorCbor
            PLedgerOutputCommitmentV1{poutputCommitment'outputIndex, poutputCommitment'address} <- pmatch descriptor
            pmatch selected $ \PMidgardTxInput{ptxInput'outputIndex} ->
              pif
                ( pverifyMembershipCarried (pfromData pstep02Args'membership) (pfromData pboundSpendInput'priorRoot) key (pfromData pstep02Args'descriptorCbor) (pfromData ptxInfo'referenceInputs) (pto $ pto $ pfromData ptxInfo'redeemers)
                    #&& pfromData poutputCommitment'outputIndex
                    #== pfromData ptxInput'outputIndex
                )
                ( pmatch (pdecodeCanonicalAddressBytes # pfromData poutputCommitment'address) $ \case
                    PNothing -> perror
                    PJust address -> pmatch address $ \PMidgardAddress{paddress'paymentCredential} ->
                      pmatch (pfromData paddress'paymentCredential) $ \case
                        PMidgardPubKeyCredential paymentCredential ->
                          let expected = pauthenticateCredentialV1 # pcon bound # pfromData paymentCredential
                           in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)
                        PMidgardScriptCredential _ ->
                          let expected = pdirectVerdictV1 # pcon bound
                           in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
                )
                perror
        )

spendInputSignerMissingStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
spendInputSignerMissingStep03Validator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep03Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      authenticated@PAuthenticatedCredentialV1{pauthenticatedCredential'transactionId, pauthenticatedCredential'witnessSetHash} <- pmatch $ pexpectStateAs @PAuthenticatedCredentialV1 inputState
      PPair view checkpoint <- pmatch $ popenedFieldWalk # pfromData pstep03Args'witnessesOpening # pcon (PWitnessAnchor pauthenticatedCredential'transactionId pauthenticatedCredential'witnessSetHash) # 7 # pfromData ptxInfo'referenceInputs # certificatePolicy
      let expected = pcon $ PWitnessScanV1 (pdata $ pcon authenticated) (pdata $ pfieldWalkCheckpointHash # checkpoint)
      pnot # (pfieldCountRequiresCertification # view) #&& outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)

spendInputSignerMissingStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
spendInputSignerMissingStep04Validator = plam $ \step05Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
      PWitnessScanV1{..} <- pmatch $ pexpectStateAs @PWitnessScanV1 inputState
      authenticated@PAuthenticatedCredentialV1{..} <- pmatch $ pfromData pwitnessScan'authenticated
      PPair view checkpoint <- pmatch $ presumeOpenedFieldWalk # pfromData pstep04Args'witnessesOpening # pcon (PWitnessAnchor pauthenticatedCredential'transactionId pauthenticatedCredential'witnessSetHash) # 7 # pfromData pwitnessScan'checkpointHash # pfromData pstep04Args'checkpointCbor # pfromData ptxInfo'referenceInputs # certificatePolicy
      let remaining = pwalkRemaining # checkpoint
          batch = pif (remaining #< pwitnessScanBatchSize) remaining pwitnessScanBatchSize
      PPair found next <-
        pmatch $
          pwalkFold @PBool
            # view
            # checkpoint
            # batch
            # pconstant False
            # ( plam $ \present _ item ->
                  pmatch (pdecodeMidgardAddressWitnessCbor # item) $ \PMidgardAddressWitness{paddressWitness'verificationKey, paddressWitness'signature} ->
                    present
                      #|| ( pverifyEd25519Signature
                              # pfromData paddressWitness'verificationKey
                              # pfromData pauthenticatedCredential'transactionId
                              # pfromData paddressWitness'signature
                              #&& pblake2b_224
                              # pfromData paddressWitness'verificationKey
                              #== pfromData pauthenticatedCredential'paymentCredential
                          )
              )
      pif
        (found #|| pwalkIsComplete # next)
        ( let expected = pscanVerdictV1 # pfromData pauthenticatedCredential'subject # (pnot # found)
           in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
        )
        ( let expected = pcon $ PWitnessScanV1 (pdata $ pcon authenticated) (pdata $ pfieldWalkCheckpointHash # next)
           in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
        )

spendInputSignerMissingStep05Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
spendInputSignerMissingStep05Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) (pfromData pstep05Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
      pterminalV1 # pexpectStateAs @PVerdictV1 inputState
