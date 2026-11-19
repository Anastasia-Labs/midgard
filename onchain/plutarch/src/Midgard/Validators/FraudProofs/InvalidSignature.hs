{- |
Module      : Midgard.Validators.FraudProofs.InvalidSignature
Description : Plutarch port of @validators/fraud-proofs/invalid-signature/step-0{1,2}.ak@.

The invalid-signature fraud proof (spec §5.1.1): a committed transaction carrying
an address witness whose signature does not verify over the transaction id.

Two validators. Step-01 binds the transaction and writes the §2.5 anchor;
step-02 opens field 7, reaches one witness, and convicts when the signature
fails.

=== The anchor's second half is what makes the conviction sound

@bad_tx_id@ alone anchors fields 0–5 and nothing else: §3's id preimage is the
body, so the compact structure's trailing @witness_set_hash@ is outside it and a
step-02 redeemer could supply any value there. Step-01 read the real one off the
structure the block's counted @transactions_root@ committed, and it is carried.

The forgery it stops is the mirror of @missing-signature@'s. There an invented
witness set makes an absence true of every transaction; here a /fabricated/
witness set — one holding a key and a signature that genuinely do not match —
would make an "invalid signature" fault provable against a signature the
transaction never carried. Both directions of the §2.5 absence rules need the
same 32 bytes.

=== One witness, reached by arithmetic

§5.3 fixes the address-witness item at 101 bytes, so reaching witness @n@ is a
multiplication and a slice rather than a walk. This family therefore has no fold:
the claim is about /one/ named item, and the whole point of naming it is not to
have to see the others.
-}
module Midgard.Validators.FraudProofs.InvalidSignature (
  invalidSignatureStep01Validator,
  invalidSignatureStep02Validator,
) where

import Plutarch.Builtin.Crypto (pverifyEd25519Signature)
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  paddressWitnessesFieldIndex,
  popenedFieldView,
 )
import Midgard.FraudProofs.InvalidSignature
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddressWitness (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemAt, pfieldItemCount)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstateIsAbsent,
  pstep,
 )
import Plutarch.Unsafe (punsafeCoerce)

{- | Aiken @validators/fraud-proofs/invalid-signature/step-01.ak@.

Binds the transaction and writes both halves of the §2.5 anchor.
-}
invalidSignatureStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
invalidSignatureStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep01Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep01Args source <- pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pmatch (pfromData source) $ \case
            PAcceptedSource carriage ->
              ppassNativeTxToNextStepCarried
                computationThreadTokenPolicyId
                hubOracle
                datum
                (pfromData carriage)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'referenceInputs)
                (pfromData ptxInfo'outputs)
                (pto (pto (pfromData ptxInfo'redeemers)))
                $ \_ _ _ inputState outputScriptHash outputStateData _header _ verified ->
                  pstateIsAbsent inputState
                    #&& outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== expectedState (Subject.pbindAcceptedSubject # verified) verified
            PForcedSource inputIndex outputIndex header membership direction ->
              pcontinue
                computationThreadTokenPolicyId
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ threadName _ inputState outputScriptHash outputStateData -> P.do
                  subject <-
                    plet $
                      Subject.pbindForcedSubjectToThread
                        # pto (pfromData threadName)
                        # pfromData header
                        # pfromData membership
                        # pfromData direction
                  PRootMembershipProof {prootMembership'value} <- pmatch $ pfromData membership
                  PForcedInclusionTxV1 {pforcedTx'txId, pforcedTx'source} <-
                    pmatch $
                      pfromData (punsafeCoerce prootMembership'value)
                  PNativeTxProofSourceV1 {..} <- pmatch $ pfromData pforcedTx'source
                  PPair verified _ <-
                    pmatch $
                      pverifyNativeTxProofSourceV1
                        # pfromData pforcedTx'txId
                        # pfromData pnativeSource'compactCbor
                        # pfromData pnativeSource'witnessSetCompactCbor
                        # pfromData pnativeSource'fieldPreimageLengthsCbor
                  PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch verified
                  Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch subject
                  pstateIsAbsent inputState
                    #&& pverified'txId
                    #== pfromData psubject'transactionId
                    #&& outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== expectedState subject verified
  where
    expectedState subject verified =
      pmatch verified $ \PVerifiedMidgardNativeTxCompact {pverified'txCompact} ->
        pmatch pverified'txCompact $ \PNativeTxCompact {pcompact'witnessSetHash} ->
          pforgetData $ pdata $ pcon $ PStep02State (pdata subject) (pdata pcompact'witnessSetHash)

{- | Aiken @validators/fraud-proofs/invalid-signature/step-02.ak@.

The conviction: the named witness's signature over the transaction id must not
verify.

The message signed is the transaction id itself, which is why this step needs no
further evidence: the id is thread state and the witness comes out of an
authenticated field, so both sides of the verification are pinned before it runs.
-}
invalidSignatureStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
invalidSignatureStep02Validator =
  plam $
    \computationThreadTokenPolicyId
     fraudProofTokenPolicyId
     fraudProofTokenAddress
     fieldPreimageCertificatePolicyId
     ctx ->
        pstep ctx $ \datum redeemer ownOutRef txInfo ->
          pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
            \args -> P.do
              PStep02Args
                { pstep02Args'inputIndex
                , pstep02Args'outputIndex
                , pstep02Args'addrTxWitsOpening
                , pstep02Args'badAddrTxWitIndex
                , pstep02Args'fraudProofMintRedeemerIndex
                } <-
                pmatch args
              PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
                pmatch txInfo
              referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
              pfinalize
                computationThreadTokenPolicyId
                fraudProofTokenPolicyId
                fraudProofTokenAddress
                (pexpectDatum datum)
                (pfromData pstep02Args'inputIndex)
                (pfromData pstep02Args'outputIndex)
                (pfromData pstep02Args'fraudProofMintRedeemerIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                (pto (pto (pfromData ptxInfo'redeemers)))
                $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                  PStep02State {pstep02State'subject, pstep02State'badTxWitnessSetHash} <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  subject <- plet $ pfromData pstep02State'subject
                  Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch subject
                  addrTxWitsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'addrTxWitsOpening
                        # pcon
                          ( PWitnessAnchor
                              { pwitnessAnchor'txId = psubject'transactionId
                              , pwitnessAnchor'witnessSetHash = pstep02State'badTxWitnessSetHash
                              }
                          )
                        # paddressWitnessesFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  count <- plet $ pfieldItemCount # addrTxWitsView
                  index <- plet $ pfromData pstep02Args'badAddrTxWitIndex
                  inRange <- plet $ index #>= 0 #&& index #< count
                  let signatureValid =
                        pif
                          inRange
                          ( pmatch (pdecodeMidgardAddressWitnessCbor # (pfieldItemAt # addrTxWitsView # index)) $
                              \PMidgardAddressWitness {paddressWitness'verificationKey, paddressWitness'signature} ->
                                pverifyEd25519Signature
                                  # pfromData paddressWitness'verificationKey
                                  # pfromData psubject'transactionId
                                  # pfromData paddressWitness'signature
                          )
                          (pconstant False)
                  pterminalContradiction # subject # index # inRange # signatureValid
