{- |
Module      : Midgard.Validators.FraudProofs.MinFee
Description : Plutarch port of @validators/fraud-proofs/min-fee/step-0{1,2}.ak@.

The min-fee fraud proof (spec §5.1.1): a committed transaction whose fee is below
the protocol minimum.

Two validators. Step-01 binds the transaction, id, inline fee and header fee
schedule into thread state. Step-02 authenticates all nine field preimages,
derives the full canonical transaction size, and applies the same shared
@min_fee_a * size + min_fee_b@ rule as the validation machine.
-}
module Midgard.Validators.FraudProofs.MinFee (
  minFeeStep01Validator,
  minFeeStep02Validator,
  pnativeTxCompactToData,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
 )
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  PNativeTxOpeningV1 (..),
  paddressWitnessesFieldIndex,
  panchoredFieldView,
  panchoredNativeTx,
  pmintFieldIndex,
  poutputsFieldIndex,
  predeemersFieldIndex,
  preferenceInputsFieldIndex,
  prequiredObserversFieldIndex,
  prequiredSignersFieldIndex,
  pscriptWitnessesFieldIndex,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.MinFee
import Midgard.FraudProofs.NativeTx.Compact (
  pminFeeLovelaceV1,
  pnativeTxCanonicalSizeV1,
  pverifyNativeTxCompactCborV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pauthenticatedCommittedPreimage, pfieldTotalLength)
import Midgard.RejectionReason (PRejectionReasonV1 (PFeeBelowMinimum))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstep,
 )
import Plutarch.Unsafe (punsafeCoerce)

{- | The @Data@ encoding of a 'PNativeTxCompact', built by hand.

@Constr 0 [Constr 0 [twelve body fields], B witness_set_hash, I validity_code]@ —
the shape an Aiken @NativeTxCompact@ serialises to, and therefore the shape an SDK
building this family's step-01 output datum must produce.

The port keeps 'PNativeTxCompact' Scott-encoded because it is produced by the
codec and consumed by the accessors inside one script everywhere else, and
Data-encoding a twelve-field record would cost execution units across every native
family to serve this one call site. This function is the whole price of that
choice: one encoder, here, rather than a heavier representation everywhere.
-}
pnativeTxCompactToData :: forall (s :: S). Term s (PNativeTxCompact :--> PData)
pnativeTxCompactToData = phoistAcyclic $
  plam $ \compact -> P.do
    PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <-
      pmatch compact
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash
      , pbodyCompact'referenceInputsHash
      , pbodyCompact'outputsHash
      , pbodyCompact'fee
      , pbodyCompact'validityIntervalStart
      , pbodyCompact'validityIntervalEnd
      , pbodyCompact'requiredObserversHash
      , pbodyCompact'requiredSignersHash
      , pbodyCompact'mintHash
      , pbodyCompact'scriptIntegrityHash
      , pbodyCompact'auxiliaryDataHash
      , pbodyCompact'networkId
      } <-
      pmatch pcompact'body
    body <-
      plet $
        pconstrBuiltin
          # 0
          #$ pfields
            [ pforgetData (pdata pbodyCompact'spendInputsHash)
            , pforgetData (pdata pbodyCompact'referenceInputsHash)
            , pforgetData (pdata pbodyCompact'outputsHash)
            , pforgetData (pdata pbodyCompact'fee)
            , pforgetData (pdata pbodyCompact'validityIntervalStart)
            , pforgetData (pdata pbodyCompact'validityIntervalEnd)
            , pforgetData (pdata pbodyCompact'requiredObserversHash)
            , pforgetData (pdata pbodyCompact'requiredSignersHash)
            , pforgetData (pdata pbodyCompact'mintHash)
            , pforgetData (pdata pbodyCompact'scriptIntegrityHash)
            , pforgetData (pdata pbodyCompact'auxiliaryDataHash)
            , pforgetData (pdata pbodyCompact'networkId)
            ]
    pforgetData $
      pconstrBuiltin
        # 0
        #$ pfields
          [ pforgetData body
          , pforgetData (pdata pcompact'witnessSetHash)
          , pforgetData (pdata pcompact'validityCode)
          ]
  where
    pfields = foldr (\d acc -> pcons # d # acc) (pcon PNil)

{- | Decode the exact Aiken @NativeTxCompact@ @Data@ shape written into thread
state back into the port's Scott representation.
-}
pnativeTxCompactFromData :: forall (s :: S). Term s (PData :--> PNativeTxCompact)
pnativeTxCompactFromData = phoistAcyclic $
  plam $ \compactData ->
    pmatch (pasConstr # compactData) $ \(PBuiltinPair compactTag compactFields) ->
      pif
        (compactTag #== 0 #&& plength # compactFields #== 3)
        ( pmatch (pasConstr # (pelemAt # 0 # compactFields)) $ \(PBuiltinPair bodyTag bodyFields) ->
            pif
              (bodyTag #== 0 #&& plength # bodyFields #== 12)
              ( pcon
                  ( PNativeTxCompact
                      { pcompact'body =
                          pcon
                            ( PNativeTxBodyCompact
                                { pbodyCompact'spendInputsHash = pasByteStr # (pelemAt # 0 # bodyFields)
                                , pbodyCompact'referenceInputsHash = pasByteStr # (pelemAt # 1 # bodyFields)
                                , pbodyCompact'outputsHash = pasByteStr # (pelemAt # 2 # bodyFields)
                                , pbodyCompact'fee = pasInt # (pelemAt # 3 # bodyFields)
                                , pbodyCompact'validityIntervalStart = pasInt # (pelemAt # 4 # bodyFields)
                                , pbodyCompact'validityIntervalEnd = pasInt # (pelemAt # 5 # bodyFields)
                                , pbodyCompact'requiredObserversHash = pasByteStr # (pelemAt # 6 # bodyFields)
                                , pbodyCompact'requiredSignersHash = pasByteStr # (pelemAt # 7 # bodyFields)
                                , pbodyCompact'mintHash = pasByteStr # (pelemAt # 8 # bodyFields)
                                , pbodyCompact'scriptIntegrityHash = pasByteStr # (pelemAt # 9 # bodyFields)
                                , pbodyCompact'auxiliaryDataHash = pasByteStr # (pelemAt # 10 # bodyFields)
                                , pbodyCompact'networkId = pasInt # (pelemAt # 11 # bodyFields)
                                }
                            )
                      , pcompact'witnessSetHash = pasByteStr # (pelemAt # 1 # compactFields)
                      , pcompact'validityCode = pasInt # (pelemAt # 2 # compactFields)
                      }
                  )
              )
              perror
        )
        perror

{- | Aiken @validators/fraud-proofs/min-fee/step-01.ak@.

Binds the transaction and forwards its verified compact form and inline body fee.
-}
minFeeStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
minFeeStep01Validator = plam $
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
                $ \_ _ _ _inputState outputScriptHash outputStateData header _ verified ->
                  outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== expectedState (Subject.pbindAcceptedSubject # verified) (pfromData header) verified
            PForcedSource inputIndex outputIndex header membership direction ->
              pcontinue
                computationThreadTokenPolicyId
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ threadName _ _inputState outputScriptHash outputStateData -> P.do
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
                  verified <-
                    plet $
                      pverifyNativeTxCompactCborV1
                        # pfromData pforcedTx'txId
                        # pfromData pnativeSource'compactCbor
                  _ <- plet $ Subject.pbindExactRejectionReason # subject # pcon PFeeBelowMinimum
                  PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch verified
                  Subject.PVerdictSubject {Subject.psubject'transactionId, Subject.psubject'direction} <- pmatch subject
                  pfromData psubject'direction
                    #== 1
                    #&& pverified'txId
                    #== pfromData psubject'transactionId
                    #&& outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== expectedState subject (pfromData header) verified
  where
    expectedState subject header verified =
      pmatch header $ \PHeaderV1 {pheader'minFeeA, pheader'minFeeB} ->
        pmatch verified $ \PVerifiedMidgardNativeTxCompact {pverified'txCompact, pverified'txId} ->
          pmatch pverified'txCompact $ \PNativeTxCompact {pcompact'body} ->
            pmatch pcompact'body $ \PNativeTxBodyCompact {pbodyCompact'fee} ->
              pforgetData $
                pdata $
                  pcon $
                    PStep02State
                      (pdata subject)
                      (pnativeTxCompactToData # pverified'txCompact)
                      (pdata pbodyCompact'fee)
                      (pdata pverified'txId)
                      pheader'minFeeA
                      pheader'minFeeB

{- | Aiken @validators/fraud-proofs/min-fee/step-02.ak@.

The declared fee must be below the header schedule applied to the canonical
full-transaction size derived through the authenticated field door.
-}
minFeeStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field-preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
minFeeStep02Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId fieldPreimageCertificatePolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep02Args
            { pstep02Args'inputIndex
            , pstep02Args'outputIndex
            , pstep02Args'fraudProofMintRedeemerIndex
            , pstep02Args'nativeTxCompactCbor
            , pstep02Args'witnessSet
            , pstep02Args'fieldCarriages
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
              PStep02State
                { pstep02State'subject
                , pstep02State'badTx
                , pstep02State'badTxBodyFee
                , pstep02State'badTxId
                , pstep02State'minFeeA
                , pstep02State'minFeeB
                } <-
                pmatch (pexpectStateAs @PStep02State mInputStateData)
              subject <- plet $ pfromData pstep02State'subject
              Subject.PVerdictSubject {Subject.psubject'direction, Subject.psubject'transactionId} <- pmatch subject
              _ <-
                plet $
                  pif
                    (Subject.psubjectIsCanonical # subject #&& psubject'transactionId #== pstep02State'badTxId)
                    ( pif
                        (pfromData psubject'direction #== 1)
                        (plet (Subject.pbindExactRejectionReason # subject # pcon PFeeBelowMinimum) $ \_ -> pcon PUnit)
                        (pcon PUnit)
                    )
                    perror
              compact <- plet $ pnativeTxCompactFromData # pstep02State'badTx
              PNativeTxCompact {pcompact'witnessSetHash} <- pmatch compact
              carriages <- plet $ pfromData pstep02Args'fieldCarriages
              pexpecting (plength # carriages #== 9) $ P.do
                bodyHandle <-
                  plet $
                    panchoredNativeTx
                      # pcon (PBodyTxOpening $ pfromData pstep02Args'nativeTxCompactCbor)
                      # pcon (PBodyAnchor pstep02State'badTxId)
                witnessHandle <-
                  plet $
                    panchoredNativeTx
                      # pcon
                        ( PWitnessTxOpening
                            (pfromData pstep02Args'nativeTxCompactCbor)
                            (pfromData pstep02Args'witnessSet)
                        )
                      # pcon (PWitnessAnchor pstep02State'badTxId (pdata pcompact'witnessSetHash))
                verified <-
                  plet $
                    pverifyNativeTxCompactCborV1
                      # pfromData pstep02State'badTxId
                      # pfromData pstep02Args'nativeTxCompactCbor
                PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
                PNativeTxCompact {pcompact'witnessSetHash = verifiedWitnessHash} <- pmatch pverified'txCompact
                _ <- plet $ pif (verifiedWitnessHash #== pcompact'witnessSetHash) (pcon PUnit) perror
                let fieldLength handle index carriageIndex =
                      pif
                        (pfromData psubject'direction #== 1)
                        ( plengthBS
                            # ( pauthenticatedCommittedPreimage
                                  # verified
                                  # pfromData pstep02Args'witnessSet
                                  # index
                                  # pfromData (pelemAt # carriageIndex # carriages)
                                  # pfromData ptxInfo'referenceInputs
                                  # fieldPreimageCertificatePolicyId
                              )
                        )
                        ( pfieldTotalLength
                            # ( panchoredFieldView
                                  # handle
                                  # index
                                  # pfromData (pelemAt # carriageIndex # carriages)
                                  # pfromData ptxInfo'referenceInputs
                                  # fieldPreimageCertificatePolicyId
                              )
                        )
                lengths <-
                  plet $
                    pcon
                      ( PNativeTxFieldPreimageLengthsV1
                          { plengths'spendInputs = fieldLength bodyHandle pspendInputsFieldIndex 0
                          , plengths'referenceInputs = fieldLength bodyHandle preferenceInputsFieldIndex 1
                          , plengths'outputs = fieldLength bodyHandle poutputsFieldIndex 2
                          , plengths'requiredObservers = fieldLength bodyHandle prequiredObserversFieldIndex 3
                          , plengths'requiredSigners = fieldLength bodyHandle prequiredSignersFieldIndex 4
                          , plengths'mint = fieldLength bodyHandle pmintFieldIndex 5
                          , plengths'addressWitnesses = fieldLength witnessHandle paddressWitnessesFieldIndex 7
                          , plengths'scriptWitnesses = fieldLength witnessHandle pscriptWitnessesFieldIndex 6
                          , plengths'redeemers = fieldLength witnessHandle predeemersFieldIndex 8
                          }
                      )
                pexpecting
                  ( Subject.pterminalContradiction
                      # subject
                      # ( pfromData pstep02State'badTxBodyFee
                            #< pminFeeLovelaceV1
                            # pfromData pstep02State'minFeeA
                            # pfromData pstep02State'minFeeB
                            # (pnativeTxCanonicalSizeV1 # compact # lengths)
                        )
                  )
                  (pconstant True)
