{- |
Module      : Midgard.Validators.FraudProofs.MissingSignature
Description : Plutarch port of @validators/fraud-proofs/missing-signature/step-0{1,2,3,4}.ak@.

The missing-signature fraud proof (spec §5.1.1): a committed transaction that
names a required signer and carries no address witness for it.

Four validators:

1. bind the transaction and write the §2.5 anchor — /both/ halves;
2. read the required signer's hash out of field 4;
3. take a verification key and check it hashes to that;
4. walk field 7 and require the key to be absent from it.

=== Why the witness-set hash is thread state and not a redeemer argument

This is the whole security of step-04, and it is not visible from step-04 alone.
§3's transaction-id preimage is the __body__: the compact structure's trailing
@witness_set_hash@ sits outside it, so bytes carrying the genuine body and an
invented tail re-derive to the very same @verified_tx_id@. A door that checked
only the id would therefore accept the /empty/ witness set against any
transaction — and an empty field 7 makes "the required signature is absent" true
of every transaction ever committed, which is a slashing proof against every
honest operator at once.

Step-01 is the only step that can close that, because it is the only one holding
the compact structure the block's counted @transactions_root@ committed. It reads
the real @witness_set_hash@ there and the value is carried, unchanged, through
every state of the family.

=== The absence is a bounded fold, and it must complete

Step-04 is one of the two rules in the whole machine that genuinely has to see
every item — "no witness carries this key" is only true of a walk that reached the
end. It consumes fixed 32-item batches, commits each intermediate checkpoint in
thread state, and permits the conviction only for a terminal suffix no longer
than one batch. No transaction therefore performs an unbounded witness scan.

=== Three steps, three different shapes of evidence

Field 4 holds raw 28-byte hashes and field 7 holds 32-byte keys inside 101-byte
witnesses, so the two collections cannot be compared directly. Step-03 is the
bridge and its only guard is @blake2b_224(vkey) == hash@ — which is why the key
arrives in a redeemer rather than being read out of anything: the thread has only
the hash, and hashes do not invert.
-}
module Midgard.Validators.FraudProofs.MissingSignature (
  missingSignatureStep01Validator,
  missingSignatureStep02Validator,
  missingSignatureStep03Validator,
  missingSignatureStep04Validator,
) where

import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInInfo,
  PTxInfo (..),
 )
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStep)
import Midgard.FraudProofs.FieldOpening (
  PFieldOpeningV1,
  PNativeTxAnchorV1 (..),
  paddressWitnessesFieldIndex,
  popenedFieldView,
  popenedFieldWalk,
  presumeOpenedFieldWalk,
  prequiredSignersFieldIndex,
 )
import Midgard.FraudProofs.MissingSignature (
  PStep02Args (..),
  PStep02State (..),
  PStep03Args (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
 )
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddressWitness (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldItemAt)
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  pfieldWalkCheckpointHash,
  pwalkFold,
  pwalkRemaining,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstep,
 )

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-signature/step-01.ak@.

Binds the transaction and writes the §2.5 anchor. This is the only step that can
supply the witness-set hash; see the module header.
-}
missingSignatureStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
missingSignatureStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          ppassNativeTxToNextStep
            computationThreadTokenPolicyId
            hubOracle
            datum
            args
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash
               _threadTokenAssetName
               _fraudProver
               _mInputStateData
               outputScriptHash
               outputStateData
               _header
               badTxId
               badTxView -> P.do
                PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
                PNativeTxCompact {pcompact'witnessSetHash, pcompact'validityCode} <- pmatch pverified'txCompact
                pexpecting (pcompact'validityCode #== 0) $
                  pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'verifiedTxId = pdata badTxId
                                      , pstep02State'verifiedWitnessSetHash =
                                          pdata pcompact'witnessSetHash
                                      }
                                  )
                              )
                          )
                    )
                    (pconstant True)

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-signature/step-02.ak@.

Reads the required signer's hash out of field 4. §5.3 fixes the item at a raw
28-byte hash, so signer @n@ is one multiplication and one slice — no reproduction
of the signer list and no re-hash of it.
-}
missingSignatureStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
missingSignatureStep02Validator = plam $
  \step03ValidatorScriptHash
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep02Args
              { pstep02Args'inputIndex
              , pstep02Args'outputIndex
              , pstep02Args'requiredSignersOpening
              , pstep02Args'badRequiredSignerHashIndex
              } <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
            pcontinue
              computationThreadTokenPolicyId
              (pexpectDatum datum)
              (pfromData pstep02Args'inputIndex)
              (pfromData pstep02Args'outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              $ \_ownScriptHash
                 _threadTokenAssetName
                 _fraudProver
                 mInputStateData
                 outputScriptHash
                 outputStateData -> P.do
                  PStep02State {pstep02State'verifiedTxId, pstep02State'verifiedWitnessSetHash} <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  -- Field 4 is a body field, so a body anchor is the right —
                  -- and the only admissible — one.
                  requiredSignersView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'requiredSignersOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'verifiedTxId})
                        # prequiredSignersFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  missingRequiredSignerHash <-
                    plet $
                      pfieldItemAt
                        # requiredSignersView
                        # pfromData pstep02Args'badRequiredSignerHashIndex
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'missingRequiredSignerHash =
                                            pdata missingRequiredSignerHash
                                        , pstep03State'verifiedTxId = pstep02State'verifiedTxId
                                        , pstep03State'verifiedWitnessSetHash =
                                            pstep02State'verifiedWitnessSetHash
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-signature/step-03.ak@.

The bridge between the two collections' shapes: field 4 holds 28-byte hashes and
field 7 holds 32-byte keys, so the key has to arrive in a redeemer and be checked
against the hash the thread carries.
-}
missingSignatureStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
missingSignatureStep03Validator = plam $
  \step04ValidatorScriptHash computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep03Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep03Args
            { pstep03Args'inputIndex
            , pstep03Args'outputIndex
            , pstep03Args'missingRequiredSignerVkey
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadTokenPolicyId
            (pexpectDatum datum)
            (pfromData pstep03Args'inputIndex)
            (pfromData pstep03Args'outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ownScriptHash
               _threadTokenAssetName
               _fraudProver
               mInputStateData
               outputScriptHash
               outputStateData -> P.do
              PStep03State
                { pstep03State'missingRequiredSignerHash
                , pstep03State'verifiedTxId
                , pstep03State'verifiedWitnessSetHash
                } <-
                pmatch (pexpectStateAs @PStep03State mInputStateData)
              -- 2. @get_verification_key_hash@ is @blake2b_224@.
              pexpecting
                ( pblake2b_224 # pfromData pstep03Args'missingRequiredSignerVkey
                    #== pfromData pstep03State'missingRequiredSignerHash
                )
                $ pexpecting (outputScriptHash #== step04ValidatorScriptHash)
                $ pexpecting
                  ( outputStateData
                      #== pforgetData
                        ( pdata
                            ( pcon
                                ( PStep04State
                                    { pstep04State'missingRequiredSignerVkey =
                                        pstep03Args'missingRequiredSignerVkey
                                    , pstep04State'verifiedTxId = pstep03State'verifiedTxId
                                    , pstep04State'verifiedWitnessSetHash =
                                        pstep03State'verifiedWitnessSetHash
                                    , pstep04State'fieldWalkCheckpointHash = pdata (pconstant "")
                                    }
                                )
                            )
                        )
                  )
                  (pconstant True)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-signature/step-04.ak@.

The conviction: the required signer's key is in no address witness.

The anchor here is a @WitnessAnchor@, so the door re-derives the supplied witness
set against the hash the thread carries before it reads anything. See the module
header for why that hash cannot come from anywhere else.
-}
missingSignatureStep04Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
missingSignatureStep04Validator = plam $
  \fraudProofTokenPolicyId
   fraudProofTokenAddress
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> pmatch args $ \case
            PScan inputIndex outputIndex opening checkpointCbor -> P.do
              PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
              pcontinue
                computationThreadTokenPolicyId
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \ownScriptHash _threadTokenAssetName _fraudProver mInputStateData outputScriptHash outputStateData -> P.do
                  state <- plet $ pexpectStateAs @PStep04State mInputStateData
                  PPair view checkpoint <-
                    pmatch $
                      popenOrResumeWitnessWalk
                        state
                        (pfromData opening)
                        (pfromData checkpointCbor)
                        (pfromData ptxInfo'referenceInputs)
                        fieldPreimageCertificatePolicyId
                  pexpecting (pwalkRemaining # checkpoint #> pwitnessScanBatchSize) $ P.do
                    PStep04State {pstep04State'missingRequiredSignerVkey} <- pmatch state
                    PPair found next <-
                      pmatch $
                        pwalkFold @PBool
                          # view
                          # checkpoint
                          # pwitnessScanBatchSize
                          # pconstant False
                          # (pwitnessPresenceFold # pfromData pstep04State'missingRequiredSignerVkey)
                    pexpecting (pnot # found) $
                      pexpecting (outputScriptHash #== ownScriptHash) $
                        pexpecting
                          ( outputStateData
                              #== pforgetData
                                (pdata $ pstateWithCheckpointHash # state # (pfieldWalkCheckpointHash # next))
                          )
                          (pconstant True)
            PFinalize inputIndex outputIndex mintRedeemerIndex opening checkpointCbor -> P.do
              PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
                pmatch txInfo
              pfinalize
                computationThreadTokenPolicyId
                fraudProofTokenPolicyId
                fraudProofTokenAddress
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                (pfromData mintRedeemerIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                (pto (pto (pfromData ptxInfo'redeemers)))
                $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                  state <- plet $ pexpectStateAs @PStep04State mInputStateData
                  PPair view checkpoint <-
                    pmatch $
                      popenOrResumeWitnessWalk
                        state
                        (pfromData opening)
                        (pfromData checkpointCbor)
                        (pfromData ptxInfo'referenceInputs)
                        fieldPreimageCertificatePolicyId
                  remaining <- plet $ pwalkRemaining # checkpoint
                  pexpecting (remaining #<= pwitnessScanBatchSize) $ P.do
                    PStep04State {pstep04State'missingRequiredSignerVkey} <- pmatch state
                    PPair found terminal <-
                      pmatch $
                        pwalkFold @PBool
                          # view
                          # checkpoint
                          # remaining
                          # pconstant False
                          # (pwitnessPresenceFold # pfromData pstep04State'missingRequiredSignerVkey)
                    pexpecting (pnot # found) $
                      pexpecting (pwalkRemaining # terminal #== 0) (pconstant True)

-- | Aiken @witness_scan_batch_size@.
pwitnessScanBatchSize :: forall (s :: S). Term s PInteger
pwitnessScanBatchSize = pconstant 32

popenOrResumeWitnessWalk ::
  forall (s :: S).
  Term s PStep04State ->
  Term s PFieldOpeningV1 ->
  Term s (PMaybeData PByteString) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
popenOrResumeWitnessWalk state opening checkpointCbor referenceInputs certificatePolicyId = P.do
  PStep04State
    { pstep04State'verifiedTxId
    , pstep04State'verifiedWitnessSetHash
    , pstep04State'fieldWalkCheckpointHash
    } <-
    pmatch state
  let anchor =
        pcon
          ( PWitnessAnchor
              { pwitnessAnchor'txId = pstep04State'verifiedTxId
              , pwitnessAnchor'witnessSetHash = pstep04State'verifiedWitnessSetHash
              }
          )
  pif
    (pfromData pstep04State'fieldWalkCheckpointHash #== pconstant "")
    ( pmatch checkpointCbor $ \case
        PDNothing ->
          popenedFieldWalk
            # opening
            # anchor
            # paddressWitnessesFieldIndex
            # referenceInputs
            # certificatePolicyId
        PDJust _ -> perror
    )
    ( pmatch checkpointCbor $ \case
        PDNothing -> perror
        PDJust checkpointBytes ->
          presumeOpenedFieldWalk
            # opening
            # anchor
            # paddressWitnessesFieldIndex
            # pfromData pstep04State'fieldWalkCheckpointHash
            # pfromData checkpointBytes
            # referenceInputs
            # certificatePolicyId
    )

pwitnessPresenceFold ::
  forall (s :: S).
  Term s (PByteString :--> PBool :--> PInteger :--> PByteString :--> PBool)
pwitnessPresenceFold = phoistAcyclic $
  plam $ \missingVkey found _index item ->
    found
      #|| pmatch
        (pdecodeMidgardAddressWitnessCbor # item)
        ( \PMidgardAddressWitness {paddressWitness'verificationKey} ->
            pfromData paddressWitness'verificationKey #== missingVkey
        )

pstateWithCheckpointHash ::
  forall (s :: S).
  Term s (PStep04State :--> PByteString :--> PStep04State)
pstateWithCheckpointHash = phoistAcyclic $
  plam $ \state checkpointHash ->
    pmatch state $
      \PStep04State
         { pstep04State'missingRequiredSignerVkey
         , pstep04State'verifiedTxId
         , pstep04State'verifiedWitnessSetHash
         } ->
          pcon
            ( PStep04State
                { pstep04State'missingRequiredSignerVkey
                , pstep04State'verifiedTxId
                , pstep04State'verifiedWitnessSetHash
                , pstep04State'fieldWalkCheckpointHash = pdata checkpointHash
                }
            )
