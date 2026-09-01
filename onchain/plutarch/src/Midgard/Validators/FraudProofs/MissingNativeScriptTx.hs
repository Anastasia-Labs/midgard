{- |
Module      : Midgard.Validators.FraudProofs.MissingNativeScriptTx
Description : Plutarch port of @validators/fraud-proofs/missing-native-script-tx/step-0{1..6}.ak@.

The missing-native-script-tx fraud proof (spec §5.1.1): a committed transaction
spending a script-locked output whose required native script it never witnessed.

Six validators — the longest chain in the machine, because the claim spans two
transactions and three of their fields. The shape of the argument:

  * step-01 binds the bad transaction and writes both halves of its §2.5 anchor;
  * step-02 opens field 0 and names the spent input;
  * step-03 binds the /producing/ transaction and checks its id is the one that
    input names;
  * step-04 opens the producing transaction's field 2, reads the spent output,
    and requires its payment credential to be a script;
  * step-05 exhibits script bytes hashing to that credential under the native
    language tag;
  * step-06 opens the bad transaction's field 6 and convicts when those bytes
    are absent.

=== The absence is the last step for a reason

Steps 02–05 are all /positive/ claims — this input, this output, this credential,
this preimage — and each is checked against something the block committed. Only
step-06 asserts a negative, and a negative is only as good as the set it is
asserted over. That set is field 6 of the bad transaction, which is why the
witness-set half of the anchor has to survive four intermediate states to reach
it. See 'Midgard.FraudProofs.MissingNativeScriptTx'.

=== Two carriage limits, both loud

Field 6 is variable-width, so step-06's fold needs
'Midgard.FraudProofs.FieldOpening.pfieldItemCount', which is authenticated only
under carriage tiers 1–2; under tier 3 it aborts rather than return the §5.1
header's self-assertion. Independently, a witness-set field is refused tier-3
carriage outright at the door, because a §8.6 certificate cannot be bound to a
transaction id that does not commit the witness set. A prover whose field-6
preimage does not fit tier-2 carriage therefore cannot finalize this family.
Both aborts are unconditional — §7.3's abort-never-clamp, recorded as limits 2
and 3 of @docs/spec/midgard-tx.md@ §8.3 erratum E2.
-}
module Midgard.Validators.FraudProofs.MissingNativeScriptTx (
  missingNativeScriptTxStep01Validator,
  missingNativeScriptTxStep02Validator,
  missingNativeScriptTxStep03Validator,
  missingNativeScriptTxStep04Validator,
  missingNativeScriptTxStep05Validator,
  missingNativeScriptTxStep06Validator,
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

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStep)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  pfoldOpenedField,
  popenedFieldView,
  popenedFieldWalk,
  poutputsFieldIndex,
  pscriptWitnessesFieldIndex,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.MissingNativeScriptTx (
  PStep02Args (..),
  PStep02State (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
  PStep05Args (..),
  PStep05State (..),
  PStep06Args (..),
  PStep06State (..),
 )
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardTxOutputCbor,
  pdecodeMidgardVersionedScriptAt,
  pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardCredential (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardVersionedScript (..),
  PMidgardScriptLanguage (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.ScriptProof (pversionedScriptHash)
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

{- | Aiken @validators/fraud-proofs/missing-native-script-tx/step-01.ak@.

Binds the bad transaction and writes both halves of its §2.5 anchor. Identical in
shape to @invalid-signature@'s step-01, and for the identical reason: this is the
only step that sees the compact structure the block's counted
@transactions_root@ committed, so it is the only step that can read
@witness_set_hash@ rather than be told it.
-}
missingNativeScriptTxStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
missingNativeScriptTxStep01Validator = plam $
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
                PNativeTxCompact {pcompact'witnessSetHash} <- pmatch pverified'txCompact
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'badTxId = pdata badTxId
                                      , pstep02State'badTxWitnessSetHash =
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

{- | Aiken @validators/fraud-proofs/missing-native-script-tx/step-02.ak@.

Names the spent input by opening field 0 of the bad transaction.

The input travels to step-03 whole rather than split into id and index, because
step-03 needs both halves for different purposes — the id to match the producing
transaction it binds, the index to hand step-04.
-}
missingNativeScriptTxStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
missingNativeScriptTxStep02Validator = plam $
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
              , pstep02Args'badInputIndex
              , pstep02Args'spendInputsOpening
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
                  PStep02State {pstep02State'badTxId, pstep02State'badTxWitnessSetHash} <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  spendInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'spendInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
                        # pspendInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  inputWithMissingScript <-
                    plet $
                      pspendInputAt
                        # spendInputsView
                        # pfromData pstep02Args'badInputIndex
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'inputWithMissingScript =
                                            pdata inputWithMissingScript
                                        , pstep03State'badTxId = pstep02State'badTxId
                                        , pstep03State'badTxWitnessSetHash =
                                            pstep02State'badTxWitnessSetHash
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-native-script-tx/step-03.ak@.

Binds the /producing/ transaction — the one whose id the spent input names — and
switches the subject of the field openings from here on.

The check that makes the swap sound is one equality: the newly verified id must
be the id the named input carries. Without it a prover could bind any committed
transaction and open its field 2 at the disputed index, which is a claim about
some other output entirely.
-}
missingNativeScriptTxStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
missingNativeScriptTxStep03Validator = plam $
  \step04ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
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
               mInputStateData
               outputScriptHash
               outputStateData
               _header
               producingTxId
               _producingTxView -> P.do
                PStep03State
                  { pstep03State'inputWithMissingScript
                  , pstep03State'badTxId
                  , pstep03State'badTxWitnessSetHash
                  } <-
                  pmatch (pexpectStateAs @PStep03State mInputStateData)
                PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} <-
                  pmatch (pfromData pstep03State'inputWithMissingScript)
                pexpecting (producingTxId #== pfromData ptxInput'txId) $
                  pexpecting (outputScriptHash #== step04ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep04State
                                        { pstep04State'producingTxId = pdata producingTxId
                                        , pstep04State'badInputOutputIndex = ptxInput'outputIndex
                                        , pstep04State'badTxId = pstep03State'badTxId
                                        , pstep04State'badTxWitnessSetHash =
                                            pstep03State'badTxWitnessSetHash
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-native-script-tx/step-04.ak@.

Reads the spent output out of the producing transaction's field 2 and requires it
to be script-locked.

Field 2 is variable-width, so reaching item @n@ walks @n@ §5.1 heads — a byte
jump each, with no offset table to trust (§7.2). That is the price of outputs
being of unequal size, and it is why the family names the index in state rather
than letting the redeemer re-choose it here.
-}
missingNativeScriptTxStep04Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-05's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
missingNativeScriptTxStep04Validator = plam $
  \step05ValidatorScriptHash
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep04Args
              {pstep04Args'inputIndex, pstep04Args'outputIndex, pstep04Args'outputsOpening} <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
            pcontinue
              computationThreadTokenPolicyId
              (pexpectDatum datum)
              (pfromData pstep04Args'inputIndex)
              (pfromData pstep04Args'outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              $ \_ownScriptHash
                 _threadTokenAssetName
                 _fraudProver
                 mInputStateData
                 outputScriptHash
                 outputStateData -> P.do
                  PStep04State
                    { pstep04State'producingTxId
                    , pstep04State'badInputOutputIndex
                    , pstep04State'badTxId
                    , pstep04State'badTxWitnessSetHash
                    } <-
                    pmatch (pexpectStateAs @PStep04State mInputStateData)
                  outputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep04Args'outputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep04State'producingTxId})
                        # poutputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  PMidgardTxOutput {ptxOutput'address} <-
                    pmatch
                      ( pdecodeMidgardTxOutputCbor
                          #$ pfieldItemAt
                          # outputsView
                          # pfromData pstep04State'badInputOutputIndex
                      )
                  PMidgardAddress {paddress'paymentCredential} <-
                    pmatch (pfromData ptxOutput'address)
                  expectedMissingScriptHash <-
                    plet $
                      pmatch (pfromData paddress'paymentCredential) $ \case
                        PMidgardScriptCredential scriptHash -> scriptHash
                        PMidgardPubKeyCredential _ -> perror
                  pexpecting (outputScriptHash #== step05ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep05State
                                        { pstep05State'expectedMissingScriptHash =
                                            expectedMissingScriptHash
                                        , pstep05State'badTxId = pstep04State'badTxId
                                        , pstep05State'badTxWitnessSetHash =
                                            pstep04State'badTxWitnessSetHash
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 05
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-native-script-tx/step-05.ak@.

Exhibits the script the credential names.

The state this writes is byte-identical to the state it reads, which is the point:
step-05 changes nothing, it establishes that the hash step-04 found is the hash of
a /native/ script. Without it the family would convict on a missing witness for a
Plutus script — a different fault with a different rule.

The tag is the language tag, 0 for native, not the constructor index. For this
family they coincide; see 'Midgard.ScriptProof'.
-}
missingNativeScriptTxStep05Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-06's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
missingNativeScriptTxStep05Validator = plam $
  \step06ValidatorScriptHash computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep05Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep05Args
            { pstep05Args'inputIndex
            , pstep05Args'outputIndex
            , pstep05Args'missingNativeScriptBytes
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadTokenPolicyId
            (pexpectDatum datum)
            (pfromData pstep05Args'inputIndex)
            (pfromData pstep05Args'outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ownScriptHash
               _threadTokenAssetName
               _fraudProver
               mInputStateData
               outputScriptHash
               outputStateData -> P.do
              PStep05State
                { pstep05State'expectedMissingScriptHash
                , pstep05State'badTxId
                , pstep05State'badTxWitnessSetHash
                } <-
                pmatch (pexpectStateAs @PStep05State mInputStateData)
              pexpecting
                ( pfromData pstep05State'expectedMissingScriptHash
                    #== pversionedScriptHash
                    # pcon
                      ( PMidgardVersionedScript
                          { pversionedScript'language = pdata (pcon PNativeCardanoScript)
                          , pversionedScript'scriptBytes = pstep05Args'missingNativeScriptBytes
                          }
                      )
                )
                $ pexpecting (outputScriptHash #== step06ValidatorScriptHash)
                $ pexpecting
                  ( outputStateData
                      #== pforgetData
                        ( pdata
                            ( pcon
                                ( PStep06State
                                    { pstep06State'expectedMissingScriptHash =
                                        pstep05State'expectedMissingScriptHash
                                    , pstep06State'badTxId = pstep05State'badTxId
                                    , pstep06State'badTxWitnessSetHash =
                                        pstep05State'badTxWitnessSetHash
                                    }
                                )
                            )
                        )
                  )
                  (pconstant True)

--------------------------------------------------------------------------------
-- Step 06
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/missing-native-script-tx/step-06.ak@.

The conviction: the required script is absent from the bad transaction's field 6.

Field 6 is variable-width, so the absence claim __walks__ it once rather than
indexing it — indexing item @n@ re-walks from item 0 every time, so a scan by
index is quadratic in the field.

Each item is re-encoded and compared to the bytes the field committed. The
decoder reads a /prefix/: it stops at the end of the versioned-script structure
and says nothing about trailing bytes, and @decode_definite_bytes_at@ accepts a
non-minimal length prefix (@58 05@ where @45@ was canonical). Either would let two
distinct committed items decode to the same script, so the hash below would be a
statement about something the field did not commit. Re-encoding pins both at
once — §6.1 canonicality, re-established rather than assumed.
-}
missingNativeScriptTxStep06Validator ::
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
missingNativeScriptTxStep06Validator = plam $
  \computationThreadTokenPolicyId
   fraudProofTokenPolicyId
   fraudProofTokenAddress
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep06Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep06Args
              { pstep06Args'inputIndex
              , pstep06Args'outputIndex
              , pstep06Args'fraudProofMintRedeemerIndex
              , pstep06Args'scriptTxWitsOpening
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
              (pfromData pstep06Args'inputIndex)
              (pfromData pstep06Args'outputIndex)
              (pfromData pstep06Args'fraudProofMintRedeemerIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto (pto (pfromData ptxInfo'redeemers)))
              $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                PStep06State
                  { pstep06State'expectedMissingScriptHash
                  , pstep06State'badTxId
                  , pstep06State'badTxWitnessSetHash
                  } <-
                  pmatch (pexpectStateAs @PStep06State mInputStateData)
                expectedMissingScriptHash <-
                  plet $ pfromData pstep06State'expectedMissingScriptHash
                scriptTxWitsWalk <-
                  plet $
                    popenedFieldWalk
                      # pfromData pstep06Args'scriptTxWitsOpening
                      # pcon
                        ( PWitnessAnchor
                            { pwitnessAnchor'txId = pstep06State'badTxId
                            , pwitnessAnchor'witnessSetHash = pstep06State'badTxWitnessSetHash
                            }
                        )
                      # pscriptWitnessesFieldIndex
                      # referenceInputs
                      # fieldPreimageCertificatePolicyId
                requiredScriptIsPresent <-
                  plet $
                    pfoldOpenedField @PBool
                      # scriptTxWitsWalk
                      # pconstant False
                      # plam
                        ( \found _index item -> P.do
                            PPair _offset scriptWit <-
                              pmatch (pdecodeMidgardVersionedScriptAt # item # 0)
                            pexpecting (pencodeMidgardVersionedScript # scriptWit #== item) $
                              found
                                #|| (pversionedScriptHash # scriptWit #== expectedMissingScriptHash)
                        )
                pexpecting (pnot # requiredScriptIsPresent) (pconstant True)
