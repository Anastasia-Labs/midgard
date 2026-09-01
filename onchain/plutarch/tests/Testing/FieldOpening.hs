{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FieldOpening
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/fraud-proofs/field-opening-v1.ak@ — the bridge a
              family step reaches the field-access door through.

The door itself is tested in "Testing.NativeTxFieldAccess" and the walk core in
"Testing.NativeTxMachineWalk". What this module owns is everything the bridge
adds on top of them, which is three things and no more.

__The anchor is paid once, and it is an anchor.__ 'panchoredNativeTx' re-derives
the transaction id from the redeemer's compact bytes, and for a witness-set
opening also checks the re-derived @witness_set_hash@ against the one thread
state named. The second check is the one §3 cannot make for itself: the id
preimage is the body CBOR alone, so a prover may hand over the /genuine/ body —
which re-derives to the anchored id — followed by a @witness_set_hash@ of its
own choosing. The forgery that matters is the empty witness set, because it
makes every §2.5 absence rule true of every transaction, and it is reproduced
here as a test rather than described.

__The §2.5 half stays enforced per field.__ A handle checked out under a body
opening must still be refused at fields 6–8, and one checked out under a witness
opening must be refused at 0–5. Both directions are asserted against the handle
that a multi-field step would hold, which is the shape the single-field entry
points could not get wrong and the handle can.

__Tier 3 does not reach the anchor for a witness-set field.__ The §8.6
certificate names @(tx_id, field_index)@ and is minted against a
@witness_set_hash@ the minter's own caller supplied, so it certifies nothing
about fields 6–8 of the disputed transaction. The refusal is an abort, not a
fallback to a lower tier.

The reference encoders below are written from the format — §2.5's positional
table, §3's body-alone id preimage, §5.1's envelope — rather than from the port,
so a change on either side fails a test instead of two copies agreeing.
-}
module Testing.FieldOpening (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PTxInInfo)
import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (
  PAnchoredNativeTxV1,
  PFieldOpeningV1 (..),
  PNativeTxAnchorV1 (..),
  PNativeTxOpeningV1 (..),
  paddressWitnessesFieldIndex,
  panchoredFieldView,
  panchoredFieldWalk,
  panchoredNativeTx,
  panchoredNativeTxVersion,
  pfirstWitnessSetFieldIndex,
  pfoldOpenedField,
  pmintFieldIndex,
  popenedFieldView,
  popenedFieldWalk,
  poutputsFieldIndex,
  predeemersFieldIndex,
  preferenceInputsFieldIndex,
  prequiredObserversFieldIndex,
  prequiredSignersFieldIndex,
  pscriptWitnessesFieldIndex,
  pspendInputsFieldIndex,
  punanchoredValidityCodeOf,
 )
import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact (..))
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1 (..),
  PFieldViewV1,
  pfieldItemAt,
  pfieldItemCount,
 )
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  pwalkFieldIndex,
  pwalkIsComplete,
  pwalkNextItemIndex,
  pwalkTxId,
 )
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Field Opening Tests"
    [ testGroup "§2.5 field indices" indexTests
    , testGroup "wire format" wireFormatTests
    , testGroup "anchored_native_tx" anchorTests
    , testGroup "handle accessors" accessorTests
    , testGroup "field_pairs_with / §2.5 half" halfTests
    , testGroup "field_pairs_with / carriage_reaches_the_anchor" tierTests
    , testGroup "anchored_field_view" viewTests
    , testGroup "anchored_field_walk" walkTests
    , testGroup "opened_field_view / opened_field_walk" singleFieldTests
    , testGroup "fold_opened_field" foldTests
    ]

--------------------------------------------------------------------------------
-- §2.5's positional table
--------------------------------------------------------------------------------

{- | The nine slots, written out from §2.5 rather than read back from the port.
They are load-bearing twice over: §4 removed field-index domain separation, so
fields 0 and 1 — and 3 and 4 — commit identically for identical items, and the
index is the only thing that tells them apart; and the 6 boundary is what
'pfirstWitnessSetFieldIndex' splits the body from the witness set at.
-}
indexTests :: [TestTree]
indexTests =
  [ testCase "spend_inputs is 0" $ passertEval $ pspendInputsFieldIndex #== 0
  , testCase "reference_inputs is 1" $ passertEval $ preferenceInputsFieldIndex #== 1
  , testCase "outputs is 2" $ passertEval $ poutputsFieldIndex #== 2
  , testCase "required_observers is 3" $ passertEval $ prequiredObserversFieldIndex #== 3
  , testCase "required_signers is 4" $ passertEval $ prequiredSignersFieldIndex #== 4
  , testCase "mint is 5" $ passertEval $ pmintFieldIndex #== 5
  , testCase "script_witnesses is 6" $ passertEval $ pscriptWitnessesFieldIndex #== 6
  , testCase "address_witnesses is 7" $ passertEval $ paddressWitnessesFieldIndex #== 7
  , testCase "redeemers is 8" $ passertEval $ predeemersFieldIndex #== 8
  , -- Named off the table rather than restated as its own literal, so the
    -- boundary and the indices cannot drift apart.
    testCase "the split point is the script-witnesses index, not a literal" $
      passertEval $
        pfirstWitnessSetFieldIndex #== pscriptWitnessesFieldIndex
  ]

--------------------------------------------------------------------------------
-- Wire format
--------------------------------------------------------------------------------

{- | 'PFieldOpeningV1' is a redeemer field on the Q1x step validators and
'PNativeTxAnchorV1' is thread state, so both encodings are interfaces an SDK
builds against. Aiken has no Scott encoding: every constructor is @Constr n@ in
declaration order, and the expectations below are written out from the Aiken
declarations rather than read back from the port.
-}
wireFormatTests :: [TestTree]
wireFormatTests =
  [ testCase "BodyFieldOpening is Constr 0 with the CBOR then the carriage" $
      passertEval $
        pencodes
          (bodyOpeningT compactCbor (inlineCarriage observersPreimage))
          ( PD.Constr
              0
              [PD.B compactCbor, PD.Constr 0 [PD.B observersPreimage]]
          )
  , testCase "WitnessFieldOpening is Constr 1 with the witness set in the middle" $
      passertEval $
        pencodes
          (witnessOpeningT compactCbor defaultWitnessSet (inlineCarriage addressWitnessPreimage))
          ( PD.Constr
              1
              [ PD.B compactCbor
              , witnessSetData defaultWitnessSet
              , PD.Constr 0 [PD.B addressWitnessPreimage]
              ]
          )
  , -- The witness set is a nested `Constr 0`, not three bare fields spliced
    -- into the opening. It ports from an Aiken record, and an Aiken record is
    -- `Constr 0` — a bare list would be a different, silently wrong, format.
    testCase "NativeTxWitnessSetCompact is Constr 0 with three hashes" $
      passertEval $
        pencodes (witnessSetT defaultWitnessSet) (witnessSetData defaultWitnessSet)
  , testCase "the witness set is not a bare list" $
      passertEval $
        pnot
          #$ pencodes
            (witnessSetT defaultWitnessSet)
            (PD.List [PD.B (wAddr defaultWitnessSet), PD.B (wScript defaultWitnessSet), PD.B (wRedeemer defaultWitnessSet)])
  , testCase "BodyAnchor is Constr 0 with the transaction id alone" $
      passertEval $
        pencodes (bodyAnchorT txId) (PD.Constr 0 [PD.B txId])
  , testCase "WitnessAnchor is Constr 1 with the id then the witness-set hash" $
      passertEval $
        pencodes
          (witnessAnchorT txId (wsHashOf defaultWitnessSet))
          (PD.Constr 1 [PD.B txId, PD.B (wsHashOf defaultWitnessSet)])
  ]

--------------------------------------------------------------------------------
-- Paying the anchor
--------------------------------------------------------------------------------

anchorTests :: [TestTree]
anchorTests =
  [ testCase "a body opening against the anchored id yields a handle" $
      passertEval $ opensBody txId compactCbor
  , testCase "a body opening whose bytes re-derive to another id aborts" $
      pfails $ opensBody otherTxId compactCbor
  , -- §3: the id preimage is the body CBOR alone. A different fee is a
    -- different body, so the same anchor cannot cover it.
    testCase "a body opening of a different transaction aborts" $
      pfails $ opensBody txId otherCompactCbor
  , -- The two §2.5 halves have to agree before any byte is read: a
    -- `WitnessAnchor` here means the family's own step-01 recorded a
    -- witness-set anchor and this step is opening it without the witness set
    -- that anchor names.
    testCase "a body opening against a WitnessAnchor aborts" $
      pfails $
        pisHandle
          ( panchoredNativeTx
              # bodyTxOpeningT compactCbor
              # witnessAnchorT txId (wsHashOf defaultWitnessSet)
          )
  , testCase "a witness opening against the anchored pair yields a handle" $
      passertEval $ opensWitness txId (wsHashOf defaultWitnessSet) defaultWitnessSet
  , testCase "a witness opening against a BodyAnchor aborts" $
      pfails $
        pisHandle (panchoredNativeTx # witnessTxOpeningT compactCbor defaultWitnessSet # bodyAnchorT txId)
  , {- The check the transaction id cannot make for itself. The compact bytes
       here are the genuine ones — they re-derive to `txId` and so satisfy the
       first check — and the anchor names a `witness_set_hash` they do not
       carry. -}
    testCase "a witness opening whose bytes carry an unanchored witness_set_hash aborts" $
      pfails $ opensWitness txId (wsHashOf otherWitnessSet) otherWitnessSet
  , {- The forgery that makes this check load-bearing, reproduced end to end: a
       prover pairs the genuine body with the empty witness set's hash. Every
       §2.5 absence rule is then true of every transaction. It is refused
       because thread state named the committed hash, not this one. -}
    testCase "the empty-witness-set substitution is refused" $
      pfails $
        pisHandle
          ( panchoredNativeTx
              # witnessTxOpeningT (compactWith (wsHashOf emptyWitnessSet)) emptyWitnessSet
              # witnessAnchorT txId (wsHashOf defaultWitnessSet)
          )
  , {- ...and the same substitution is accepted when thread state is the one
       that named it, which is what shows the previous case turns on the anchor
       rather than on anything about the witness set's contents. -}
    testCase "an anchor that names the substituted hash accepts it" $
      passertEval $
        pisHandle
          ( panchoredNativeTx
              # witnessTxOpeningT (compactWith (wsHashOf emptyWitnessSet)) emptyWitnessSet
              # witnessAnchorT txId (wsHashOf emptyWitnessSet)
          )
  , -- A witness set whose hash the anchor names is still not checked to be the
    -- witness set the *bytes* carry unless those two agree — which is exactly
    -- what the equality above is. Here the anchor and the bytes agree and the
    -- supplied witness set does not hash to either.
    testCase "a witness set that does not hash to the anchored value aborts at the door" $
      pfails $
        pfieldItemCount
          #$ panchoredFieldView
            # ( panchoredNativeTx
                  # witnessTxOpeningT compactCbor otherWitnessSet
                  # witnessAnchorT txId (wsHashOf defaultWitnessSet)
              )
            # pconstant 7
            # inlineCarriage addressWitnessPreimage
            # inputsT []
            # pdata (pconstant certificatePolicy)
  ]

--------------------------------------------------------------------------------
-- What the handle will say about itself
--------------------------------------------------------------------------------

accessorTests :: [TestTree]
accessorTests =
  [ -- Anchored: the version byte is inside §3's id derivation, not beside it.
    testCase "anchored_native_tx_version is the committed version" $
      passertEval $
        panchoredNativeTxVersion # bodyHandle #== 1
  , -- Not anchored, and the name says so. §3's preimage is the body alone, so
    -- the trailing validity code is the prover's; the accessor hands back
    -- whatever the redeemer's bytes carried.
    testCase "unanchored_validity_code_of returns the code in the supplied bytes" $
      passertEval $
        punanchoredValidityCodeOf # bodyHandle #== 3
  , testCase "a different trailing code re-derives to the same id and is returned unchanged" $
      passertEval $
        pand'List
          [ punanchoredValidityCodeOf # handleOf (compactCodeOf 0) #== 0
          , panchoredNativeTxVersion # handleOf (compactCodeOf 0) #== 1
          ]
  ]

--------------------------------------------------------------------------------
-- The §2.5 half, per field
--------------------------------------------------------------------------------

{- | The pairing has to hold for every field a handle opens, not once for the
handle. Both directions are refusals of forgeries: a body handle read at a
witness index would hand the door the module's internal placeholder witness set
as if it were the transaction's, and a witness handle read at a body index would
carry a witness set the door silently ignores.
-}
halfTests :: [TestTree]
halfTests =
  [ testCase "a body handle opens a body field" $
      passertEval $ bodyOpensAt 3 observersPreimage
  , testCase "a body handle is refused at script_witnesses" $
      pfails $ bodyOpensAt 6 scriptWitnessPreimage
  , testCase "a body handle is refused at address_witnesses" $
      pfails $ bodyOpensAt 7 addressWitnessPreimage
  , testCase "a body handle is refused at redeemers" $
      pfails $ bodyOpensAt 8 redeemerPreimage
  , testCase "a witness handle opens a witness field" $
      passertEval $ witnessOpensAt 7 addressWitnessPreimage
  , testCase "a witness handle is refused at spend_inputs" $
      pfails $ witnessOpensAt 0 spendInputPreimage
  , testCase "a witness handle is refused at required_observers" $
      pfails $ witnessOpensAt 3 observersPreimage
  , testCase "a witness handle is refused at mint — the last body slot" $
      pfails $ witnessOpensAt 5 mintPreimage
  ]

--------------------------------------------------------------------------------
-- The carriage tier, per field
--------------------------------------------------------------------------------

{- | Tiers 1 and 2 hand the door the whole preimage and the door hashes it
against a commitment this module already pinned to thread state. Tier 3 cannot:
the door never hashes the preimage and the §8.6 certificate is the binding
instead — and that certificate is minted against a @witness_set_hash@ its own
caller supplied, which §3's id does not cover. So tier 3 is admissible for a
body field and inadmissible for a witness-set one, and the refusal is an abort
rather than a silent fallback to a lower tier.
-}
tierTests :: [TestTree]
tierTests =
  [ testCase "tier 1 reaches the anchor for a body field" $
      passertEval $ bodyOpensAt 3 observersPreimage
  , testCase "tier 1 reaches the anchor for a witness-set field" $
      passertEval $ witnessOpensAt 7 addressWitnessPreimage
  , -- Tier 3 at a body field: refused here for a reason that is *not* the tier
    -- rule — the fixture carries no certificate reference input, so the door
    -- itself aborts. What the pair of cases below establishes is that the body
    -- field gets as far as the door and the witness-set field does not.
    testCase "tier 3 at a body field is refused by the door, not by the tier rule" $
      pfails $ pfieldItemCount #$ bodyViewWith 3 certifiedCarriage
  , testCase "tier 3 at a witness-set field is refused before the door" $
      pfails $ pfieldItemCount #$ witnessViewWith 7 certifiedCarriage
  , {- The distinguishing test. `field_pairs_with` asserts the tier guard first
       and the §2.5 half second, so a tier-3 *body* opening on a *witness*
       handle must fail the half rule — and a tier-3 witness opening fails the
       tier rule whichever handle it is on. Neither reaches the door, and the
       one thing this can show without a certificate fixture is that no tier-3
       witness-set opening is ever admissible on either handle. -}
    testCase "tier 3 at a witness-set field is refused on a body handle too" $
      pfails $ pfieldItemCount #$ bodyViewWith 7 certifiedCarriage
  , testCase "tier 3 is refused at every witness-set index" $
      pfails $ pfieldItemCount #$ witnessViewWith 6 certifiedCarriage
  , testCase "tier 3 is refused at the redeemers index" $
      pfails $ pfieldItemCount #$ witnessViewWith 8 certifiedCarriage
  , -- The Aiken neutralisation twin: tier 3 is scoped away from witness-set
    -- fields, not disabled altogether. Supply a real certificate and both
    -- chunks so the identical oversized carriage reaches a body field.
    testCase "certified_carriage_still_opens_on_a_body_field" $
      passertEval $
        pfieldItemAt # certifiedBodyView # 0 #== pconstant addressWitnessItem
  ]

--------------------------------------------------------------------------------
-- Opening a view against a handle
--------------------------------------------------------------------------------

viewTests :: [TestTree]
viewTests =
  [ testCase "the view reads the field the handle was opened at" $
      passertEval $
        pand'List
          [ pfieldItemCount # bodyViewAt 3 observersPreimage #== 3
          , pfieldItemAt # bodyViewAt 3 observersPreimage # 0 #== pconstant (hash28 0x41)
          , pfieldItemAt # bodyViewAt 3 observersPreimage # 2 #== pconstant (hash28 0x43)
          ]
  , -- §4 removed field-index domain separation: the same preimage commits
    -- identically at 3 and 4, and the index is the only thing that separates
    -- them. The body here commits it at 3 only.
    testCase "the same preimage is refused at the sibling slot the body does not commit" $
      pfails $ pfieldItemCount #$ bodyViewOf onlyObserverSlot 4 observersPreimage
  , testCase "and is accepted at the slot the body does commit" $
      passertEval $
        pfieldItemCount # bodyViewOf onlyObserverSlot 3 observersPreimage #== 3
  , -- §7.1: one handle, two fields, both authenticated. The saving the handle
    -- exists for is the id derivation, which is paid once.
    testCase "one handle opens two different fields" $
      passertEval $
        pand'List
          [ pfieldItemCount # bodyViewWith 3 (inlineCarriage observersPreimage) #== 3
          , pfieldItemCount # bodyViewWith 4 (inlineCarriage signersPreimage) #== 2
          ]
  , -- Aiken's live-handle regression: prove the witness handle works under
    -- tier 1, then reuse that same anchor and refuse tier 3 on field 7.
    testCase "one_anchor_refuses_certified_carriage_at_a_witness_field_on_the_second_open" $
      pfails $
        plet witnessHandle $ \anchored ->
          pand'List
            [ pfieldItemCount
                # ( panchoredFieldView
                      # anchored
                      # 7
                      # inlineCarriage addressWitnessPreimage
                      # inputsT []
                      # pdata (pconstant certificatePolicy)
                  )
                #== 1
            , pfieldItemCount # certifiedViewAt anchored 7 #== 256
            ]
  , -- Neutralisation twin for the handle route: accepting certified carriage
    -- is scoped to body fields even when it is the handle's second opening.
    testCase "one_anchor_still_opens_certified_carriage_on_a_body_field_second" $
      passertEval $
        plet bodyHandle $ \anchored ->
          pand'List
            [ pfieldItemAt
                # ( panchoredFieldView
                      # anchored
                      # 0
                      # inlineCarriage spendInputPreimage
                      # inputsT []
                      # pdata (pconstant certificatePolicy)
                  )
                # 0
                #== pconstant (head spendInputItems)
            , pfieldItemAt # certifiedViewAt anchored 5 # 0
                #== pconstant addressWitnessItem
            ]
  , testCase "a preimage the body does not commit is refused" $
      pfails $ pfieldItemCount #$ bodyViewWith 3 (inlineCarriage signersPreimage)
  ]

--------------------------------------------------------------------------------
-- Opening a walk against a handle
--------------------------------------------------------------------------------

{- | The walk is the same authentication with a §10.2-derived starting position
attached. The position is derived from the authenticated view rather than taken
from a redeemer, so the checkpoint's own field index and transaction id are the
handle's and cannot be crossed with another field's.
-}
walkTests :: [TestTree]
walkTests =
  [ testCase "the opened walk starts at item 0" $
      passertEval $ pwalkNextItemIndex # bodyCheckpointAt 3 observersPreimage #== 0
  , testCase "the checkpoint carries the field index it was opened at" $
      passertEval $ pwalkFieldIndex # bodyCheckpointAt 3 observersPreimage #== 3
  , testCase "the checkpoint carries the anchored transaction id" $
      passertEval $ pwalkTxId # bodyCheckpointAt 3 observersPreimage #== pconstant txId
  , testCase "a fresh walk over a non-empty field is not complete" $
      passertEval $ pnot #$ pwalkIsComplete # bodyCheckpointAt 3 observersPreimage
  , -- The pairing rules are the view's, re-run: the walk shares the anchor and
    -- nothing else.
    testCase "the walk applies the §2.5 half rule" $
      pfails $ pwalkFieldIndex #$ bodyCheckpointAt 7 addressWitnessPreimage
  , testCase "the walk applies the tier rule" $
      pfails $
        pwalkFieldIndex
          #$ psnd
            ( panchoredFieldWalk
                # witnessHandle
                # pconstant 7
                # certifiedCarriage
                # inputsT []
                # pdata (pconstant certificatePolicy)
            )
  , -- `anchored_field_walk` owns a separate pairing guard. First prove the
    -- handle is live, then exercise that guard with authenticated tier 3.
    testCase "one_anchor_walk_refuses_certified_carriage_at_a_witness_field_on_the_second_open" $
      pfails $
        plet witnessHandle $ \anchored ->
          pand'List
            [ pfieldItemCount
                # ( panchoredFieldView
                      # anchored
                      # 7
                      # inlineCarriage addressWitnessPreimage
                      # inputsT []
                      # pdata (pconstant certificatePolicy)
                  )
                #== 1
            , pmatch
                ( panchoredFieldWalk
                    # anchored
                    # 7
                    # certifiedCarriageFor forgedWitnessChunks
                    # inputsT (certificateRefInput 7 : map chunkRefInput forgedWitnessChunks)
                    # pdata (pconstant certificatePolicy)
                )
                $ \(PPair view _) -> pfieldItemCount # view #== 256
            ]
  ]

--------------------------------------------------------------------------------
-- The single-field spellings
--------------------------------------------------------------------------------

{- | @opened_field_view@ is @anchored_native_tx@ followed by
@anchored_field_view@ and nothing else. The tests state that as an equality of
outcomes rather than restating the guards a third time — and then check that the
opening's own constructor is what supplies the carriage and the witness set, so
that the two-argument form cannot be crossed.
-}
singleFieldTests :: [TestTree]
singleFieldTests =
  [ testCase "opened_field_view agrees with the two-step form" $
      passertEval $
        pfieldItemCount # openedBodyView 3 observersPreimage
          #== pfieldItemCount # bodyViewAt 3 observersPreimage
  , testCase "opened_field_walk agrees with the two-step form" $
      passertEval $
        pand'List
          [ pwalkFieldIndex # openedBodyCheckpoint 3 observersPreimage #== 3
          , pwalkNextItemIndex # openedBodyCheckpoint 3 observersPreimage #== 0
          ]
  , testCase "a witness opening opens a witness-set field" $
      passertEval $
        pfieldItemCount
          # ( popenedFieldView
                # witnessOpeningT compactCbor defaultWitnessSet (inlineCarriage addressWitnessPreimage)
                # witnessAnchorT txId (wsHashOf defaultWitnessSet)
                # pconstant 7
                # inputsT []
                # pdata (pconstant certificatePolicy)
            )
          #== 1
  , -- The constructor is the pairing: a `BodyFieldOpening` carries no witness
    -- set, so there is no way to spell "read field 7 with no witness set".
    testCase "a body opening is refused at a witness-set index" $
      pfails $
        pfieldItemCount
          # ( popenedFieldView
                # bodyOpeningT compactCbor (inlineCarriage addressWitnessPreimage)
                # bodyAnchorT txId
                # pconstant 7
                # inputsT []
                # pdata (pconstant certificatePolicy)
            )
  , testCase "a witness opening is refused at a body index" $
      pfails $
        pfieldItemCount
          # ( popenedFieldView
                # witnessOpeningT compactCbor defaultWitnessSet (inlineCarriage observersPreimage)
                # witnessAnchorT txId (wsHashOf defaultWitnessSet)
                # pconstant 3
                # inputsT []
                # pdata (pconstant certificatePolicy)
            )
  , testCase "the anchor still has to match the opening's bytes" $
      pfails $
        pfieldItemCount
          # ( popenedFieldView
                # bodyOpeningT otherCompactCbor (inlineCarriage observersPreimage)
                # bodyAnchorT txId
                # pconstant 3
                # inputsT []
                # pdata (pconstant certificatePolicy)
            )
  , testCase "certified_carriage_is_refused_at_the_address_witness_field" $
      pfails $
        pfieldItemCount # openedCertifiedWitnessView 7 #== 256
  , -- Fields 6 and 8 are variable-width, so read the forged first item just as
    -- Aiken does; counting would abort at a separate tier-3 E2 restriction.
    testCase "certified_carriage_is_refused_at_the_script_witness_field" $
      pfails $
        pfieldItemAt # openedCertifiedWitnessView 6 # 0
          #== pconstant addressWitnessItem
  , testCase "certified_carriage_is_refused_at_the_redeemer_field" $
      pfails $
        pfieldItemAt # openedCertifiedWitnessView 8 # 0
          #== pconstant addressWitnessItem
  , testCase "opened_field_walk_refuses_certified_carriage_on_a_witness_field" $
      pfails $
        pfieldItemCount # openedCertifiedWitnessWalkView 7 #== 256
  ]

--------------------------------------------------------------------------------
-- Folding a whole field
--------------------------------------------------------------------------------

{- | The budget is the field's own authenticated item count and completion is
asserted, so a fold either sees every item in order exactly once or aborts.
There is no partial answer, which is what makes an absence rule — "no item is
this signature" — sound.
-}
foldTests :: [TestTree]
foldTests =
  [ testCase "every item is visited once, in order" $
      passertEval $
        foldConcat (bodyOpenedWalk 3 observersPreimage)
          #== pconstant (BS.concat [hash28 0x41, hash28 0x42, hash28 0x43])
  , testCase "the index the step is handed counts up from zero" $
      passertEval $
        foldIndices (bodyOpenedWalk 3 observersPreimage) #== 0 + 1 + 2
  , testCase "the item count is the number of steps taken" $
      passertEval $
        foldCount (bodyOpenedWalk 3 observersPreimage) #== 3
  , -- A variable-width field folds by the same route: the walk is what makes an
    -- O(N) read of a field whose items are not a fixed stride apart possible at
    -- all.
    testCase "a variable-width field folds item by item" $
      passertEval $
        pand'List
          [ foldCount (bodyOpenedWalk 2 variablePreimage) #== 3
          , foldConcat (bodyOpenedWalk 2 variablePreimage)
              #== pconstant (BS.concat variableItems)
          ]
  , -- A field of one item still has to complete.
    testCase "a single-item field folds to that item" $
      passertEval $
        foldConcat (witnessOpenedWalk 7 addressWitnessPreimage)
          #== pconstant addressWitnessItem
  , -- The guards do not move because a fold is what follows: a fold of a field
    -- the handle may not open aborts before the first step.
    testCase "a fold of a field the handle may not open aborts" $
      pfails $ foldCount (bodyOpenedWalk 7 addressWitnessPreimage)
  ]

--------------------------------------------------------------------------------
-- Driving the module
--------------------------------------------------------------------------------

-- | Does this handle exist? Anything that reads it forces the checks above it.
pisHandle :: forall s. Term s PAnchoredNativeTxV1 -> Term s PBool
pisHandle handle = panchoredNativeTxVersion # handle #== 1

opensBody :: forall s. BS.ByteString -> BS.ByteString -> Term s PBool
opensBody anchoredId cbor =
  pisHandle (panchoredNativeTx # bodyTxOpeningT cbor # bodyAnchorT anchoredId)

opensWitness :: forall s. BS.ByteString -> BS.ByteString -> Ws -> Term s PBool
opensWitness anchoredId anchoredWsHash ws =
  pisHandle
    (panchoredNativeTx # witnessTxOpeningT compactCbor ws # witnessAnchorT anchoredId anchoredWsHash)

-- | The handle every body test opens against.
bodyHandle :: forall s. Term s PAnchoredNativeTxV1
bodyHandle = handleOf compactCbor

handleOf :: forall s. BS.ByteString -> Term s PAnchoredNativeTxV1
handleOf cbor = panchoredNativeTx # bodyTxOpeningT cbor # bodyAnchorT txId

-- | The handle every witness-set test opens against.
witnessHandle :: forall s. Term s PAnchoredNativeTxV1
witnessHandle =
  panchoredNativeTx
    # witnessTxOpeningT compactCbor defaultWitnessSet
    # witnessAnchorT txId (wsHashOf defaultWitnessSet)

{- | Open @fieldIndex@ against the body handle, over a transaction whose body
commits @preimage@ at that index.
-}
bodyViewAt :: forall s. Integer -> BS.ByteString -> Term s PFieldViewV1
bodyViewAt fieldIndex preimage = bodyViewOf (bodyCommitting fieldIndex preimage) fieldIndex preimage

bodyViewOf :: forall s. Body -> Integer -> BS.ByteString -> Term s PFieldViewV1
bodyViewOf body fieldIndex preimage =
  panchoredFieldView
    # (panchoredNativeTx # bodyTxOpeningT cbor # bodyAnchorT (txIdOfBody body))
    # pconstant fieldIndex
    # inlineCarriage preimage
    # inputsT []
    # pdata (pconstant certificatePolicy)
  where
    cbor = compactOfBody body (wsHashOf defaultWitnessSet) 3

-- | The same, with the carriage under the caller's control.
bodyViewWith :: forall s. Integer -> Term s PFieldCarriageV1 -> Term s PFieldViewV1
bodyViewWith fieldIndex carriage =
  panchoredFieldView
    # bodyHandle
    # pconstant fieldIndex
    # carriage
    # inputsT []
    # pdata (pconstant certificatePolicy)

witnessViewWith :: forall s. Integer -> Term s PFieldCarriageV1 -> Term s PFieldViewV1
witnessViewWith fieldIndex carriage =
  panchoredFieldView
    # witnessHandle
    # pconstant fieldIndex
    # carriage
    # inputsT []
    # pdata (pconstant certificatePolicy)

-- | Open @fieldIndex@ against the witness handle.
witnessViewAt :: forall s. Integer -> BS.ByteString -> Term s PFieldViewV1
witnessViewAt fieldIndex preimage =
  panchoredFieldView
    # witnessHandle
    # pconstant fieldIndex
    # inlineCarriage preimage
    # inputsT []
    # pdata (pconstant certificatePolicy)

-- | Did this opening produce a view? Reading the count forces every guard.
bodyOpensAt :: forall s. Integer -> BS.ByteString -> Term s PBool
bodyOpensAt fieldIndex preimage = pfieldItemCount # bodyViewAt fieldIndex preimage #>= 0

witnessOpensAt :: forall s. Integer -> BS.ByteString -> Term s PBool
witnessOpensAt fieldIndex preimage = pfieldItemCount # witnessViewAt fieldIndex preimage #>= 0

bodyOpenedWalk ::
  forall s. Integer -> BS.ByteString -> Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
bodyOpenedWalk fieldIndex preimage =
  panchoredFieldWalk
    # (panchoredNativeTx # bodyTxOpeningT cbor # bodyAnchorT (txIdOfBody body))
    # pconstant fieldIndex
    # inlineCarriage preimage
    # inputsT []
    # pdata (pconstant certificatePolicy)
  where
    body = bodyCommitting fieldIndex preimage
    cbor = compactOfBody body (wsHashOf defaultWitnessSet) 3

witnessOpenedWalk ::
  forall s. Integer -> BS.ByteString -> Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
witnessOpenedWalk fieldIndex preimage =
  panchoredFieldWalk
    # witnessHandle
    # pconstant fieldIndex
    # inlineCarriage preimage
    # inputsT []
    # pdata (pconstant certificatePolicy)

bodyCheckpointAt :: forall s. Integer -> BS.ByteString -> Term s PFieldWalkCheckpointV1
bodyCheckpointAt fieldIndex preimage = psnd (bodyOpenedWalk fieldIndex preimage)

openedBodyView :: forall s. Integer -> BS.ByteString -> Term s PFieldViewV1
openedBodyView fieldIndex preimage =
  popenedFieldView
    # bodyOpeningT cbor (inlineCarriage preimage)
    # bodyAnchorT (txIdOfBody body)
    # pconstant fieldIndex
    # inputsT []
    # pdata (pconstant certificatePolicy)
  where
    body = bodyCommitting fieldIndex preimage
    cbor = compactOfBody body (wsHashOf defaultWitnessSet) 3

openedBodyCheckpoint :: forall s. Integer -> BS.ByteString -> Term s PFieldWalkCheckpointV1
openedBodyCheckpoint fieldIndex preimage =
  psnd
    ( popenedFieldWalk
        # bodyOpeningT cbor (inlineCarriage preimage)
        # bodyAnchorT (txIdOfBody body)
        # pconstant fieldIndex
        # inputsT []
        # pdata (pconstant certificatePolicy)
    )
  where
    body = bodyCommitting fieldIndex preimage
    cbor = compactOfBody body (wsHashOf defaultWitnessSet) 3

openedCertifiedWitnessView :: forall s. Integer -> Term s PFieldViewV1
openedCertifiedWitnessView fieldIndex =
  popenedFieldView
    # witnessOpeningT compactCbor defaultWitnessSet (certifiedCarriageFor forgedWitnessChunks)
    # witnessAnchorT txId (wsHashOf defaultWitnessSet)
    # pconstant fieldIndex
    # inputsT (certifiedReferenceInputs fieldIndex)
    # pdata (pconstant certificatePolicy)

openedCertifiedWitnessWalkView :: forall s. Integer -> Term s PFieldViewV1
openedCertifiedWitnessWalkView fieldIndex =
  pfst
    $ popenedFieldWalk
      # witnessOpeningT compactCbor defaultWitnessSet (certifiedCarriageFor forgedWitnessChunks)
      # witnessAnchorT txId (wsHashOf defaultWitnessSet)
      # pconstant fieldIndex
      # inputsT (certifiedReferenceInputs fieldIndex)
      # pdata (pconstant certificatePolicy)

pfst :: forall a b s. Term s (PPair a b) -> Term s a
pfst pair = pmatch pair $ \(PPair a _) -> a

psnd :: forall a b s. Term s (PPair a b) -> Term s b
psnd pair = pmatch pair $ \(PPair _ b) -> b

--------------------------------------------------------------------------------
-- The three folds the tests use
--------------------------------------------------------------------------------

-- | Concatenate every item, which records both the contents and the order.
foldConcat ::
  forall s. Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1) -> Term s PByteString
foldConcat opened =
  pfoldOpenedField # opened # pconstant "" # plam (\acc _ item -> acc <> item)

-- | Sum the indices the step is handed.
foldIndices :: forall s. Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1) -> Term s PInteger
foldIndices opened =
  pfoldOpenedField # opened # 0 # plam (\acc index _ -> acc + index)

-- | Count the steps taken.
foldCount :: forall s. Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1) -> Term s PInteger
foldCount opened =
  pfoldOpenedField # opened # 0 # plam (\acc _ _ -> acc + 1)

--------------------------------------------------------------------------------
-- Terms for the module's own types
--------------------------------------------------------------------------------

bodyOpeningT ::
  forall s. BS.ByteString -> Term s PFieldCarriageV1 -> Term s PFieldOpeningV1
bodyOpeningT cbor carriage =
  pcon
    ( PBodyFieldOpening
        { pbodyOpening'nativeTxCompactCbor = pdata (pconstant cbor)
        , pbodyOpening'carriage = pdata carriage
        }
    )

witnessOpeningT ::
  forall s. BS.ByteString -> Ws -> Term s PFieldCarriageV1 -> Term s PFieldOpeningV1
witnessOpeningT cbor ws carriage =
  pcon
    ( PWitnessFieldOpening
        { pwitnessOpening'nativeTxCompactCbor = pdata (pconstant cbor)
        , pwitnessOpening'witnessSet = pdata (witnessSetT ws)
        , pwitnessOpening'carriage = pdata carriage
        }
    )

bodyTxOpeningT :: forall s. BS.ByteString -> Term s PNativeTxOpeningV1
bodyTxOpeningT cbor =
  pcon (PBodyTxOpening {pbodyTxOpening'nativeTxCompactCbor = pconstant cbor})

witnessTxOpeningT :: forall s. BS.ByteString -> Ws -> Term s PNativeTxOpeningV1
witnessTxOpeningT cbor ws =
  pcon
    ( PWitnessTxOpening
        { pwitnessTxOpening'nativeTxCompactCbor = pconstant cbor
        , pwitnessTxOpening'witnessSet = witnessSetT ws
        }
    )

bodyAnchorT :: forall s. BS.ByteString -> Term s PNativeTxAnchorV1
bodyAnchorT anchoredId = pcon (PBodyAnchor {pbodyAnchor'txId = pdata (pconstant anchoredId)})

witnessAnchorT :: forall s. BS.ByteString -> BS.ByteString -> Term s PNativeTxAnchorV1
witnessAnchorT anchoredId wsHash =
  pcon
    ( PWitnessAnchor
        { pwitnessAnchor'txId = pdata (pconstant anchoredId)
        , pwitnessAnchor'witnessSetHash = pdata (pconstant wsHash)
        }
    )

witnessSetT :: forall s. Ws -> Term s PNativeTxWitnessSetCompact
witnessSetT ws =
  pcon
    ( PNativeTxWitnessSetCompact
        { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant (wAddr ws))
        , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant (wScript ws))
        , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant (wRedeemer ws))
        }
    )

inlineCarriage :: forall s. BS.ByteString -> Term s PFieldCarriageV1
inlineCarriage preimage = pcon (PInline (pdata (pconstant preimage)))

-- | A tier-3 carriage naming reference inputs the fixtures never supply.
certifiedCarriage :: forall s. Term s PFieldCarriageV1
certifiedCarriage =
  pcon
    ( PCertified
        { pcertified'certRefInputIndex = pdata 0
        , pcertified'chunkRefInputIndices = pdata (pconstant [1 :: Integer])
        }
    )

certifiedBodyView :: forall s. Term s PFieldViewV1
certifiedBodyView = certifiedViewAt bodyHandle 5

certifiedViewAt :: forall s. Term s PAnchoredNativeTxV1 -> Integer -> Term s PFieldViewV1
certifiedViewAt anchored fieldIndex =
  panchoredFieldView
    # anchored
    # pconstant fieldIndex
    # certifiedCarriageFor forgedWitnessChunks
    # inputsT (certificateRefInput fieldIndex : map chunkRefInput forgedWitnessChunks)
    # pdata (pconstant certificatePolicy)

certifiedCarriageFor :: forall s. [BS.ByteString] -> Term s PFieldCarriageV1
certifiedCarriageFor chunks =
  pcon
    ( PCertified
        { pcertified'certRefInputIndex = pdata 0
        , pcertified'chunkRefInputIndices =
            pdata (pconstant [1 .. fromIntegral (length chunks) :: Integer])
        }
    )

certifiedReferenceInputs :: Integer -> [TxInInfo]
certifiedReferenceInputs fieldIndex =
  certificateRefInput fieldIndex : map chunkRefInput forgedWitnessChunks

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT = pconstant

pencodes :: forall a s. (PIsData a) => Term s a -> PD.Data -> Term s PBool
pencodes value expected = pforgetData (pdata value) #== pconstant expected

--------------------------------------------------------------------------------
-- The witness set
--------------------------------------------------------------------------------

data Ws = Ws
  { wAddr :: BS.ByteString
  , wScript :: BS.ByteString
  , wRedeemer :: BS.ByteString
  }

-- | The committed witness set. Slot 7 commits 'addressWitnessPreimage'.
defaultWitnessSet :: Ws
defaultWitnessSet =
  Ws
    { wAddr = blake2b256 addressWitnessPreimage
    , wScript = blake2b256 scriptWitnessPreimage
    , wRedeemer = blake2b256 redeemerPreimage
    }

{- | A witness set that still commits 'addressWitnessPreimage' at slot 7 but
re-derives to a different @witness_set_hash@ — so the preimage would pass its
own hash check and the door still refuses the set it came from.
-}
otherWitnessSet :: Ws
otherWitnessSet = defaultWitnessSet {wScript = hash32 0x22}

{- | The useful forgery: three commitments to the empty field. Under it every
§2.5 absence rule is true of every transaction.
-}
emptyWitnessSet :: Ws
emptyWitnessSet =
  Ws
    { wAddr = blake2b256 emptyPreimage
    , wScript = blake2b256 emptyPreimage
    , wRedeemer = blake2b256 emptyPreimage
    }

-- | @encode_native_tx_witness_set_compact@, written out from §2.5.
encodeWitnessSet :: Ws -> BS.ByteString
encodeWitnessSet w =
  BS.concat ["\x83", defBytes32 (wAddr w), defBytes32 (wScript w), defBytes32 (wRedeemer w)]

wsHashOf :: Ws -> BS.ByteString
wsHashOf = blake2b256 . encodeWitnessSet

-- | The same record as @Data@, so the wire tests do not read the port back.
witnessSetData :: Ws -> PD.Data
witnessSetData w = PD.Constr 0 [PD.B (wAddr w), PD.B (wScript w), PD.B (wRedeemer w)]

--------------------------------------------------------------------------------
-- The body's six hash slots
--------------------------------------------------------------------------------

-- | The body's six field commitments, as a record so a test can move one.
data Body = Body
  { bSpendInputs :: BS.ByteString
  , bReferenceInputs :: BS.ByteString
  , bOutputs :: BS.ByteString
  , bObservers :: BS.ByteString
  , bSigners :: BS.ByteString
  , bMint :: BS.ByteString
  }

{- | Six distinct commitments, so a transposed slot is a hash mismatch rather
than an accidental match.
-}
defaultBody :: Body
defaultBody =
  Body
    { bSpendInputs = blake2b256 spendInputPreimage
    , bReferenceInputs = hash32 0x02
    , bOutputs = blake2b256 variablePreimage
    , bObservers = blake2b256 observersPreimage
    , bSigners = blake2b256 signersPreimage
    , bMint = blake2b256 mintPreimage
    }

-- | Slot 3 commits the observers preimage and slot 4 does not.
onlyObserverSlot :: Body
onlyObserverSlot = defaultBody {bSigners = hash32 0x05}

-- | 'defaultBody' with slot @fieldIndex@ set to this preimage's commitment.
bodyCommitting :: Integer -> BS.ByteString -> Body
bodyCommitting fieldIndex preimage = case fieldIndex of
  0 -> defaultBody {bSpendInputs = commitment}
  1 -> defaultBody {bReferenceInputs = commitment}
  2 -> defaultBody {bOutputs = commitment}
  3 -> defaultBody {bObservers = commitment}
  4 -> defaultBody {bSigners = commitment}
  5 -> defaultBody {bMint = commitment}
  _ -> defaultBody
  where
    commitment = blake2b256 preimage

--------------------------------------------------------------------------------
-- Reference compact CBOR (§2.5) and the §3 transaction id
--------------------------------------------------------------------------------

{- | The compact body: an array of twelve, six field commitments interleaved
with the scalars §2.5 lists between them.
-}
compactBody :: Body -> BS.ByteString
compactBody b =
  BS.concat
    [ "\x8c"
    , defBytes32 (bSpendInputs b)
    , defBytes32 (bReferenceInputs b)
    , defBytes32 (bOutputs b)
    , cborInt 1_000_000
    , cborInt 0
    , cborInt 65536
    , defBytes32 (bObservers b)
    , defBytes32 (bSigners b)
    , defBytes32 (bMint b)
    , defBytes32 (hash32 0x07)
    , defBytes32 (hash32 0x08)
    , cborInt 1
    ]

{- | The compact structure: version, body, witness-set hash, validity code.
§3's id preimage is the /body/ alone, which is what makes the last two the
prover's to choose.
-}
compactOfBody :: Body -> BS.ByteString -> Integer -> BS.ByteString
compactOfBody b wsHash validityCode =
  BS.concat ["\x84", cborInt 1, compactBody b, defBytes32 wsHash, cborInt validityCode]

-- | §3: @blake2b_256("MidgardNativeTxBodyV1" ‖ version ‖ body_cbor)@.
txIdOfBody :: Body -> BS.ByteString
txIdOfBody b = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> compactBody b)

-- | The transaction every test anchors against.
compactCbor :: BS.ByteString
compactCbor = compactOfBody defaultBody (wsHashOf defaultWitnessSet) 3

-- | The same body under a different fee, so it is a different transaction.
otherCompactCbor :: BS.ByteString
otherCompactCbor =
  BS.concat
    [ "\x84"
    , cborInt 1
    , otherBodyCbor
    , defBytes32 (wsHashOf defaultWitnessSet)
    , cborInt 3
    ]

otherBodyCbor :: BS.ByteString
otherBodyCbor =
  BS.concat
    [ "\x8c"
    , defBytes32 (bSpendInputs defaultBody)
    , defBytes32 (bReferenceInputs defaultBody)
    , defBytes32 (bOutputs defaultBody)
    , cborInt 999_999
    , cborInt 0
    , cborInt 65536
    , defBytes32 (bObservers defaultBody)
    , defBytes32 (bSigners defaultBody)
    , defBytes32 (bMint defaultBody)
    , defBytes32 (hash32 0x07)
    , defBytes32 (hash32 0x08)
    , cborInt 1
    ]

-- | 'compactCbor' with the witness-set hash slot replaced.
compactWith :: BS.ByteString -> BS.ByteString
compactWith wsHash = compactOfBody defaultBody wsHash 3

-- | 'compactCbor' with the trailing validity code replaced. Same id.
compactCodeOf :: Integer -> BS.ByteString
compactCodeOf code = compactOfBody defaultBody (wsHashOf defaultWitnessSet) code

txId, otherTxId :: BS.ByteString
txId = txIdOfBody defaultBody
otherTxId = blake2b256 "not this transaction"

--------------------------------------------------------------------------------
-- Reference preimages (§5.1)
--------------------------------------------------------------------------------

-- | A definite array header followed by one definite byte string per item.
fieldPreimage :: [BS.ByteString] -> BS.ByteString
fieldPreimage items = arrayHeader (length items) <> BS.concat (map wrapItem items)

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (0x80 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | n <= 65535 = BS.pack [0x99, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
  | otherwise = error "arrayHeader: out of fixture range"

wrapItem :: BS.ByteString -> BS.ByteString
wrapItem bytes
  | n <= 23 = BS.cons (fromIntegral (0x40 + n)) bytes
  | n <= 255 = BS.pack [0x58, fromIntegral n] <> bytes
  | n <= 65535 = BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)] <> bytes
  | otherwise = error "wrapItem: out of fixture range"
  where
    n = BS.length bytes

emptyPreimage :: BS.ByteString
emptyPreimage = fieldPreimage []

hash28 :: Int -> BS.ByteString
hash28 fill = BS.replicate 28 (fromIntegral fill)

-- | §5.3 field 3: three 28-byte items, stride 30.
observersPreimage :: BS.ByteString
observersPreimage = fieldPreimage [hash28 0x41, hash28 0x42, hash28 0x43]

-- | Two items, so a preimage offered at slot 4 is distinguishable by its count.
signersPreimage :: BS.ByteString
signersPreimage = fieldPreimage [hash28 0x51, hash28 0x52]

-- | §5.3 fields 0 and 1: @82 ‖ 58 20 tx_id ‖ 19 index_be16@, stride 40.
spendInputPreimage :: BS.ByteString
spendInputPreimage = fieldPreimage spendInputItems

spendInputItems :: [BS.ByteString]
spendInputItems =
  [ BS.concat ["\x82", defBytes32 (BS.replicate 32 (fromIntegral (0x81 + i))), "\x19", BS.pack [0, fromIntegral i]]
  | i <- [0 .. 2 :: Int]
  ]

-- | §5.3 field 7: @82 ‖ 58 20 vkey ‖ 58 40 signature@, 101 bytes, stride 103.
addressWitnessItem :: BS.ByteString
addressWitnessItem =
  BS.concat ["\x82", defBytes32 (BS.replicate 32 0x61), "\x58\x40" <> BS.replicate 64 0x62]

addressWitnessPreimage :: BS.ByteString
addressWitnessPreimage = fieldPreimage [addressWitnessItem]

-- The exact Aiken tier-3 fixture: 256 address-witness-shaped items cross K
-- once, producing two independently authenticated reference-input chunks.
forgedWitnessPreimage :: BS.ByteString
forgedWitnessPreimage = fieldPreimage (replicate 256 addressWitnessItem)

forgedWitnessChunks :: [BS.ByteString]
forgedWitnessChunks = chunksOf forgedWitnessPreimage

-- | Variable-width fields: three items of three different widths.
variableItems :: [BS.ByteString]
variableItems = [BS.replicate 4 0x71, BS.replicate 30 0x72, BS.replicate 60 0x73]

variablePreimage :: BS.ByteString
variablePreimage = fieldPreimage variableItems

scriptWitnessPreimage, redeemerPreimage, mintPreimage :: BS.ByteString
scriptWitnessPreimage = fieldPreimage [BS.replicate 20 0x81, BS.replicate 40 0x82]
redeemerPreimage = fieldPreimage [BS.replicate 12 0x91]
mintPreimage = fieldPreimage [BS.replicate 33 0xa1]

--------------------------------------------------------------------------------
-- Reference CBOR and hashing
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 h = "\x58\x20" <> h

hash32 :: Int -> BS.ByteString
hash32 n = blake2b256 (BS.pack [fromIntegral n])

-- | Canonical CBOR for a signed integer, minimal width.
cborInt :: Integer -> BS.ByteString
cborInt n
  | n < 0 = tagged 0x20 (negate n - 1)
  | otherwise = tagged 0x00 n
  where
    tagged major v
      | v <= 23 = BS.pack [fromIntegral (major + v)]
      | v <= 0xff = BS.pack [fromIntegral (major + 24), fromIntegral v]
      | v <= 0xffff =
          BS.pack [fromIntegral (major + 25), fromIntegral (v `div` 256), fromIntegral (v `mod` 256)]
      | v <= 0xffff_ffff =
          BS.pack (fromIntegral (major + 26) : beBytes 4 v)
      | otherwise = BS.pack (fromIntegral (major + 27) : beBytes 8 v)
    beBytes width v = [fromIntegral (v `div` (256 ^ k) `mod` 256) | k <- [width - 1, width - 2 .. 0 :: Int]]

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

certificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x91))

certificateRefInput :: Integer -> TxInInfo
certificateRefInput fieldIndex =
  TxInInfo
    (outRefN 0)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin certificateScript)))
        ( adaValue 2_000_000
            <> singleton
              certificatePolicy
              (TokenName (toBuiltin (referenceAssetName txId fieldIndex)))
              1
        )
        (OutputDatum (Datum (dataToBuiltinData certificateDatum)))
        Nothing
    )
  where
    certificateDatum =
      PD.Constr
        0
        [ PD.B (BS.replicate 28 0x31)
        , PD.B txId
        , PD.I fieldIndex
        , PD.I (fromIntegral (BS.length forgedWitnessPreimage))
        , PD.List (map (PD.B . blake2b256) forgedWitnessChunks)
        ]

chunkRefInput :: BS.ByteString -> TxInInfo
chunkRefInput bytes =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin carriageScript)))
        (adaValue 2_000_000)
        (OutputDatum (Datum (dataToBuiltinData (PD.B bytes))))
        Nothing
    )

referenceAssetName :: BS.ByteString -> Integer -> BS.ByteString
referenceAssetName tid index = blake2b256 (BS.cons (fromIntegral index) tid)

chunksOf :: BS.ByteString -> [BS.ByteString]
chunksOf bytes
  | BS.null bytes = []
  | BS.length bytes <= 15_900 = [bytes]
  | otherwise = BS.take 15_900 bytes : chunksOf (BS.drop 15_900 bytes)

adaValue :: Integer -> Value
adaValue = singleton (CurrencySymbol "") (TokenName "")

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01)))

certificateScript, carriageScript :: BS.ByteString
certificateScript = BS.replicate 28 0x93
carriageScript = BS.replicate 28 0x94
