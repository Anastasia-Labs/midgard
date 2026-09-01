{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxMachineWalk
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/native-tx-machine-walk-v1.ak@ — the §10 resumable
              walk.

The walk core turns an authenticated field view into a /position/ that survives
across transactions. Three properties are what these tests are organised
around, plus the guards that make a resumed position trustworthy.

__Positions, not bytes__ (§7.6). The checkpoint's wire form is 53 bytes
/whatever the field holds/. Two tests state that as a property rather than as a
restatement of the encoder: one encodes checkpoints taken over four different
fields under two carriage tiers at several positions and asserts a constant
length, and one walks two /different/ preimages of the same shape to the same
position and asserts the two serialisations are byte-identical. A structure
smuggling preimage content could not pass either.

__Authenticate-once, and resumption without re-paying__ (§7.1, §10.4). A fold
with a budget stops where the budget runs out, and resuming from the checkpoint
it returns finishes the field — visiting each item exactly once across the two
halves, which is what the fold's accumulated result is checked against.

__A resumed position is bound to the bytes it came from__ (§10.2). The
checkpoint type is exported without its constructor and the decoder is private,
exactly as the Aiken original is opaque, so these tests plant positions the only
way a caller can: by writing the 53 wire bytes, hashing them, and going through
'presumeFieldWalkFromCommitment'. Each negative case below moves one scalar of
an otherwise honest checkpoint.

Four of the case names are the Aiken tree's own, and one fixture is its vector
verbatim — @8244aa41ccdd43eeff99@, the ten-byte field-6 preimage that isolates
the final-advance rule. Reproducing it is the point: it is the one guard whose
refusal cannot be reached from an honestly opened walk, only from a position
planted through the commitment, and constructing that situation independently
would risk testing a different thing under the same name.

The reference encoder is written from §10.3's layout rather than from the port.
-}
module Testing.NativeTxMachineWalk (tests) where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
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

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PTxInInfo)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (pbyteAt)
import Midgard.FraudProofs.NativeTx.Compact qualified as Compact
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardTxInput (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1 (..),
  PFieldViewV1,
  pfieldItemExtent,
  pfieldItemCount,
  pmaximumCardanoSpendRedeemerCount,
  pspendInputItemBytes,
  pspendInputStride,
 )
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  pencodeFieldWalkCheckpoint,
  pfieldWalkCheckpointBytes,
  pfieldWalkCheckpointHash,
  popenFieldWalk,
  presumeFieldWalkFromCommitment,
  pspendInputAt,
  pspendInputCount,
  pwalkFieldIndex,
  pwalkFold,
  pwalkIsComplete,
  pwalkNext,
  pwalkNextItemIndex,
  pwalkRemaining,
  pwalkSkip,
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
    "Native Tx Machine Walk Tests"
    [ testGroup "§7.1 authenticate-once measurements" authenticateOnceTests
    , testGroup "§10.3 the checkpoint wire form" wireTests
    , testGroup "§10.1 opening" openTests
    , testGroup "§10.4 advancing" advanceTests
    , testGroup "§10.2 resuming through the commitment" resumeTests
    , testGroup "§10.4 the step's own guards" stepGuardTests
    , testGroup "§10.5 the spend-input shortcut" spendInputTests
    ]

--------------------------------------------------------------------------------
-- §7.1 authenticate once, measured
--------------------------------------------------------------------------------

authenticateOnceTests :: [TestTree]
authenticateOnceTests =
  [ testCase "authenticate_once_one_open_one_read" $
      passertEval authenticateOnceOneOpenOneRead
  , testCase "authenticate_once_one_open_every_read" $
      passertEval authenticateOnceOneOpenEveryRead
  , testCase "authenticate_once_reopen_per_item_costs_more" $
      passertEval authenticateOnceReopenPerItem
  , testCase "authenticate_once_one_open_every_relocation" $
      passertEval authenticateOnceOneOpenEveryRelocation
  , testCase "authenticate_once_holds_under_tier_two" $
      passertEval authenticateOnceHoldsUnderTierTwo
  , testCase "authenticate_once_holds_under_tier_three" $
      passertEval authenticateOnceHoldsUnderTierThree
  ]

authenticateOnceOneOpenOneRead :: forall s. Term s PBool
authenticateOnceOneOpenOneRead =
  pmatch aikenAuthenticateSource $ \(PPair verified witnessSet) ->
  pmatch (aikenAuthenticateOpenFrom verified witnessSet) $ \(PPair view start) ->
  pmatch (pwalkNext # view # start) $ \(PPair item next) ->
    item #== pconstant (aikenInputItem 0x44 0)
      #&& pwalkRemaining # next #== 63

authenticateOnceOneOpenEveryRead :: forall s. Term s PBool
authenticateOnceOneOpenEveryRead =
  pmatch aikenAuthenticateSource $ \(PPair verified witnessSet) ->
  pmatch (aikenAuthenticateOpenFrom verified witnessSet) $ \(PPair view start) ->
  pmatch (walkFoldAiken view start 64 0) $ \(PPair total done) ->
    pwalkIsComplete # done
      #&& total #== (expectedAuthenticateTally # 0 # 0 # 64)

authenticateOnceReopenPerItem :: forall s. Term s PBool
authenticateOnceReopenPerItem =
  pmatch aikenAuthenticateSource $ \(PPair verified witnessSet) ->
  plet
    ( pfix $ \self -> plam $ \index state ->
        pif
          (index #>= 64)
          state
          ( pmatch (aikenAuthenticateOpenFrom verified witnessSet) $ \(PPair view start) ->
            plet (pwalkSkip # view # start # index) $ \checkpoint ->
            pmatch (pwalkNext # view # checkpoint) $ \(PPair item _next) ->
              self # (index + 1) # (aikenWalkTally state index item)
          )
    )
    $ \reopenEach ->
      reopenEach # 0 # 0 #== (expectedAuthenticateTally # 0 # 0 # 64)

authenticateOnceOneOpenEveryRelocation :: forall s. Term s PBool
authenticateOnceOneOpenEveryRelocation =
  pmatch aikenAuthenticateSource $ \(PPair verified witnessSet) ->
  pmatch (aikenAuthenticateOpenFrom verified witnessSet) $ \(PPair view start) ->
  plet
    ( pfix $ \self -> plam $ \index state ->
        pif
          (index #>= 64)
          state
          ( plet (pwalkSkip # view # start # index) $ \checkpoint ->
            pmatch (pwalkNext # view # checkpoint) $ \(PPair item _next) ->
              self # (index + 1) # (aikenWalkTally state index item)
          )
    )
    $ \relocateEach ->
      relocateEach # 0 # 0 #== (expectedAuthenticateTally # 0 # 0 # 64)

authenticateOnceHoldsUnderTierTwo :: forall s. Term s PBool
authenticateOnceHoldsUnderTierTwo =
  pmatch aikenAuthenticateSource $ \(PPair verified witnessSet) ->
  pmatch
    ( popenFieldWalk
        # verified # witnessSet # 0
        # pcon (PRawUtxo (pdata 0))
        # inputsT aikenTierTwoReferenceInputs
        # pdata (pconstant aikenCertificatePolicy)
    )
    $ \(PPair view start) ->
  pmatch (walkFoldAiken view start 64 0) $ \(PPair total done) ->
    pwalkIsComplete # done
      #&& total #== (expectedAuthenticateTally # 0 # 0 # 64)

authenticateOnceHoldsUnderTierThree :: forall s. Term s PBool
authenticateOnceHoldsUnderTierThree =
  pmatch (aikenInputSource aikenTierThreePreimage) $ \(PPair verified witnessSet) ->
  pmatch
    ( popenFieldWalk
        # verified # witnessSet # 0
        # pcon
          ( PCertified
              { pcertified'certRefInputIndex = pdata 0
              , pcertified'chunkRefInputIndices = pdata (pconstant [1, 2 :: Integer])
              }
          )
        # inputsT aikenTierThreeReferenceInputs
        # pdata (pconstant aikenCertificatePolicy)
    )
    $ \(PPair view start) ->
  plet (pwalkSkip # view # start # (pconstant aikenTierThreeItemCount - 1)) $ \checkpoint ->
  pmatch (pwalkNext # view # checkpoint) $ \(PPair item done) ->
    pand'List
      [ (plengthBS # pconstant aikenTierThreePreimage) #> pconstant (fromIntegral chunkBytesK :: Integer)
      , (pspendInputCount # view) #== pconstant aikenTierThreeItemCount
      , item #== pconstant (aikenInputItem 0x44 0)
      , pwalkIsComplete # done
      ]

expectedAuthenticateTally :: forall s.
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
expectedAuthenticateTally = phoistAcyclic $
  pfix $ \self -> plam $ \state index remaining ->
    pif
      (remaining #<= 0)
      state
      ( plet
          ( pencodeMidgardTxInput
              # pcon
                ( PMidgardTxInput
                    (pdata $ pconstant (BS.replicate 32 0x44))
                    (pdata index)
                )
          )
          $ \item ->
            self
              # aikenWalkTally state index item
              # (index + 1)
              # (remaining - 1)
      )

aikenWalkTally :: forall s.
  Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger
aikenWalkTally state index item =
  state * 31 + (index + 1) * (plengthBS # item) + (pbyteAt # item # 0)

aikenAuthenticateSource :: forall s.
  Term s (PPair PVerifiedMidgardNativeTxCompact PNativeTxWitnessSetCompact)
aikenAuthenticateSource = aikenInputSource (fPreimage aikenAuthenticateField)

aikenInputSource :: forall s.
  BS.ByteString ->
  Term s (PPair PVerifiedMidgardNativeTxCompact PNativeTxWitnessSetCompact)
aikenInputSource preimage =
  plet (pconstant preimage) $ \preimageCbor ->
  plet (pblake2b_256 # preimageCbor) $ \fieldCommitment ->
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata aikenZero32) (pdata aikenZero32) (pdata aikenZero32)
    )
    $ \witnessSet ->
  plet (Compact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        fieldCommitment aikenZero32 aikenZero32 0 (-1) (-1)
        aikenZero32 aikenZero32 aikenZero32 aikenZero32 aikenZero32 255
    )
    $ \body ->
  plet
    (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0)
    $ \compact ->
      pcon $ PPair
        ( pcon $ PVerifiedMidgardNativeTxCompact
            (pconstant aikenSampleTxId) 1 compact
        )
        witnessSet

aikenAuthenticateOpenFrom :: forall s.
  Term s PVerifiedMidgardNativeTxCompact ->
  Term s PNativeTxWitnessSetCompact ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
aikenAuthenticateOpenFrom verified witnessSet =
  aikenInputOpenFrom (fPreimage aikenAuthenticateField) verified witnessSet

aikenInputOpenFrom :: forall s.
  BS.ByteString ->
  Term s PVerifiedMidgardNativeTxCompact ->
  Term s PNativeTxWitnessSetCompact ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
aikenInputOpenFrom preimage verified witnessSet =
  popenFieldWalk
    # verified # witnessSet # 0
    # pcon (PInline (pdata $ pconstant preimage))
    # pnil # pdata (pconstant aikenCertificatePolicy)

aikenZero32 :: forall s. Term s PByteString
aikenZero32 = pconstant $ BS.replicate 32 0

aikenCertificatePolicy :: CurrencySymbol
aikenCertificatePolicy = CurrencySymbol (toBuiltin aikenCertificatePolicyBytes)

--------------------------------------------------------------------------------
-- §10.3 the wire form
--------------------------------------------------------------------------------

wireTests :: [TestTree]
wireTests =
  [ testCase "no_preimage_bytes_reach_the_checkpoint" $
      passertEval noPreimageBytesReachFixedCheckpoint
  , testCase "no_preimage_bytes_reach_a_variable_width_checkpoint" $
      passertEval noPreimageBytesReachVariableCheckpoint
  , -- The property "positions, not bytes" is checkable exactly because this
    -- length is constant. Four fields, two carriage tiers, several positions.
    testCase "the wire form is 53 bytes whatever the field holds" $
      passertEval $
        pand'List
          [ (plengthBS # (pencodeFieldWalkCheckpoint # checkpoint))
            #== pfieldWalkCheckpointBytes
          | checkpoint <- assortedCheckpoints
          ]
  , testCase "checkpoint_wire_form_is_constant_at_the_spend_input_maximum" $
      passertEval maximumSpendCheckpointWidth
  , testCase "matches the reference encoding of §10.3's layout" $
      passertEval $
        (pencodeFieldWalkCheckpoint # startOf hash28Field)
          #== pconstant (referenceCheckpoint txId 3 91 3 0 1)
  , testCase "encodes an advanced position" $
      passertEval $
        (pencodeFieldWalkCheckpoint #$ advancedBy 2 hash28Field)
          #== pconstant (referenceCheckpoint txId 3 91 3 2 61)
  , -- The §7.6 statement, made falsifiable: same field, same shape, same
    -- position, entirely different preimage bytes — and one serialisation.
    testCase "two walks over different preimages serialise identically" $
      passertEval $
        (pencodeFieldWalkCheckpoint #$ advancedBy 1 hash28Field)
          #== (pencodeFieldWalkCheckpoint #$ advancedBy 1 otherHash28Field)
  , testCase "the commitment is blake2b_256 of the domain and the wire form" $
      passertEval $
        (pfieldWalkCheckpointHash # startOf hash28Field)
          #== pconstant (referenceHash (referenceCheckpoint txId 3 91 3 0 1))
  , -- New surface: the commitment is domain-separated with an ASCII string of
    -- its own, and reuses none of §4's prohibited counted-scheme domains. A
    -- bare hash of the wire form is what it must *not* be.
    testCase "the commitment is domain-separated, not a bare hash of the wire form" $
      passertEval $
        pnot
          #$ (pfieldWalkCheckpointHash # startOf hash28Field)
          #== pconstant (blake2b256 (referenceCheckpoint txId 3 91 3 0 1))
  ]

noPreimageBytesReachFixedCheckpoint :: forall s. Term s PBool
noPreimageBytesReachFixedCheckpoint =
  pmatch
    (walkFoldAiken (viewOf aikenDisjointInputFieldA) (startOf aikenDisjointInputFieldA) 5 0)
    $ \(PPair _ pausedA) ->
  pmatch
    (walkFoldAiken (viewOf aikenDisjointInputFieldB) (startOf aikenDisjointInputFieldB) 5 0)
    $ \(PPair _ pausedB) ->
    pand'List
      [ pconstant inputItemsAreContentDisjoint
      , (plengthBS # pconstant (fPreimage aikenDisjointInputFieldA))
          #== (plengthBS # pconstant (fPreimage aikenDisjointInputFieldB))
      , (pencodeFieldWalkCheckpoint # pausedA)
          #== (pencodeFieldWalkCheckpoint # pausedB)
      , (pfieldWalkCheckpointHash # pausedA)
          #== (pfieldWalkCheckpointHash # pausedB)
      ]

noPreimageBytesReachVariableCheckpoint :: forall s. Term s PBool
noPreimageBytesReachVariableCheckpoint =
  pmatch (foldAikenWalk (startOf aikenWalkField) 11 0) $ \(PPair _ pausedA) ->
  pmatch
    (walkFoldAiken (viewOf aikenOtherWalkField) (startOf aikenOtherWalkField) 11 0)
    $ \(PPair _ pausedB) ->
    pand'List
      [ pconstant scriptItemsAreContentDisjoint
      , (plengthBS # pconstant (fPreimage aikenWalkField))
          #== (plengthBS # pconstant (fPreimage aikenOtherWalkField))
      , (pencodeFieldWalkCheckpoint # pausedA)
          #== (pencodeFieldWalkCheckpoint # pausedB)
      ]

-- | Checkpoints from four fields under two tiers, at assorted positions.
assortedCheckpoints :: forall s. [Term s PFieldWalkCheckpointV1]
assortedCheckpoints =
  [ startOf hash28Field
  , advancedBy 3 hash28Field
  , startOf spendField
  , advancedBy 1 spendField
  , startOf variableField
  , advancedBy 2 variableField
  , startOf emptyField
  , startOf bigCertifiedField
  , -- Deep into a two-chunk tier-3 field. Reached by an arithmetic skip rather
    -- than by 599 steps: each step of a chunked walk re-verifies the chunk it
    -- lands in, so stepping there would cost 599 hashes over 15,900 bytes to
    -- assert something about a 53-byte encoding.
    pwalkSkip # viewOf bigCertifiedField # startOf bigCertifiedField # 599
  ]

--------------------------------------------------------------------------------
-- §10.1 opening
--------------------------------------------------------------------------------

openTests :: [TestTree]
openTests =
  [ -- The opening position is derived from the authenticated view, not
    -- supplied: index 0, offset at the end of the §5.1 header, shape from the
    -- view itself.
    testCase "derives the opening position from the authenticated view" $
      passertEval $
        pand'List
          [ (pwalkNextItemIndex # startOf hash28Field) #== 0
          , (pwalkRemaining # startOf hash28Field) #== 3
          , (pwalkFieldIndex # startOf hash28Field) #== 3
          , (pwalkTxId # startOf hash28Field) #== pconstant txId
          , pnot #$ pwalkIsComplete # startOf hash28Field
          ]
  , testCase "carries the field index the walk was opened on" $
      passertEval $ (pwalkFieldIndex # startOf variableField) #== 6
  , -- An empty field is complete the moment it opens: §5.1 leaves no trailing
    -- bytes, so its one admissible offset is already `total_length`.
    testCase "an empty field opens complete" $
      passertEval $
        pand'List
          [ pwalkIsComplete # startOf emptyField
          , (pwalkRemaining # startOf emptyField) #== 0
          ]
  , -- A variable-width field under tier-3 carriage has no authenticated item
    -- count, and the walk does not work around it.
    testCase "aborts opening a variable-width field under tier 3" $
      pfails $ pwalkNextItemIndex #$ startOf bigVariableCertifiedField
  , testCase "opens a fixed-stride field under tier 3" $
      passertEval $ (pwalkRemaining # startOf bigCertifiedField) #== 600
  ]

--------------------------------------------------------------------------------
-- §10.4 advancing
--------------------------------------------------------------------------------

advanceTests :: [TestTree]
advanceTests =
  [ testCase "steps through a fixed-stride field item by item" $
      passertEval $
        pand'List
          [ (itemAt hash28Field i) #== pconstant (hash28Items !! fromIntegral i)
          | i <- [0 .. 2]
          ]
  , testCase "steps through a variable-width field item by item" $
      passertEval $
        pand'List
          [ (itemAt variableField i) #== pconstant (variableItems !! fromIntegral i)
          | i <- [0 .. 1]
          ]
  , testCase "the last advance lands complete" $
      passertEval $ pwalkIsComplete #$ advancedBy 3 hash28Field
  , testCase "aborts on a step past the last item" $
      pfails $ advancedBy 4 hash28Field
  , -- The budget is what makes a walk interruptible.
    testCase "a zero budget returns the state and the position unchanged" $
      passertEval $
        pmatch (foldFrom hash28Field (startOf hash28Field) 0) $ \(PPair total checkpoint) ->
          pand'List [total #== 0, (pwalkNextItemIndex # checkpoint) #== 0]
  , testCase "a partial budget stops where the budget runs out" $
      passertEval $
        pmatch (foldFrom hash28Field (startOf hash28Field) 2) $ \(PPair total checkpoint) ->
          pand'List
            [ total #== pconstant (foldReference [0, 1])
            , (pwalkNextItemIndex # checkpoint) #== 2
            , pnot #$ pwalkIsComplete # checkpoint
            ]
  , -- AC2: the second half re-pays none of the first half's items, and between
    -- them every item is visited exactly once.
    testCase "resuming from a partial fold finishes the field without repeating" $
      passertEval $
        pmatch (foldFrom hash28Field (startOf hash28Field) 2) $ \(PPair firstHalf checkpoint) ->
          pmatch (foldFrom hash28Field checkpoint 10) $ \(PPair secondHalf final) ->
            pand'List
              [ (firstHalf + secondHalf) #== pconstant (foldReference [0, 1, 2])
              , pwalkIsComplete # final
              ]
  , testCase "a budget beyond the item count stops at the end rather than aborting" $
      passertEval $
        pmatch (foldFrom hash28Field (startOf hash28Field) 99) $ \(PPair total checkpoint) ->
          pand'List
            [ total #== pconstant (foldReference [0, 1, 2])
            , pwalkIsComplete # checkpoint
            ]
  , testCase "aborts on a negative budget" $
      pfails $ foldFrom hash28Field (startOf hash28Field) (-1)
  , -- §10.4: for a fixed-stride field relocation is one multiplication, and it
    -- has to land exactly where walking would.
    testCase "an arithmetic skip lands where walking lands" $
      passertEval $
        (pencodeFieldWalkCheckpoint #$ pwalkSkip # viewOf hash28Field # startOf hash28Field # 2)
          #== (pencodeFieldWalkCheckpoint #$ advancedBy 2 hash28Field)
  , testCase "a walked skip lands where walking lands" $
      passertEval $
        (pencodeFieldWalkCheckpoint #$ pwalkSkip # viewOf variableField # startOf variableField # 2)
          #== (pencodeFieldWalkCheckpoint #$ advancedBy 2 variableField)
  , testCase "variable_width_skip_to_index_1" $
      passertEval $ variableWidthSkipTo 1
  , testCase "variable_width_skip_to_index_19" $
      passertEval $ variableWidthSkipTo 19
  , testCase "a zero skip is the identity" $
      passertEval $
        (pencodeFieldWalkCheckpoint #$ pwalkSkip # viewOf hash28Field # startOf hash28Field # 0)
          #== (pencodeFieldWalkCheckpoint # startOf hash28Field)
  , testCase "aborts on a skip past the last item" $
      pfails $ pwalkSkip # viewOf hash28Field # startOf hash28Field # 4
  , testCase "aborts on a negative skip" $
      pfails $ pwalkSkip # viewOf hash28Field # startOf hash28Field # (-1)
  ]

variableWidthSkipTo :: forall s. Integer -> Term s PBool
variableWidthSkipTo index =
  pmatch
    (pwalkNext # viewOf aikenWalkField # (pwalkSkip # viewOf aikenWalkField # startOf aikenWalkField # pconstant index))
    $ \(PPair item _next) -> item #== pconstant (aikenWalkItems !! fromIntegral index)

-- | The fold under test: sum of @index * 256 + first payload byte@.
foldReference :: [Int] -> Integer
foldReference indices =
  sum
    [ fromIntegral i * 256 + fromIntegral (BS.head (hash28Items !! i))
    | i <- indices
    ]

foldFrom ::
  forall s.
  Field ->
  Term s PFieldWalkCheckpointV1 ->
  Integer ->
  Term s (PPair PInteger PFieldWalkCheckpointV1)
foldFrom field checkpoint budget =
  pwalkFold
    # viewOf field
    # checkpoint
    # pconstant budget
    # 0
    # plam (\state index item -> state + index * 256 + (pbyteAt # item # 0))

--------------------------------------------------------------------------------
-- §10.2 resuming
--------------------------------------------------------------------------------

{- | Every case here plants a position the only way a caller can: 53 wire bytes,
their digest, and 'presumeFieldWalkFromCommitment'. The accepting case is the
control — each rejection below is the same bytes with one scalar moved.
-}
resumeTests :: [TestTree]
resumeTests =
  [ testCase "resume_finishes_an_interrupted_walk_across_a_carriage_change" $
      passertEval resumeFinishesAcrossCarriageChange
  , testCase "resume_survives_two_interruptions" $
      passertEval resumeSurvivesTwoInterruptions
  , testCase "resume_does_not_re_read_completed_items" $
      passertEval resumeDoesNotRereadCompletedItems
  , testCase "resume_works_on_a_fixed_stride_field" $
      passertEval resumeWorksOnFixedStrideField
  , testCase "accepts a genuine mid-walk position" $
      passertEval $
        pmatch (resume hash28Field honestMidWalk) $ \(PPair _view checkpoint) ->
          pand'List
            [ (pwalkNextItemIndex # checkpoint) #== 1
            , (pwalkRemaining # checkpoint) #== 2
            ]
  , testCase "the resumed walk reads the item the position points at" $
      passertEval $
        pmatch (resume hash28Field honestMidWalk) $ \(PPair view checkpoint) ->
          pmatch (pwalkNext # view # checkpoint) $ \(PPair item _advanced) ->
            item #== pconstant (hash28Items !! 1)
  , -- §10.6: the digest is what pins the position, so bytes that do not hash to
    -- the thread's commitment are not a position at all.
    testCase "rejects wire bytes that do not hash to the commitment" $
      pfails $ resumeWithCommitment hash28Field honestMidWalk (BS.replicate 32 0xff)
  , testCase "rejects wire bytes of the wrong length" $
      pfails $ resume hash28Field (BS.take 52 honestMidWalk)
  , -- §6.1: re-encoding the decoded value and demanding the input back is both
    -- the canonicity check and the range check.
    testCase "rejects a non-canonical wire form" $
      pfails $ resume hash28Field (patchByte 0 0x87 honestMidWalk)
  , testCase "rejects a wire form whose scalar wrappers are wrong" $
      pfails $ resume hash28Field (patchByte 37 0x44 honestMidWalk)
  , -- Identity: a checkpoint cannot be replayed against another transaction or
    -- pointed at another of §2.5's nine slots.
    testCase "rejects a checkpoint for another transaction" $
      pfails $ resume hash28Field (patchTxId otherTxId honestMidWalk)
  , testCase "rejects a checkpoint naming another field index" $
      pfails $ resume hash28Field (patchScalar wireFieldIndexAt 1 4 honestMidWalk)
  , -- Shape: a checkpoint from a differently-shaped carriage of the same field
    -- cannot be resumed.
    testCase "rejects a total length that is not the view's" $
      pfails $ resume hash28Field (patchScalar wireTotalLengthAt 3 92 honestMidWalk)
  , testCase "rejects an item count that is not the view's" $
      pfails $ resume hash28Field (patchScalar wireItemCountAt 3 4 honestMidWalk)
  , -- §10.2: on a fixed-stride field the offset is a function of the index
    -- alone, so a forged one cannot survive.
    testCase "resume_rejects_a_forged_fixed_stride_offset" $
      pfails $ resume hash28Field (patchScalar wireNextOffsetAt 3 32 honestMidWalk)
  , -- The live one of the four bounds. The header of this field is two bytes
    -- and its second byte is itself a decodable §5.1 head, so a position aimed
    -- inside the array header would otherwise find something to read.
    testCase "resume_rejects_an_offset_inside_the_array_header" $
      pfails $ resume headerAliasField headerAliasForged
  , -- §5.1 leaves no trailing bytes, so a finished walk has exactly one
    -- admissible offset. This branch reads no bytes at all.
    testCase "resume_rejects_a_walk_that_declares_itself_finished_early" $
      pfails $ resume hash28Field (patchScalar wireNextOffsetAt 3 61 finishedWalk)
  , -- The O(1) half of what a variable-width position can be held to: the head
    -- at the offset must decode and its item must end inside the field.
    testCase "resume_rejects_a_position_whose_item_runs_past_the_field" $
      pfails $ resume overrunField overrunForged
  , testCase "accepts the same variable-width position at an honest offset" $
      passertEval $
        pmatch (resume overrunField (walkBytes overrunField 1)) $ \(PPair _view checkpoint) ->
          (pwalkNextItemIndex # checkpoint) #== 1
  ]

resumeFinishesAcrossCarriageChange :: forall s. Term s PBool
resumeFinishesAcrossCarriageChange =
  pmatch (foldAikenWalk (startOf aikenWalkField) 7 0) $ \(PPair partial paused) ->
  pmatch (resumeRawAt aikenWalkField paused) $ \(PPair resumedView resumed) ->
  pmatch
    ( walkFoldAiken resumedView resumed 13 partial )
    $ \(PPair total done) ->
  pmatch (foldAikenWalk (startOf aikenWalkField) 20 0) $ \(PPair uninterrupted uninterruptedDone) ->
    pand'List
      [ pnot #$ pwalkIsComplete # paused
      , pwalkRemaining # paused #== 13
      , pwalkIsComplete # done
      , pwalkIsComplete # uninterruptedDone
      , total #== uninterrupted
      ]

resumeSurvivesTwoInterruptions :: forall s. Term s PBool
resumeSurvivesTwoInterruptions =
  pmatch (foldAikenWalk (startOf aikenWalkField) 4 0) $ \(PPair stateOne pauseOne) ->
  pmatch (resumeAtTerm aikenWalkField pauseOne) $ \(PPair viewTwo resumeOne) ->
  pmatch (walkFoldAiken viewTwo resumeOne 9 stateOne) $ \(PPair stateTwo pauseTwo) ->
  pmatch (resumeAtTerm aikenWalkField pauseTwo) $ \(PPair viewThree resumeTwo) ->
  pmatch (walkFoldAiken viewThree resumeTwo 7 stateTwo) $ \(PPair total done) ->
  pmatch (foldAikenWalk (startOf aikenWalkField) 20 0) $ \(PPair uninterrupted uninterruptedDone) ->
    pand'List
      [ pwalkRemaining # pauseOne #== 16
      , pwalkRemaining # pauseTwo #== 7
      , pwalkIsComplete # done
      , pwalkIsComplete # uninterruptedDone
      , total #== uninterrupted
      ]

resumeDoesNotRereadCompletedItems :: forall s. Term s PBool
resumeDoesNotRereadCompletedItems =
  pmatch (foldAikenWalk (startOf aikenWalkField) 7 0) $ \(PPair _partial paused) ->
  pmatch (resumeAtTerm aikenWalkField paused) $ \(PPair resumedView resumed) ->
  pmatch (walkFoldAiken resumedView resumed 13 0) $ \(PPair _ exact) ->
  pmatch (walkFoldAiken resumedView resumed 12 0) $ \(PPair _ oneShort) ->
    pand'List
      [ pwalkIsComplete # exact
      , pnot #$ pwalkIsComplete # oneShort
      , pwalkRemaining # oneShort #== 1
      ]

resumeWorksOnFixedStrideField :: forall s. Term s PBool
resumeWorksOnFixedStrideField =
  pmatch
    (walkFoldAiken (viewOf aikenAuthenticateField) (startOf aikenAuthenticateField) 40 0)
    $ \(PPair _partial paused) ->
  pmatch (resumeAtTerm aikenAuthenticateField paused) $ \(PPair resumedView resumed) ->
  pmatch (pwalkNext # resumedView # resumed) $ \(PPair item _next) ->
    pand'List
      [ pwalkRemaining # resumed #== 24
      , item #== pconstant (aikenInputItem 0x44 40)
      ]

foldAikenWalk :: forall s.
  Term s PFieldWalkCheckpointV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PPair PInteger PFieldWalkCheckpointV1)
foldAikenWalk = walkFoldAiken (viewOf aikenWalkField)

walkFoldAiken :: forall s.
  Term s PFieldViewV1 ->
  Term s PFieldWalkCheckpointV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PPair PInteger PFieldWalkCheckpointV1)
walkFoldAiken view checkpoint budget state =
  pwalkFold
    # view # checkpoint # budget # state
    # plam
      ( \accumulator index item ->
          accumulator * 31
            + (index + 1) * (plengthBS # item)
            + (pbyteAt # item # 0)
      )

resumeAtTerm :: forall s.
  Field ->
  Term s PFieldWalkCheckpointV1 ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
resumeAtTerm field checkpoint =
  presumeFieldWalkFromCommitment
    # verifiedT field # witnessSetT field
    # (pfieldWalkCheckpointHash # checkpoint)
    # (pencodeFieldWalkCheckpoint # checkpoint)
    # carriageOf field # inputsT (referenceInputsOf field)
    # pdata (pconstant certificatePolicy)

resumeRawAt :: forall s.
  Field ->
  Term s PFieldWalkCheckpointV1 ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
resumeRawAt field checkpoint =
  presumeFieldWalkFromCommitment
    # verifiedT field # witnessSetT field
    # (pfieldWalkCheckpointHash # checkpoint)
    # (pencodeFieldWalkCheckpoint # checkpoint)
    # pcon (PRawUtxo (pdata 0))
    # inputsT [bytesRefIn (fPreimage field)]
    # pdata (pconstant certificatePolicy)

-- | An honest position one item into the three-item hash28 field.
honestMidWalk :: BS.ByteString
honestMidWalk = walkBytes hash28Field 1

-- | An honest position at the end of that field.
finishedWalk :: BS.ByteString
finishedWalk = walkBytes hash28Field 3

--------------------------------------------------------------------------------
-- §10.4 the step's own guards
--------------------------------------------------------------------------------

stepGuardTests :: [TestTree]
stepGuardTests =
  [ -- §7.2: arithmetic locates an item, it does not excuse reading its wrapper.
    -- This preimage's first slot fills the whole 30-byte stride but spells its
    -- wrapper with a length the stride does not admit. The view constructs —
    -- §7.4's count check is arithmetic over the total length, which this
    -- satisfies — so the step's own check is the only thing left.
    testCase "walk_next_refuses_a_wrapper_whose_length_misses_the_stride" $
      pfails $ itemAt miswrappedField 0
  , testCase "walk_next_refuses_a_one_byte_wrapper_on_a_fixed_stride_field" $
      pfails $ itemAt oneByteMiswrappedField 0
  , testCase "the same field's canonical second slot is unaffected by that" $
      passertEval $
        pmatch (resume miswrappedField (walkBytes' miswrappedField 1 31)) $
          \(PPair view checkpoint) ->
            pmatch (pwalkNext # view # checkpoint) $ \(PPair item _advanced) ->
              (plengthBS # item) #== 28
  , -- §5.1's no-trailing-bytes rule at the advance. Every deeper guard is
    -- satisfied: the head at the forged offset decodes, the field is
    -- variable-width so the stride clause does not apply, and the advance stays
    -- well inside the authenticated bytes. Only the clause that says the *last*
    -- item must end the field can refuse.
    --
    -- The resume that plants the position is honest about its own job: it
    -- checks that the item at that offset ends inside the field, and it does.
    -- That is the residual §10.2 item-7 gap, and this is the guard that
    -- notices one step later.
    testCase "walk_next_refuses_a_final_advance_that_misses_the_end" $
      pfails $
        pmatch (resume shortFinalField shortFinalForged) $ \(PPair view checkpoint) ->
          pmatch (pwalkNext # view # checkpoint) $ \(PPair item _advanced) -> item
  , testCase "the same field advances cleanly from its honest position" $
      passertEval $
        pmatch (resume shortFinalField (walkBytes shortFinalField 1)) $
          \(PPair view checkpoint) ->
            pmatch (pwalkNext # view # checkpoint) $ \(PPair item _advanced) ->
              item #== pconstant "\xee\xff\x99"
  ]

--------------------------------------------------------------------------------
-- §10.5 the spend-input shortcut
--------------------------------------------------------------------------------

spendInputTests :: [TestTree]
spendInputTests =
  [ testCase "spend_input_lookup_at_index_0" $
      passertEval $ maximumSpendInputLookup 0
  , testCase "spend_input_lookup_at_index_295" $
      passertEval $ maximumSpendInputLookup 295
  , testCase "spend_input_extent_is_pure_arithmetic" $
      passertEval maximumSpendInputExtent
  , testCase "decodes an input item by arithmetic" $
      passertEval $
        pand'List
          [ pmatch (pspendInputAt # viewOf spendField # pconstant (fromIntegral i)) $
            \PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} ->
              pand'List
                [ pfromData ptxInput'txId #== pconstant (inputTxId i)
                , pfromData ptxInput'outputIndex #== pconstant (fromIntegral i)
                ]
          | i <- [0, 1 :: Int]
          ]
  , testCase "the count is the field's authenticated item count" $
      passertEval $
        (pspendInputCount # viewOf spendField) #== (pfieldItemCount # viewOf spendField)
  , testCase "aborts on an index past the last input" $
      pfails $ pspendInputAt # viewOf spendField # 2
  , -- The stride guard is what refuses. This field's items are input-shaped to
    -- the byte, and it is still not a field-0/1 view.
    testCase "spend_input_at_refuses_a_variable_width_view_of_input_shaped_items" $
      pfails $ pspendInputAt # viewOf inputShapedVariableField # 0
  , testCase "spend_input_count refuses the same view" $
      pfails $ pspendInputCount # viewOf inputShapedVariableField
  ]

maximumSpendCheckpointWidth :: forall s. Term s PBool
maximumSpendCheckpointWidth =
  pmatch aikenMaximumSpendOpen $ \(PPair view start) ->
    (plengthBS # (pencodeFieldWalkCheckpoint #$ pwalkSkip # view # start # 295))
      #== pfieldWalkCheckpointBytes

maximumSpendInputLookup :: forall s. Integer -> Term s PBool
maximumSpendInputLookup index =
  pmatch aikenMaximumSpendOpen $ \(PPair view _start) ->
  pmatch (pspendInputAt # view # pconstant index) $
    \PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} ->
      pand'List
        [ pfromData ptxInput'outputIndex #== pconstant index
        , pfromData ptxInput'txId #== pconstant (BS.replicate 32 0x44)
        ]

maximumSpendInputExtent :: forall s. Term s PBool
maximumSpendInputExtent =
  pmatch aikenMaximumSpendOpen $ \(PPair view _start) ->
    pand'List $
      [ (pspendInputCount # view) #== pmaximumCardanoSpendRedeemerCount
      ]
        <> [ pmatch (pfieldItemExtent # view # pconstant index) $ \(PPair offset len) ->
              pand'List
                [ offset #== 3 + pspendInputStride * pconstant index + 2
                , len #== pspendInputItemBytes
                ]
           | index <- [0, 1, 147, 295]
           ]

aikenMaximumSpendOpen :: forall s.
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
aikenMaximumSpendOpen =
  pmatch (aikenInputSource aikenMaximumSpendPreimage) $ \(PPair verified witnessSet) ->
    aikenInputOpenFrom aikenMaximumSpendPreimage verified witnessSet

--------------------------------------------------------------------------------
-- Fields, and how to walk them
--------------------------------------------------------------------------------

{- | A field to open a walk over: which of §2.5's nine slots, the preimage that
slot commits, and whether it travels inline or as certified chunks.
-}
data Field = Field
  { fIndex :: Integer
  , fPreimage :: BS.ByteString
  , fCertified :: Bool
  }

inlineField :: Integer -> BS.ByteString -> Field
inlineField i preimage = Field i preimage False

certifiedField :: Integer -> BS.ByteString -> Field
certifiedField i preimage = Field i preimage True

-- | Three 28-byte items: §5.3's field-3 shape, stride 30, total 91 bytes.
hash28Field :: Field
hash28Field = inlineField 3 (fieldPreimage hash28Items)

-- | The same shape and the same positions over entirely different bytes.
otherHash28Field :: Field
otherHash28Field =
  inlineField 3 (fieldPreimage [BS.replicate 28 (fromIntegral (0x91 + i)) | i <- [0 .. 2 :: Int]])

-- | Two 38-byte input items: §5.3's field-0 shape, stride 40.
spendField :: Field
spendField = inlineField 0 (fieldPreimage [inputItem 0, inputItem 1])

-- | A variable-width witness-set field — the Aiken tree's ten-byte vector.
variableField :: Field
variableField = inlineField 6 shortFinalPreimage

-- The Aiken resume fixture: twenty field-6 script items whose payload lengths
-- cycle through 4..10 bytes, forcing the variable-width walk path.
aikenWalkField :: Field
aikenWalkField = inlineField 6 (fieldPreimage aikenWalkItems)

aikenOtherWalkField :: Field
aikenOtherWalkField = inlineField 6 (fieldPreimage aikenOtherWalkItems)

aikenAuthenticateField :: Field
aikenAuthenticateField =
  inlineField 0 (fieldPreimage [aikenInputItem 0x44 index | index <- [0 .. 63]])

aikenMaximumSpendPreimage :: BS.ByteString
aikenMaximumSpendPreimage =
  fieldPreimage [aikenInputItem 0x44 index | index <- [0 .. 295]]

aikenTierTwoReferenceInputs :: [TxInInfo]
aikenTierTwoReferenceInputs =
  [aikenInputOf 0 (aikenKeyOutput (PD.B $ fPreimage aikenAuthenticateField))]

aikenTierThreeItemCount :: Integer
aikenTierThreeItemCount = fromIntegral (chunkBytesK `div` 40 + 1)

aikenTierThreePreimage :: BS.ByteString
aikenTierThreePreimage =
  arrayHeader (fromIntegral aikenTierThreeItemCount)
    <> BS.concat
      ( replicate
          (fromIntegral aikenTierThreeItemCount)
          ("\x58\x26" <> aikenInputItem 0x44 0)
      )

aikenTierThreeChunks :: [BS.ByteString]
aikenTierThreeChunks =
  [ BS.take chunkBytesK aikenTierThreePreimage
  , BS.drop chunkBytesK aikenTierThreePreimage
  ]

aikenTierThreeReferenceInputs :: [TxInInfo]
aikenTierThreeReferenceInputs =
  aikenCertificateInput aikenTierThreePreimage aikenTierThreeChunks
    : zipWith
      (\index chunk -> aikenInputOf index (aikenKeyOutput (PD.B chunk)))
      [1, 2]
      aikenTierThreeChunks

aikenInputOf :: Integer -> TxOut -> TxInInfo
aikenInputOf index output =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin aikenItemTxIdB) index)
    output

aikenKeyOutput :: PD.Data -> TxOut
aikenKeyOutput datum =
  TxOut
    (pubKeyHashAddress $ PubKeyHash $ toBuiltin aikenOwnerHash)
    (adaValue 2_000_000)
    (OutputDatum $ Datum $ dataToBuiltinData datum)
    Nothing

aikenCertificateInput :: BS.ByteString -> [BS.ByteString] -> TxInInfo
aikenCertificateInput preimage chunks =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin aikenItemTxIdA) 0)
    ( TxOut
        (scriptHashAddress $ ScriptHash $ toBuiltin aikenCertificatePolicyBytes)
        ( adaValue 2_000_000
            <> singleton
              aikenCertificatePolicy
              (TokenName $ toBuiltin $ certificateAssetName aikenSampleTxId 0)
              1
        )
        (OutputDatum $ Datum $ dataToBuiltinData datum)
        Nothing
    )
  where
    datum =
      PD.Constr
        0
        [ PD.B aikenOwnerHash
        , PD.B aikenSampleTxId
        , PD.I 0
        , PD.I (fromIntegral $ BS.length preimage)
        , PD.List (map (PD.B . blake2b256) chunks)
        ]

aikenSampleTxId, aikenItemTxIdA, aikenItemTxIdB, aikenOwnerHash, aikenCertificatePolicyBytes :: BS.ByteString
aikenSampleTxId = BS.replicate 32 0x11
aikenItemTxIdA = BS.replicate 32 0x44
aikenItemTxIdB = BS.replicate 32 0x55
aikenOwnerHash = BS.replicate 28 0x66
aikenCertificatePolicyBytes = BS.replicate 28 0x22

aikenDisjointInputFieldA, aikenDisjointInputFieldB :: Field
aikenDisjointInputFieldA =
  inlineField 0 (fieldPreimage [aikenInputItem 0x44 index | index <- [0 .. 11]])
aikenDisjointInputFieldB =
  inlineField 0 (fieldPreimage [aikenInputItem 0x55 (300 + index) | index <- [0 .. 11]])

aikenWalkItems :: [BS.ByteString]
aikenWalkItems = [aikenScriptItem index | index <- [0 .. 19]]

aikenOtherWalkItems :: [BS.ByteString]
aikenOtherWalkItems = [aikenOtherScriptItem index | index <- [0 .. 19]]

aikenScriptItem :: Int -> BS.ByteString
aikenScriptItem index =
  BS.pack [0x82, 0x03, fromIntegral (0x40 + payloadLength)]
    <> BS.replicate payloadLength 0x44
  where
    payloadLength = 4 + index `mod` 7

aikenOtherScriptItem :: Int -> BS.ByteString
aikenOtherScriptItem index =
  BS.pack [0x82, 0x00, fromIntegral (0x40 + payloadLength)]
    <> BS.replicate payloadLength 0x55
  where
    payloadLength = 4 + index `mod` 7

aikenInputItem :: Word8 -> Int -> BS.ByteString
aikenInputItem txByte index =
  BS.concat ["\x82\x58\x20", BS.replicate 32 txByte, "\x19", be 2 (fromIntegral index)]

inputItemsAreContentDisjoint :: Bool
inputItemsAreContentDisjoint =
  and
    [ differsEverywhere (BS.replicate 32 0x44) (BS.replicate 32 0x55)
        && differsEverywhere (be 2 (fromIntegral index)) (be 2 (300 + fromIntegral index))
    | index <- [0 .. 11 :: Int]
    ]

scriptItemsAreContentDisjoint :: Bool
scriptItemsAreContentDisjoint =
  and
    [ differsEverywhere
        (BS.cons 0x03 $ BS.replicate (4 + index `mod` 7) 0x44)
        (BS.cons 0x00 $ BS.replicate (4 + index `mod` 7) 0x55)
    | index <- [0 .. 19 :: Int]
    ]

differsEverywhere :: BS.ByteString -> BS.ByteString -> Bool
differsEverywhere left right =
  BS.length left == BS.length right
    && and (BS.zipWith (/=) left right)

-- | The empty field: @80@, one byte, no items.
emptyField :: Field
emptyField = inlineField 3 (fieldPreimage [])

-- | 600 fixed-stride items over two chunks.
bigCertifiedField :: Field
bigCertifiedField =
  certifiedField 3 (fieldPreimage [BS.replicate 28 (fromIntegral (i `mod` 251)) | i <- [0 .. 599 :: Int]])

-- | A variable-width field over two chunks — openable as a view, not as a walk.
bigVariableCertifiedField :: Field
bigVariableCertifiedField =
  certifiedField
    2
    ( fieldPreimage
        [BS.replicate (if even i then 40 else 50) (fromIntegral (i `mod` 251)) | i <- [0 .. 499 :: Int]]
    )

{- | A variable-width field of 70 one-byte items. Its §5.1 header is @98 46@,
and @0x46@ is itself a decodable packed head declaring six bytes — so a position
aimed at offset 1 finds something readable there and is refused by the header
bound alone.
-}
headerAliasField :: Field
headerAliasField = inlineField 2 (fieldPreimage (replicate 70 "\x01"))

headerAliasForged :: BS.ByteString
headerAliasForged = walkBytes' headerAliasField 0 1

{- | Two five-byte items where the second's payload opens with @0x57@ — a packed
head declaring 23 bytes, which would run 19 bytes past the end of the field.
-}
overrunField :: Field
overrunField = inlineField 2 (fieldPreimage [BS.replicate 5 0x00, "\x57" <> BS.replicate 4 0x00])

overrunForged :: BS.ByteString
overrunForged = walkBytes' overrunField 1 8

{- | Two 30-byte field-3 slots whose /first/ spells its wrapper as @58 19@ where
the stride admits only @58 1c@. The payload still lands exactly where the stride
says, and the total length and item count are untouched, so §7.4's arithmetic
passes at construction and only the step's own wrapper check is left.
-}
miswrappedField :: Field
miswrappedField =
  inlineField
    3
    ( BS.concat
        [ arrayHeader 2
        , "\x58\x19"
        , BS.replicate 28 0x41
        , wrapItem (BS.replicate 28 0x42)
        ]
    )

{- | The Aiken fixed-stride attack through the packed one-byte bytestring head.
The first slot still occupies all 40 authenticated bytes, but @57@ declares
only 23 payload bytes and moves the payload start one byte earlier than the
stride permits.
-}
oneByteMiswrappedField :: Field
oneByteMiswrappedField =
  inlineField
    0
    ( BS.concat
        [ arrayHeader 2
        , "\x57"
        , BS.replicate 39 0x44
        , wrapItem (aikenInputItem 0x44 0)
        ]
    )

-- | The Aiken tree's vector for the final-advance rule, verbatim.
shortFinalPreimage :: BS.ByteString
shortFinalPreimage = BS.pack [0x82, 0x44, 0xaa, 0x41, 0xcc, 0xdd, 0x43, 0xee, 0xff, 0x99]

shortFinalField :: Field
shortFinalField = inlineField 6 shortFinalPreimage

{- | The honest position after one advance, with its offset moved back to 3 —
where @0x41@ is a packed head declaring one byte, so the item at that offset
ends at 5 and the resume's extent check passes.
-}
shortFinalForged :: BS.ByteString
shortFinalForged = walkBytes' shortFinalField 1 3

-- | A variable-width field whose items are byte-for-byte spend inputs.
inputShapedVariableField :: Field
inputShapedVariableField = inlineField 2 (fieldPreimage [inputItem 0, inputItem 1])

--------------------------------------------------------------------------------
-- Driving the walk
--------------------------------------------------------------------------------

openOf :: forall s. Field -> Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
openOf field =
  popenFieldWalk
    # verifiedT field
    # witnessSetT field
    # pconstant (fIndex field)
    # carriageOf field
    # inputsT (referenceInputsOf field)
    # pdata (pconstant certificatePolicy)

viewOf :: forall s. Field -> Term s PFieldViewV1
viewOf field = pmatch (openOf field) $ \(PPair view _checkpoint) -> view

startOf :: forall s. Field -> Term s PFieldWalkCheckpointV1
startOf field = pmatch (openOf field) $ \(PPair _view checkpoint) -> checkpoint

-- | The checkpoint reached by @n@ single steps from the opening position.
advancedBy :: forall s. Integer -> Field -> Term s PFieldWalkCheckpointV1
advancedBy n field = go n (startOf field)
  where
    go 0 checkpoint = checkpoint
    go k checkpoint =
      go (k - 1) (pmatch (pwalkNext # viewOf field # checkpoint) $ \(PPair _item next) -> next)

-- | The item at @index@, reached by stepping to it.
itemAt :: forall s. Field -> Integer -> Term s PByteString
itemAt field index =
  pmatch (pwalkNext # viewOf field # advancedBy index field) $ \(PPair item _next) -> item

{- | Resume the field from wire bytes, with the commitment the honest thread
would have carried.
-}
resume ::
  forall s. Field -> BS.ByteString -> Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
resume field bytes = resumeWithCommitment field bytes (referenceHash bytes)

resumeWithCommitment ::
  forall s.
  Field ->
  BS.ByteString ->
  BS.ByteString ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
resumeWithCommitment field bytes committed =
  presumeFieldWalkFromCommitment
    # verifiedT field
    # witnessSetT field
    # pconstant committed
    # pconstant bytes
    # carriageOf field
    # inputsT (referenceInputsOf field)
    # pdata (pconstant certificatePolicy)

--------------------------------------------------------------------------------
-- The reference wire form (§10.3), written from the layout
--------------------------------------------------------------------------------

{- | @86 5820 tx_id 41 idx 43 total 43 count 43 next_index 43 next_offset@ —
fixed-width scalars, 53 bytes.
-}
referenceCheckpoint ::
  BS.ByteString -> Integer -> Integer -> Integer -> Integer -> Integer -> BS.ByteString
referenceCheckpoint tid fieldIndex totalLength itemCount nextIndex nextOffset =
  BS.concat
    [ "\x86\x58\x20"
    , tid
    , "\x41"
    , be 1 fieldIndex
    , "\x43"
    , be 3 totalLength
    , "\x43"
    , be 3 itemCount
    , "\x43"
    , be 3 nextIndex
    , "\x43"
    , be 3 nextOffset
    ]

be :: Int -> Integer -> BS.ByteString
be width n =
  BS.pack [fromIntegral (n `div` (256 ^ i) `mod` 256) | i <- [width - 1, width - 2 .. 0]]

checkpointDomain :: BS.ByteString
checkpointDomain = "MidgardFieldWalkCheckpointV1"

referenceHash :: BS.ByteString -> BS.ByteString
referenceHash bytes = blake2b256 (checkpointDomain <> bytes)

-- | Wire offsets of the scalars the negative cases move.
wireFieldIndexAt, wireTotalLengthAt, wireItemCountAt, wireNextOffsetAt :: Int
wireFieldIndexAt = 36
wireTotalLengthAt = 38
wireItemCountAt = 42
wireNextOffsetAt = 50

{- | The honest wire form of the position @index@ items into a field, obtained by
walking there rather than by asserting it.
-}
walkBytes :: Field -> Integer -> BS.ByteString
walkBytes field index =
  referenceCheckpoint
    txId
    (fIndex field)
    (fromIntegral (BS.length (fPreimage field)))
    (referenceItemCount field)
    index
    (referenceOffsetAt field index)

-- | The same, with the offset overridden — the only forgery a resume can be handed.
walkBytes' :: Field -> Integer -> Integer -> BS.ByteString
walkBytes' field index offset =
  patchScalar wireNextOffsetAt 3 offset (walkBytes field index)

patchScalar :: Int -> Int -> Integer -> BS.ByteString -> BS.ByteString
patchScalar offset width value bytes =
  BS.concat [BS.take offset bytes, be width value, BS.drop (offset + width) bytes]

patchByte :: Int -> Int -> BS.ByteString -> BS.ByteString
patchByte offset value bytes =
  BS.concat
    [BS.take offset bytes, BS.singleton (fromIntegral value), BS.drop (offset + 1) bytes]

patchTxId :: BS.ByteString -> BS.ByteString -> BS.ByteString
patchTxId tid bytes = BS.concat [BS.take 3 bytes, tid, BS.drop 35 bytes]

-- | The §5.1 item count of a field's preimage, recomputed by walking it here.
referenceItemCount :: Field -> Integer
referenceItemCount field = fst (referenceHeader (fPreimage field))

-- | The byte offset of item @index@'s wrapper, recomputed by walking.
referenceOffsetAt :: Field -> Integer -> Integer
referenceOffsetAt field index = go (snd (referenceHeader (fPreimage field))) index
  where
    preimage = fPreimage field
    go offset 0 = offset
    go offset k = go (offset + itemWidthAt preimage offset) (k - 1)

itemWidthAt :: BS.ByteString -> Integer -> Integer
itemWidthAt preimage offset
  | tag >= 0x40 && tag <= 0x57 = 1 + fromIntegral (tag - 0x40)
  | tag == 0x58 = 2 + fromIntegral (BS.index preimage (fromIntegral offset + 1))
  | tag == 0x59 =
      3
        + fromIntegral (BS.index preimage (fromIntegral offset + 1)) * 256
        + fromIntegral (BS.index preimage (fromIntegral offset + 2))
  | otherwise = error "itemWidthAt: fixture is not a §5.1 item"
  where
    tag = BS.index preimage (fromIntegral offset)

-- | @(item count, header width)@ of a §5.1 array header.
referenceHeader :: BS.ByteString -> (Integer, Integer)
referenceHeader preimage
  | tag >= 0x80 && tag <= 0x97 = (fromIntegral (tag - 0x80), 1)
  | tag == 0x98 = (fromIntegral (BS.index preimage 1), 2)
  | tag == 0x99 =
      (fromIntegral (BS.index preimage 1) * 256 + fromIntegral (BS.index preimage 2), 3)
  | otherwise = error "referenceHeader: fixture is not a §5.1 envelope"
  where
    tag = BS.head preimage

--------------------------------------------------------------------------------
-- Reference preimages
--------------------------------------------------------------------------------

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

hash28Items :: [BS.ByteString]
hash28Items = [BS.replicate 28 (fromIntegral (0x41 + i)) | i <- [0 .. 2 :: Int]]

-- | The two items of 'variableField', as the Aiken vector spells them.
variableItems :: [BS.ByteString]
variableItems = [BS.pack [0xaa, 0x41, 0xcc, 0xdd], BS.pack [0xee, 0xff, 0x99]]

-- | §5.3 field 0/1: @82 ‖ 58 20 tx_id ‖ 19 index_be16@, 38 bytes.
inputItem :: Int -> BS.ByteString
inputItem i = BS.concat ["\x82", "\x58\x20", inputTxId i, "\x19", be 2 (fromIntegral i)]

inputTxId :: Int -> BS.ByteString
inputTxId i = BS.replicate 32 (fromIntegral (0xa1 + i))

--------------------------------------------------------------------------------
-- Driving the door
--------------------------------------------------------------------------------

carriageOf :: forall s. Field -> Term s PFieldCarriageV1
carriageOf field
  | fCertified field =
      pcon
        ( PCertified
            { pcertified'certRefInputIndex = pdata 0
            , pcertified'chunkRefInputIndices =
                pdata (pconstant [1 .. fromIntegral (length (chunksOf (fPreimage field)))])
            }
        )
  | otherwise = pcon (PInline (pdata (pconstant (fPreimage field))))

referenceInputsOf :: Field -> [TxInInfo]
referenceInputsOf field
  | fCertified field = certRefIn field : map bytesRefIn (chunksOf (fPreimage field))
  | otherwise = []

verifiedT :: forall s. Field -> Term s PVerifiedMidgardNativeTxCompact
verifiedT field =
  pcon $
    PVerifiedMidgardNativeTxCompact
      { pverified'txId = pconstant txId
      , pverified'version = 1
      , pverified'txCompact =
          pcon $
            PNativeTxCompact
              { pcompact'body = bodyT field
              , pcompact'witnessSetHash = pconstant (wsHashOf (witnessSetOf field))
              , pcompact'validityCode = 3
              }
      }

{- | The body's six hash slots, with slot @fIndex@ carrying this preimage's
commitment when the field is a body field.
-}
bodyT :: forall s. Field -> Term s PNativeTxBodyCompact
bodyT field =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = slot 0
      , pbodyCompact'referenceInputsHash = slot 1
      , pbodyCompact'outputsHash = slot 2
      , pbodyCompact'fee = 1_000_000
      , pbodyCompact'validityIntervalStart = 0
      , pbodyCompact'validityIntervalEnd = 1
      , pbodyCompact'requiredObserversHash = slot 3
      , pbodyCompact'requiredSignersHash = slot 4
      , pbodyCompact'mintHash = slot 5
      , pbodyCompact'scriptIntegrityHash = pconstant (hash32 0x07)
      , pbodyCompact'auxiliaryDataHash = pconstant (hash32 0x08)
      , pbodyCompact'networkId = 1
      }
  where
    slot i
      | fIndex field == i = pconstant (blake2b256 (fPreimage field))
      | otherwise = pconstant (hash32 (0x20 + fromIntegral i))

-- | The three witness-set hashes, with slot @fIndex@ likewise.
witnessSetOf :: Field -> (BS.ByteString, BS.ByteString, BS.ByteString)
witnessSetOf field = (slot 7, slot 6, slot 8)
  where
    slot i
      | fIndex field == i = blake2b256 (fPreimage field)
      | otherwise = hash32 (0x30 + fromIntegral i)

witnessSetT :: forall s. Field -> Term s PNativeTxWitnessSetCompact
witnessSetT field =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant addr)
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant script)
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant redeemer)
      }
  where
    (addr, script, redeemer) = witnessSetOf field

-- | @encode_native_tx_witness_set_compact@, written out from §2.5.
wsHashOf :: (BS.ByteString, BS.ByteString, BS.ByteString) -> BS.ByteString
wsHashOf (addr, script, redeemer) =
  blake2b256 (BS.concat ["\x83", defBytes32 addr, defBytes32 script, defBytes32 redeemer])

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT = pconstant

--------------------------------------------------------------------------------
-- Tier-3 plumbing
--------------------------------------------------------------------------------

chunkBytesK :: Int
chunkBytesK = 15900

chunksOf :: BS.ByteString -> [BS.ByteString]
chunksOf bytes
  | BS.null bytes = []
  | BS.length bytes <= chunkBytesK = [bytes]
  | otherwise = BS.take chunkBytesK bytes : chunksOf (BS.drop chunkBytesK bytes)

certRefIn :: Field -> TxInInfo
certRefIn field =
  TxInInfo
    (outRefN 0)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin certificateScript)))
        ( adaValue 2_000_000
            <> singleton
              certificatePolicy
              (TokenName (toBuiltin (certificateAssetName txId (fIndex field))))
              1
        )
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )
  where
    preimage = fPreimage field
    datum =
      PD.Constr
        0
        [ PD.B (BS.replicate 28 0x31)
        , PD.B txId
        , PD.I (fIndex field)
        , PD.I (fromIntegral (BS.length preimage))
        , PD.List (map (PD.B . blake2b256) (chunksOf preimage))
        ]

-- | §8.6's derivation: @blake2b_256(field_index_byte ‖ tx_id)@.
certificateAssetName :: BS.ByteString -> Integer -> BS.ByteString
certificateAssetName tid index = blake2b256 (BS.cons (fromIntegral index) tid)

bytesRefIn :: BS.ByteString -> TxInInfo
bytesRefIn bytes =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin carriageScript)))
        (adaValue 2_000_000)
        (OutputDatum (Datum (dataToBuiltinData (PD.B bytes))))
        Nothing
    )

--------------------------------------------------------------------------------
-- Reference hashing and identities
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 h = "\x58\x20" <> h

hash32 :: Int -> BS.ByteString
hash32 n = blake2b256 (BS.pack [fromIntegral n])

txId, otherTxId :: BS.ByteString
txId = BS.replicate 32 0x0a
otherTxId = BS.replicate 32 0x0b

certificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x91))

certificateScript, carriageScript :: BS.ByteString
certificateScript = BS.replicate 28 0x93
carriageScript = BS.replicate 28 0x94

adaValue :: Integer -> Value
adaValue = singleton (CurrencySymbol "") (TokenName "")

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01)))
