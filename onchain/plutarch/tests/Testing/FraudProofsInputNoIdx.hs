{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsInputNoIdx
Description : Behavioural tests for the Plutarch ports of
              @validators/fraud-proofs/{input-no-idx,reference-input-no-idx}/step-0{1,2,3,4}.ak@.

Two families, driven together because they are the same proof one §2.5 slot
apart: @input-no-idx@ opens field 0 and @reference-input-no-idx@ opens field 1,
and everything else about them is identical.

__That similarity is the reason to test them side by side.__ §4 removed
field-index domain separation, so a field-0 preimage and a field-1 preimage over
the same items commit /identically/. Nothing in either family's types or
redeemers names which slot it is about — the index is a compiled-in literal
passed to the door. A port that passed the wrong constant would prove the other
family's fault, and no test of the types would notice. The fixture makes that
detectable by giving 'tx3' /different/ spend inputs and reference inputs, so each
family's step-02 succeeds only when it read its own slot.

__The two bindings and where a valid block dies.__ Steps 01 and 03 both run the
full inclusion check, against the same thread and so the same block. Step-03's
own guard is @producing_tx_id == bad_input_tx_id@, and it is where a challenge
against a valid block dies: in a valid block every input names its true producing
transaction, so binding some other committed transaction to the forwarded id is
refused. The @rejects a substituted producing transaction@ cases are that.

__The verdict rests on an authenticated count.__ Step-04's rule compares the
challenged index against @field_item_count@ of field 2 — a number, not an item —
which the door only answers where it has been checked against the bytes.
-}
module Testing.FraudProofsInputNoIdx (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.InputNoIdx (
  inputNoIdxStep01Validator,
  inputNoIdxStep02Validator,
  inputNoIdxStep03Validator,
  inputNoIdxStep04Validator,
 )
import Midgard.Validators.FraudProofs.ReferenceInputNoIdx (
  referenceInputNoIdxStep01Validator,
  referenceInputNoIdxStep02Validator,
  referenceInputNoIdxStep03Validator,
  referenceInputNoIdxStep04Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Input Index Fraud Proof Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "input-no-idx" (familyTests spendFamily)
    , testGroup "reference-input-no-idx" (familyTests referenceFamily)
    , testGroup "the two families read different slots" slotTests
    ]

fixtureTests :: [TestTree]
fixtureTests =
  [ -- What makes the slot confusion detectable at all: one committed
    -- transaction whose two collections commit to different values.
    testCase "tx3's two input collections differ" $
      assertBool "tx3's slots agree" (tSpendInputs tx3 /= tReferenceInputs tx3)
  , testCase "tx1's two input collections agree" $
      tSpendInputs tx1 @?= tReferenceInputs tx1
  , testCase "the producing transaction has two outputs" $
      tOutputCount tx1 @?= 2
  ]

--------------------------------------------------------------------------------
-- One family, described by what differs
--------------------------------------------------------------------------------

{- | The two families differ only in which slot step-02 opens and which
collection it reads, so one description drives both.
-}
data Family = Family
  { fStep01 :: forall s. ScriptContext -> Term s PUnit
  , fStep02 :: forall s. ScriptContext -> Term s PUnit
  , fStep03 :: forall s. ScriptContext -> Term s PUnit
  , fStep04 :: forall s. ScriptContext -> Term s PUnit
  , fPreimage :: Tx -> BS.ByteString
  -- ^ The collection step-02 opens, for a given transaction.
  , fOtherPreimage :: Tx -> BS.ByteString
  -- ^ The /sibling/ slot's collection — what a mis-indexed port would read.
  }

spendFamily :: Family
spendFamily =
  Family
    { fStep01 = \ctx ->
        inputNoIdxStep01Validator
          # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
          # pdata (pconstant ctPolicy)
          # pdata (pconstant hubOracleHash)
          # pconstant ctx
    , fStep02 = \ctx ->
        inputNoIdxStep02Validator
          # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
          # pdata (pconstant ctPolicy)
          # pdata (pconstant certificatePolicy)
          # pconstant ctx
    , fStep03 = \ctx ->
        inputNoIdxStep03Validator
          # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
          # pdata (pconstant ctPolicy)
          # pdata (pconstant hubOracleHash)
          # pconstant ctx
    , fStep04 = \ctx ->
        inputNoIdxStep04Validator
          # pdata (pconstant fpPolicy)
          # pdata (pconstant fraudProofAddress)
          # pdata (pconstant ctPolicy)
          # pdata (pconstant certificatePolicy)
          # pconstant ctx
    , fPreimage = spendInputsPreimage
    , fOtherPreimage = referenceInputsPreimage
    }

referenceFamily :: Family
referenceFamily =
  Family
    { fStep01 = \ctx ->
        referenceInputNoIdxStep01Validator
          # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
          # pdata (pconstant ctPolicy)
          # pdata (pconstant hubOracleHash)
          # pconstant ctx
    , fStep02 = \ctx ->
        referenceInputNoIdxStep02Validator
          # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
          # pdata (pconstant ctPolicy)
          # pdata (pconstant certificatePolicy)
          # pconstant ctx
    , fStep03 = \ctx ->
        referenceInputNoIdxStep03Validator
          # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
          # pdata (pconstant ctPolicy)
          # pdata (pconstant hubOracleHash)
          # pconstant ctx
    , fStep04 = \ctx ->
        referenceInputNoIdxStep04Validator
          # pdata (pconstant fpPolicy)
          # pdata (pconstant fraudProofAddress)
          # pdata (pconstant ctPolicy)
          # pdata (pconstant certificatePolicy)
          # pconstant ctx
    , fPreimage = referenceInputsPreimage
    , fOtherPreimage = spendInputsPreimage
    }

--------------------------------------------------------------------------------
-- The tests every family runs
--------------------------------------------------------------------------------

familyTests :: Family -> [TestTree]
familyTests f =
  [ testGroup
      "step-01"
      [ testCase "binds the disputed transaction and forwards its id" $
          psucceeds $ fStep01 f (context01 defaultStep01)
      , testCase "rejects an output at a script that is not step-02's" $
          pfails $ fStep01 f (context01 defaultStep01 {s1OutputScript = otherScript})
      , testCase "rejects a state carrying another transaction's id" $
          pfails $
            fStep01 f (context01 defaultStep01 {s1OutputState = Just (state02 tx3Id)})
      , testCase "rejects a raw root the header does not commit" $
          pfails $ fStep01 f (context01 defaultStep01 {s1PhasRoot = otherRoot})
      , testCase "a cancel burning the thread token succeeds" $
          psucceeds $ fStep01 f (cancelContext True)
      , testCase "a cancel that does not burn the thread token fails" $
          pfails $ fStep01 f (cancelContext False)
      , testCase "a minting purpose fails" $
          pfails $ fStep01 f (asMinting (context01 defaultStep01))
      ]
  , testGroup
      "step-02"
      [ testCase "reads the named item and splits it in two" $
          psucceeds $ fStep02 f (context02 f defaultStep02)
      , testCase "rejects an opening of a transaction the thread did not anchor" $
          pfails $ fStep02 f (context02 f defaultStep02 {s2OpeningCbor = tx3Cbor})
      , -- §7.3: an out-of-range read aborts rather than clamping.
        testCase "rejects an index past the end of the collection" $
          pfails $ fStep02 f (context02 f defaultStep02 {s2BadIndex = 1})
      , -- The empty collection: well formed, committed by no slot of tx1.
        testCase "rejects a preimage the transaction does not commit" $
          pfails $ fStep02 f (context02 f defaultStep02 {s2Preimage = Just (fPreimage f txEmpty)})
      , testCase "rejects an output at a script that is not step-03's" $
          pfails $ fStep02 f (context02 f defaultStep02 {s2OutputScript = otherScript})
      , -- The state is the input *split*, and the halves are in declaration
        -- order: the id first, because step-03 uses it, then the index, which
        -- step-04 does.
        --
        -- Worth recording that the /unsplit/ shape is not testable here: a
        -- `MidgardTxInput` is `Constr 0 [B tx_id, I output_index]` and this
        -- state is `Constr 0 [B bad_input_tx_id, I bad_input_output_index]`, so
        -- the two are byte-identical. An SDK that forwarded the decoded input
        -- whole would produce the right bytes by accident, and nothing on
        -- either side would notice — which is fine, because they mean the same
        -- thing, but it does mean the split is a readability choice rather than
        -- an enforced one.
        testCase "rejects a state with the two halves transposed" $
          pfails $
            fStep02
              f
              ( context02
                  f
                  defaultStep02
                    { s2OutputState =
                        Just (PD.Constr 0 [PD.I (snd sharedInputRef), PD.B (fst sharedInputRef)])
                    }
              )
      , testCase "rejects a state naming another output index" $
          pfails $
            fStep02
              f
              (context02 f defaultStep02 {s2OutputState = Just (state03 (fst sharedInputRef) 9)})
      ]
  , testGroup
      "step-03"
      [ testCase "binds the producing transaction and forwards the index" $
          psucceeds $ fStep03 f (context03 defaultStep03)
      , -- Where a challenge against a valid block dies.
        testCase "rejects a substituted producing transaction" $
          pfails $
            fStep03
              f
              (context03 defaultStep03 {s3BoundTxId = tx3Id, s3BoundCbor = tx3Cbor})
      , testCase "rejects an output at a script that is not step-04's" $
          pfails $ fStep03 f (context03 defaultStep03 {s3OutputScript = otherScript})
      , testCase "rejects a state that alters the challenged output index" $
          pfails $
            fStep03
              f
              (context03 defaultStep03 {s3OutputState = Just (state04 tx1Id 9)})
      , testCase "rejects a raw root the header does not commit" $
          pfails $ fStep03 f (context03 defaultStep03 {s3PhasRoot = otherRoot})
      ]
  , testGroup
      "step-04"
      [ -- The producing transaction has two outputs, so index 2 is out of range.
        testCase "convicts when the index is at the output count" $
          psucceeds $ fStep04 f (context04 defaultStep04)
      , testCase "convicts when the index is past the output count" $
          psucceeds $ fStep04 f (context04 defaultStep04 {s4BadIndex = 5})
      , -- ...and index 1 names a real output, so an honest transaction is safe.
        testCase "rejects an index the producing transaction really has" $
          pfails $ fStep04 f (context04 defaultStep04 {s4BadIndex = 1})
      , testCase "rejects an index of zero against a transaction with outputs" $
          pfails $ fStep04 f (context04 defaultStep04 {s4BadIndex = 0})
      , testCase "rejects an opening of a transaction the thread did not anchor" $
          pfails $ fStep04 f (context04 defaultStep04 {s4OpeningCbor = tx3Cbor})
      , -- The count has to be the committed one: a fabricated shorter outputs
        -- preimage would make any index out of range.
        testCase "rejects an outputs preimage the transaction does not commit" $
          pfails $
            fStep04 f (context04 defaultStep04 {s4Preimage = Just (outputCollectionPreimage 1)})
      , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
          pfails $ fStep04 f (context04 defaultStep04 {s4FraudProofAddress = otherAddress})
      , testCase "rejects a conviction under a name that is not the thread's" $
          pfails $ fStep04 f (context04 defaultStep04 {s4FraudProofName = otherThreadName})
      ]
  ]

--------------------------------------------------------------------------------
-- The slot the two families do not share
--------------------------------------------------------------------------------

{- | The one thing that separates the two families, stated directly.

'tx3' spends 'otherInputRef' and /references/ 'sharedInputRef', so its two
collections commit to different values. Each family is then handed both
preimages of that one committed transaction: the one its own slot commits, and
the sibling's. A family that read the wrong index would authenticate against the
sibling's commitment, and all four cases below would flip.
-}
slotTests :: [TestTree]
slotTests =
  [ testCase "input-no-idx refuses tx3's reference-inputs preimage" $
      pfails $ fStep02 spendFamily (tx3Opening spendFamily (fOtherPreimage spendFamily tx3) (fst otherInputRef))
  , testCase "reference-input-no-idx refuses tx3's spend-inputs preimage" $
      pfails $
        fStep02 referenceFamily (tx3Opening referenceFamily (fOtherPreimage referenceFamily tx3) (fst sharedInputRef))
  , -- ...and each accepts its own slot's preimage for the same transaction,
    -- forwarding the item that slot holds.
    testCase "input-no-idx accepts tx3's spend inputs" $
      psucceeds $
        fStep02
          spendFamily
          (tx3Opening spendFamily (spendInputsPreimage tx3) (fst otherInputRef))
  , testCase "reference-input-no-idx accepts tx3's reference inputs" $
      psucceeds $
        fStep02
          referenceFamily
          (tx3Opening referenceFamily (referenceInputsPreimage tx3) (fst sharedInputRef))
  ]

-- | Step-02 over 'tx3', with the preimage and the expected forwarded id given.
tx3Opening :: Family -> BS.ByteString -> BS.ByteString -> ScriptContext
tx3Opening f preimage forwardedTxId =
  context02
    f
    defaultStep02
      { s2StateTxId = tx3Id
      , s2OpeningCbor = tx3Cbor
      , s2Preimage = Just preimage
      , s2OutputState = Just (state03 forwardedTxId 0)
      }

--------------------------------------------------------------------------------
-- Thread states
--------------------------------------------------------------------------------

state02 :: BS.ByteString -> PD.Data
state02 txId = PD.Constr 0 [PD.B txId]

state03 :: BS.ByteString -> Integer -> PD.Data
state03 txId index = PD.Constr 0 [PD.B txId, PD.I index]

state04 :: BS.ByteString -> Integer -> PD.Data
state04 txId index = PD.Constr 0 [PD.B txId, PD.I index]

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

data Step01 = Step01
  { s1OutputScript :: BS.ByteString
  , s1OutputState :: Maybe PD.Data
  , s1PhasRoot :: BS.ByteString
  }

defaultStep01 :: Step01
defaultStep01 =
  Step01
    { s1OutputScript = nextScript
    , s1OutputState = Just (state02 tx1Id)
    , s1PhasRoot = phasRoot
    }

{- | Steps 01 and 03 take bare @NativeTxInclusionArgs@ rather than a carriage, so
the @Continue@ payload is the args record itself.
-}
context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [bareInclusionArgs tx1Id tx1Cbor (s1PhasRoot s)])
    [threadInput]
    [stepOutput (s1OutputScript s) (s1OutputState s)]
    referenceInputs
    [phasEntry (s1PhasRoot s) tx1Id tx1Cbor]
    mempty

cancelContext :: Bool -> ScriptContext
cancelContext burns =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    referenceInputs
    [cancelMintEntry (if burns then threadName else otherThreadName)]
    mempty

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

data Step02 = Step02
  { s2StateTxId :: BS.ByteString
  , s2OpeningCbor :: BS.ByteString
  , s2Preimage :: Maybe BS.ByteString
  , s2BadIndex :: Integer
  , s2OutputScript :: BS.ByteString
  , s2OutputState :: Maybe PD.Data
  }

defaultStep02 :: Step02
defaultStep02 =
  Step02
    { s2StateTxId = tx1Id
    , s2OpeningCbor = tx1Cbor
    , s2Preimage = Nothing
    , s2BadIndex = 0
    , s2OutputScript = nextScript
    , s2OutputState = Just (state03 (fst sharedInputRef) (snd sharedInputRef))
    }

context02 :: Family -> Step02 -> ScriptContext
context02 f s =
  spendContext
    (stepDatum (Just (state02 (s2StateTxId s))))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , bodyOpening (s2OpeningCbor s) preimage
            , PD.I (s2BadIndex s)
            ]
        ]
    )
    [threadInput]
    [stepOutput (s2OutputScript s) (s2OutputState s)]
    referenceInputs
    []
    mempty
  where
    preimage = maybe (fPreimage f (txFor (s2StateTxId s))) id (s2Preimage s)

-- | The fixture transaction a given id belongs to.
txFor :: BS.ByteString -> Tx
txFor txId
  | txId == tx1Id = tx1
  | txId == tx2Id = tx2
  | txId == tx3Id = tx3
  | otherwise = txEmpty

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

data Step03 = Step03
  { s3BoundTxId :: BS.ByteString
  , s3BoundCbor :: BS.ByteString
  , s3PhasRoot :: BS.ByteString
  , s3OutputScript :: BS.ByteString
  , s3OutputState :: Maybe PD.Data
  }

defaultStep03 :: Step03
defaultStep03 =
  Step03
    { s3BoundTxId = tx1Id
    , s3BoundCbor = tx1Cbor
    , s3PhasRoot = phasRoot
    , s3OutputScript = nextScript
    , s3OutputState = Just (state04 tx1Id 2)
    }

context03 :: Step03 -> ScriptContext
context03 s =
  spendContext
    (stepDatum (Just (state03 tx1Id 2)))
    (PD.Constr 1 [bareInclusionArgs (s3BoundTxId s) (s3BoundCbor s) (s3PhasRoot s)])
    [threadInput]
    [stepOutput (s3OutputScript s) (s3OutputState s)]
    referenceInputs
    [phasEntry (s3PhasRoot s) (s3BoundTxId s) (s3BoundCbor s)]
    mempty

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

data Step04 = Step04
  { s4BadIndex :: Integer
  , s4OpeningCbor :: BS.ByteString
  , s4Preimage :: Maybe BS.ByteString
  , s4FraudProofAddress :: Address
  , s4FraudProofName :: BS.ByteString
  }

defaultStep04 :: Step04
defaultStep04 =
  Step04
    { s4BadIndex = 2
    , s4OpeningCbor = tx1Cbor
    , s4Preimage = Nothing
    , s4FraudProofAddress = fraudProofAddress
    , s4FraudProofName = threadName
    }

context04 :: Step04 -> ScriptContext
context04 s =
  spendContext
    (stepDatum (Just (state04 tx1Id (s4BadIndex s))))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [PD.I 0, PD.I 0, PD.I 0, bodyOpening (s4OpeningCbor s) preimage]
        ]
    )
    [threadInput]
    [convictionOutput (s4FraudProofAddress s) (s4FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (s4FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (s4FraudProofName s))) 1)
  where
    preimage = maybe (outputsPreimage tx1) id (s4Preimage s)
