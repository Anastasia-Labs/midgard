{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsSignature
Description : Behavioural tests for the Plutarch ports of
              @validators/fraud-proofs/{missing-signature,invalid-signature}/step-*.ak@.

The two witness-set families, driven together because they turn on the same
thing and fail in opposite directions.

__Both live or die on the §2.5 anchor's second half.__ @verified_tx_id@
authenticates fields 0–5 and nothing else: §3's id preimage is the /body/, so the
compact structure's trailing @witness_set_hash@ sits outside it and bytes carrying
the genuine body with an invented tail re-derive to the very same id. Only
step-01 can authenticate that hash, because only step-01 holds the compact
structure the block's counted @transactions_root@ committed.

The forgeries the two families invite are mirror images, and both are reproduced
below rather than described:

* @missing-signature@ wants the __empty__ witness set. Under it "the required
  signature is absent" is true of every transaction ever committed — a slashing
  proof against every honest operator at once.
* @invalid-signature@ wants a __fabricated__ witness set holding a key and a
  signature that genuinely do not match, making an "invalid signature" fault
  provable against a signature the transaction never carried.

Both are refused for the same reason: the door re-derives the supplied witness
set against the hash the /thread/ carries, and a witness set the transaction
never committed does not hash to it.

__The absence is a fold and the fault is an index.__ "No witness carries this
key" is only true of a walk that reached the end, so @missing-signature@'s
step-04 folds the whole field and asserts completion. "This witness's signature
is bad" is a claim about one named item, so @invalid-signature@ reaches it by
arithmetic at §5.3's fixed 101-byte stride and never sees the rest.

Signatures are real Ed25519, generated in "Testing.FraudProofsFixture" over each
transaction's own §3 id — which is what makes the negative case that matters
(a witness that /does/ verify) constructible at all.
-}
module Testing.FraudProofsSignature (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.InvalidSignature (
  invalidSignatureStep01Validator,
  invalidSignatureStep02Validator,
 )
import Midgard.Validators.FraudProofs.MissingSignature (
  missingSignatureStep01Validator,
  missingSignatureStep02Validator,
  missingSignatureStep03Validator,
  missingSignatureStep04Validator,
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
    "Signature Fraud Proof Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "missing-signature" missingTests
    , testGroup "invalid-signature" invalidTests
    , testGroup "the witness-set anchor" anchorTests
    ]

{- | The two violations have to be real violations, and the honest transaction
has to be honestly witnessed, or every case below proves nothing.
-}
fixtureTests :: [TestTree]
fixtureTests =
  [ testCase "txUnsigned requires a signer it is not witnessed by" $
      assertBool "txUnsigned is witnessed by its required signer" $
        tRequiredSigners txUnsigned /= map wKeyIndex (tWitnesses txUnsigned)
  , testCase "tx1 is witnessed by the signer it requires" $
      tRequiredSigners tx1 @?= map wKeyIndex (tWitnesses tx1)
  , testCase "txBadSig's only witness signed something else" $
      map wValid (tWitnesses txBadSig) @?= [False]
  , testCase "tx1's witness signed tx1's own id" $
      map wValid (tWitnesses tx1) @?= [True]
  , {- §5.1's envelope over §5.3's items, spelled out because both widths cross
       a CBOR header boundary and the wrong guess is plausible.

       A field-7 item is @82 ‖ 58 20 vkey ‖ 58 40 sig@ = 101 bytes, and 101 > 23
       so its wrapper is the one-byte-length form @58 65@: 2 + 101. With a
       one-item array header that is 1 + 103 = 104.

       A field-4 item is a bare 28-byte hash, and 28 > 23 too, so its wrapper is
       @58 1c@: 2 + 28. With the header, 1 + 30 = 31. -}
    testCase "a one-witness field-7 preimage is 104 bytes" $
      BS.length (addressWitnessesPreimage tx1) @?= 1 + (2 + 101)
  , testCase "a one-signer field-4 preimage is 31 bytes" $
      BS.length (requiredSignersPreimage tx1) @?= 1 + (2 + 28)
  ]

--------------------------------------------------------------------------------
-- missing-signature
--------------------------------------------------------------------------------

missingTests :: [TestTree]
missingTests =
  [ testGroup
      "step-01"
      [ testCase "binds the transaction and writes both halves of the anchor" $
          psucceeds $ missing01 (mContext01 defaultMissing01)
      , testCase "rejects an output at a script that is not step-02's" $
          pfails $ missing01 (mContext01 defaultMissing01 {m1OutputScript = otherScript})
      , -- The half that matters: a state naming any other witness-set hash is
        -- not the one step-01 read off the block-committed structure.
        testCase "rejects a state naming another witness-set hash" $
          pfails $
            missing01
              ( mContext01
                  defaultMissing01
                    {m1OutputState = Just (mState02 txUnsignedId (witnessSetHashOf tx1))}
              )
      , testCase "rejects a state carrying only the transaction id" $
          pfails $
            missing01
              (mContext01 defaultMissing01 {m1OutputState = Just (PD.Constr 0 [PD.B txUnsignedId])})
      , testCase "rejects a raw root the header does not commit" $
          pfails $ missing01 (mContext01 defaultMissing01 {m1PhasRoot = otherRoot})
      , testCase "a cancel burning the thread token succeeds" $
          psucceeds $ missing01 (cancelContext True)
      , testCase "a cancel that does not burn the thread token fails" $
          pfails $ missing01 (cancelContext False)
      , testCase "a minting purpose fails" $
          pfails $ missing01 (asMinting (mContext01 defaultMissing01))
      ]
  , testGroup
      "step-02"
      [ testCase "reads the required signer's hash out of field 4" $
          psucceeds $ missing02 (mContext02 defaultMissing02)
      , testCase "rejects an opening of a transaction the thread did not anchor" $
          pfails $ missing02 (mContext02 defaultMissing02 {m2OpeningCbor = tx1Cbor})
      , testCase "rejects an index past the end of the collection" $
          pfails $ missing02 (mContext02 defaultMissing02 {m2SignerIndex = 1})
      , testCase "rejects a preimage the transaction does not commit" $
          pfails $
            missing02 (mContext02 defaultMissing02 {m2Preimage = Just (requiredSignersPreimage tx1)})
      , testCase "rejects an output at a script that is not step-03's" $
          pfails $ missing02 (mContext02 defaultMissing02 {m2OutputScript = otherScript})
      , testCase "rejects a state naming another signer's hash" $
          pfails $
            missing02
              ( mContext02
                  defaultMissing02
                    {m2OutputState = Just (mState03 (keyHashFor 0) txUnsignedId unsignedWsHash)}
              )
      , -- Both halves of the anchor have to survive every step: step-04 needs
        -- the hash and cannot re-derive it.
        testCase "rejects a state that drops the witness-set hash" $
          pfails $
            missing02
              ( mContext02
                  defaultMissing02
                    {m2OutputState = Just (PD.Constr 0 [PD.B (keyHashFor 1), PD.B txUnsignedId])}
              )
      ]
  , testGroup
      "step-03"
      [ testCase "accepts the key that hashes to the carried signer hash" $
          psucceeds $ missing03 (mContext03 defaultMissing03)
      , -- The bridge's only guard. Field 4 holds 28-byte hashes and field 7
        -- holds 32-byte keys, so without this the prover picks the key freely.
        testCase "rejects a key that hashes to something else" $
          pfails $ missing03 (mContext03 defaultMissing03 {m3Vkey = verKeyFor 0})
      , testCase "rejects a key that is not a key at all" $
          pfails $ missing03 (mContext03 defaultMissing03 {m3Vkey = BS.replicate 32 0x00})
      , testCase "rejects an output at a script that is not step-04's" $
          pfails $ missing03 (mContext03 defaultMissing03 {m3OutputScript = otherScript})
      , testCase "rejects a state that drops the witness-set hash" $
          pfails $
            missing03
              ( mContext03
                  defaultMissing03
                    {m3OutputState = Just (PD.Constr 0 [PD.B (verKeyFor 1), PD.B txUnsignedId])}
              )
      ]
  , testGroup
      "step-04"
      [ testCase "convicts when no witness carries the required key" $
          psucceeds $ missing04 (mContext04 defaultMissing04)
      , -- Where a challenge against an honestly witnessed transaction dies.
        testCase "rejects a transaction whose required signer did witness it" $
          pfails $
            missing04
              ( mContext04
                  defaultMissing04
                    { m4StateVkey = verKeyFor 0
                    , m4StateTxId = tx1Id
                    , m4StateWsHash = witnessSetHashOf tx1
                    , m4OpeningCbor = tx1Cbor
                    , m4WitnessSetOf = tx1
                    , m4Preimage = Just (addressWitnessesPreimage tx1)
                    }
              )
      , testCase "rejects an opening of a transaction the thread did not anchor" $
          pfails $ missing04 (mContext04 defaultMissing04 {m4OpeningCbor = tx1Cbor})
      , testCase "rejects a preimage the witness set does not commit" $
          pfails $
            missing04 (mContext04 defaultMissing04 {m4Preimage = Just (addressWitnessesPreimage tx3)})
      , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
          pfails $ missing04 (mContext04 defaultMissing04 {m4FraudProofAddress = otherAddress})
      , testCase "rejects a conviction under a name that is not the thread's" $
          pfails $ missing04 (mContext04 defaultMissing04 {m4FraudProofName = otherThreadName})
      ]
  ]

--------------------------------------------------------------------------------
-- invalid-signature
--------------------------------------------------------------------------------

invalidTests :: [TestTree]
invalidTests =
  [ testGroup
      "step-01"
      [ testCase "binds the transaction and writes both halves of the anchor" $
          psucceeds $ invalid01 (iContext01 defaultInvalid01)
      , testCase "rejects an output at a script that is not step-02's" $
          pfails $ invalid01 (iContext01 defaultInvalid01 {i1OutputScript = otherScript})
      , testCase "rejects a state naming another witness-set hash" $
          pfails $
            invalid01
              ( iContext01
                  defaultInvalid01
                    {i1OutputState = Just (mState02 txBadSigId (witnessSetHashOf tx1))}
              )
      , testCase "rejects a raw root the header does not commit" $
          pfails $ invalid01 (iContext01 defaultInvalid01 {i1PhasRoot = otherRoot})
      ]
  , testGroup
      "step-02"
      [ testCase "convicts a witness whose signature does not verify" $
          psucceeds $ invalid02 (iContext02 defaultInvalid02)
      , -- Where a challenge against an honest transaction dies: the message is
        -- the transaction's own id, and tx1's witness signed exactly that.
        testCase "rejects a witness whose signature does verify" $
          pfails $
            invalid02
              ( iContext02
                  defaultInvalid02
                    { i2StateTxId = tx1Id
                    , i2StateWsHash = witnessSetHashOf tx1
                    , i2OpeningCbor = tx1Cbor
                    , i2WitnessSetOf = tx1
                    , i2Preimage = Just (addressWitnessesPreimage tx1)
                    }
              )
      , testCase "rejects an opening of a transaction the thread did not anchor" $
          pfails $ invalid02 (iContext02 defaultInvalid02 {i2OpeningCbor = tx1Cbor})
      , testCase "rejects an index past the end of the collection" $
          pfails $ invalid02 (iContext02 defaultInvalid02 {i2WitnessIndex = 1})
      , testCase "rejects a preimage the witness set does not commit" $
          pfails $
            invalid02 (iContext02 defaultInvalid02 {i2Preimage = Just (addressWitnessesPreimage tx1)})
      , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
          pfails $ invalid02 (iContext02 defaultInvalid02 {i2FraudProofAddress = otherAddress})
      , testCase "rejects a conviction under a name that is not the thread's" $
          pfails $ invalid02 (iContext02 defaultInvalid02 {i2FraudProofName = otherThreadName})
      ]
  ]

--------------------------------------------------------------------------------
-- The anchor, reproduced from both directions
--------------------------------------------------------------------------------

{- | The forgeries the second half of the anchor exists to stop, driven end to
end rather than described.

Each substitutes a witness set the transaction never committed and hands the
step compact bytes carrying the /genuine/ body — which re-derive to the anchored
transaction id, so a door checking only the id would let both through.
-}
anchorTests :: [TestTree]
anchorTests =
  [ -- The empty witness set: under it, "the required signature is absent" is
    -- true of every transaction ever committed.
    testCase "missing-signature refuses the empty-witness-set substitution" $
      pfails $
        missing04
          ( mContext04
              defaultMissing04
                { m4StateTxId = tx1Id
                , m4StateWsHash = witnessSetHashOf tx1
                , m4StateVkey = verKeyFor 0
                , m4OpeningCbor = tx1Cbor
                , m4WitnessSetOf = txEmpty
                , m4Preimage = Just (addressWitnessesPreimage txEmpty)
                }
          )
  , -- The mirror: a fabricated witness set holding a key and a signature that
    -- do not match, making a fault provable against a signature that was never
    -- carried.
    testCase "invalid-signature refuses a fabricated witness set" $
      pfails $
        invalid02
          ( iContext02
              defaultInvalid02
                { i2StateTxId = tx1Id
                , i2StateWsHash = witnessSetHashOf tx1
                , i2OpeningCbor = tx1Cbor
                , i2WitnessSetOf = txBadSig
                , i2Preimage = Just (addressWitnessesPreimage txBadSig)
                }
          )
  , -- ...and each is accepted when the thread's own hash is the one that names
    -- it, which is what shows both cases turn on the anchor rather than on
    -- anything about the witness set's contents.
    testCase "the same substitution is accepted under its own anchor" $
      psucceeds $
        invalid02
          ( iContext02
              defaultInvalid02
                { i2StateTxId = txBadSigId
                , i2StateWsHash = witnessSetHashOf txBadSig
                , i2OpeningCbor = txBadSigCbor
                , i2WitnessSetOf = txBadSig
                }
          )
  ]

--------------------------------------------------------------------------------
-- Thread states
--------------------------------------------------------------------------------

-- | Both families' step-02 state: the §2.5 anchor, both halves.
mState02 :: BS.ByteString -> BS.ByteString -> PD.Data
mState02 txId wsHash = PD.Constr 0 [PD.B txId, PD.B wsHash]

mState03 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
mState03 signerHash txId wsHash = PD.Constr 0 [PD.B signerHash, PD.B txId, PD.B wsHash]

mState04 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
mState04 vkey txId wsHash = PD.Constr 0 [PD.B vkey, PD.B txId, PD.B wsHash]

-- | 'txUnsigned''s own witness-set hash, which its thread carries throughout.
unsignedWsHash :: BS.ByteString
unsignedWsHash = witnessSetHashOf txUnsigned

--------------------------------------------------------------------------------
-- Driving missing-signature
--------------------------------------------------------------------------------

missing01, missing02, missing03, missing04 :: forall s. ScriptContext -> Term s PUnit
missing01 ctx =
  missingSignatureStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
missing02 ctx =
  missingSignatureStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
missing03 ctx =
  missingSignatureStep03Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pconstant ctx
missing04 ctx =
  missingSignatureStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

data Missing01 = Missing01
  { m1OutputScript :: BS.ByteString
  , m1OutputState :: Maybe PD.Data
  , m1PhasRoot :: BS.ByteString
  }

defaultMissing01 :: Missing01
defaultMissing01 =
  Missing01
    { m1OutputScript = nextScript
    , m1OutputState = Just (mState02 txUnsignedId unsignedWsHash)
    , m1PhasRoot = phasRoot
    }

mContext01 :: Missing01 -> ScriptContext
mContext01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [bareInclusionArgs txUnsignedId txUnsignedCbor (m1PhasRoot s)])
    [threadInput]
    [stepOutput (m1OutputScript s) (m1OutputState s)]
    referenceInputs
    [phasEntry (m1PhasRoot s) txUnsignedId txUnsignedCbor]
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

data Missing02 = Missing02
  { m2OpeningCbor :: BS.ByteString
  , m2Preimage :: Maybe BS.ByteString
  , m2SignerIndex :: Integer
  , m2OutputScript :: BS.ByteString
  , m2OutputState :: Maybe PD.Data
  }

defaultMissing02 :: Missing02
defaultMissing02 =
  Missing02
    { m2OpeningCbor = txUnsignedCbor
    , m2Preimage = Nothing
    , m2SignerIndex = 0
    , m2OutputScript = nextScript
    , m2OutputState = Just (mState03 (keyHashFor 1) txUnsignedId unsignedWsHash)
    }

mContext02 :: Missing02 -> ScriptContext
mContext02 s =
  spendContext
    (stepDatum (Just (mState02 txUnsignedId unsignedWsHash)))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , bodyOpening (m2OpeningCbor s) preimage
            , PD.I (m2SignerIndex s)
            ]
        ]
    )
    [threadInput]
    [stepOutput (m2OutputScript s) (m2OutputState s)]
    referenceInputs
    []
    mempty
  where
    preimage = maybe (requiredSignersPreimage txUnsigned) id (m2Preimage s)

data Missing03 = Missing03
  { m3Vkey :: BS.ByteString
  , m3OutputScript :: BS.ByteString
  , m3OutputState :: Maybe PD.Data
  }

defaultMissing03 :: Missing03
defaultMissing03 =
  Missing03
    { m3Vkey = verKeyFor 1
    , m3OutputScript = nextScript
    , m3OutputState = Just (mState04 (verKeyFor 1) txUnsignedId unsignedWsHash)
    }

mContext03 :: Missing03 -> ScriptContext
mContext03 s =
  spendContext
    (stepDatum (Just (mState03 (keyHashFor 1) txUnsignedId unsignedWsHash)))
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B (m3Vkey s)]])
    [threadInput]
    [stepOutput (m3OutputScript s) (m3OutputState s)]
    referenceInputs
    []
    mempty

data Missing04 = Missing04
  { m4StateVkey :: BS.ByteString
  , m4StateTxId :: BS.ByteString
  , m4StateWsHash :: BS.ByteString
  , m4OpeningCbor :: BS.ByteString
  , m4WitnessSetOf :: Tx
  , m4Preimage :: Maybe BS.ByteString
  , m4FraudProofAddress :: Address
  , m4FraudProofName :: BS.ByteString
  }

defaultMissing04 :: Missing04
defaultMissing04 =
  Missing04
    { m4StateVkey = verKeyFor 1
    , m4StateTxId = txUnsignedId
    , m4StateWsHash = unsignedWsHash
    , m4OpeningCbor = txUnsignedCbor
    , m4WitnessSetOf = txUnsigned
    , m4Preimage = Nothing
    , m4FraudProofAddress = fraudProofAddress
    , m4FraudProofName = threadName
    }

mContext04 :: Missing04 -> ScriptContext
mContext04 s =
  spendContext
    (stepDatum (Just (mState04 (m4StateVkey s) (m4StateTxId s) (m4StateWsHash s))))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , PD.I 0
            , witnessOpening (m4OpeningCbor s) (m4WitnessSetOf s) preimage
            ]
        ]
    )
    [threadInput]
    [convictionOutput (m4FraudProofAddress s) (m4FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (m4FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (m4FraudProofName s))) 1)
  where
    preimage = maybe (addressWitnessesPreimage (m4WitnessSetOf s)) id (m4Preimage s)

--------------------------------------------------------------------------------
-- Driving invalid-signature
--------------------------------------------------------------------------------

invalid01, invalid02 :: forall s. ScriptContext -> Term s PUnit
invalid01 ctx =
  invalidSignatureStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
invalid02 ctx =
  invalidSignatureStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

data Invalid01 = Invalid01
  { i1OutputScript :: BS.ByteString
  , i1OutputState :: Maybe PD.Data
  , i1PhasRoot :: BS.ByteString
  }

defaultInvalid01 :: Invalid01
defaultInvalid01 =
  Invalid01
    { i1OutputScript = nextScript
    , i1OutputState = Just (mState02 txBadSigId (witnessSetHashOf txBadSig))
    , i1PhasRoot = phasRoot
    }

iContext01 :: Invalid01 -> ScriptContext
iContext01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [bareInclusionArgs txBadSigId txBadSigCbor (i1PhasRoot s)])
    [threadInput]
    [stepOutput (i1OutputScript s) (i1OutputState s)]
    referenceInputs
    [phasEntry (i1PhasRoot s) txBadSigId txBadSigCbor]
    mempty

data Invalid02 = Invalid02
  { i2StateTxId :: BS.ByteString
  , i2StateWsHash :: BS.ByteString
  , i2OpeningCbor :: BS.ByteString
  , i2WitnessSetOf :: Tx
  , i2Preimage :: Maybe BS.ByteString
  , i2WitnessIndex :: Integer
  , i2FraudProofAddress :: Address
  , i2FraudProofName :: BS.ByteString
  }

defaultInvalid02 :: Invalid02
defaultInvalid02 =
  Invalid02
    { i2StateTxId = txBadSigId
    , i2StateWsHash = witnessSetHashOf txBadSig
    , i2OpeningCbor = txBadSigCbor
    , i2WitnessSetOf = txBadSig
    , i2Preimage = Nothing
    , i2WitnessIndex = 0
    , i2FraudProofAddress = fraudProofAddress
    , i2FraudProofName = threadName
    }

iContext02 :: Invalid02 -> ScriptContext
iContext02 s =
  spendContext
    (stepDatum (Just (mState02 (i2StateTxId s) (i2StateWsHash s))))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , witnessOpening (i2OpeningCbor s) (i2WitnessSetOf s) preimage
            , PD.I (i2WitnessIndex s)
            , PD.I 0
            ]
        ]
    )
    [threadInput]
    [convictionOutput (i2FraudProofAddress s) (i2FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (i2FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (i2FraudProofName s))) 1)
  where
    preimage = maybe (addressWitnessesPreimage (i2WitnessSetOf s)) id (i2Preimage s)
