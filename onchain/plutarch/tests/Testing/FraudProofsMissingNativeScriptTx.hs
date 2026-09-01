{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsMissingNativeScriptTx
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/missing-native-script-tx/step-0{1..6}.ak@.

The longest chain in the machine, and the only family that binds __two__
transactions: the one that spends a script-locked output, and the one that
produced it.

Three things this suite is built to catch, none of which a shorter family could
show:

* __The subject changes at step-03.__ From step-04 on the field openings are
  against the /producing/ transaction, not the bad one, and both ids travel
  because step-06 goes back. A step-03 that forgot to check its newly verified id
  against the named input would let a prover bind any committed transaction and
  read its slot 0 instead.

* __An id does not pin a witness set.__ 'txScriptSpend' and its witnessed
  variant have __byte-identical bodies__, so §3 gives them __the same
  transaction id__ and they differ only in the compact structure's trailing
  @witness_set_hash@. That is not a contrivance for the test — it is exactly the
  substitution §3 permits, and it makes the pair the sharpest available
  demonstration that the anchor needs both halves. Convicting the honest one with
  the guilty one's empty witness set is driven below, and refused.

* __A prefix decoder is not a canonical decoder.__ Step-06 re-encodes each item
  and compares. Two committed items decoding to the same script — one with
  trailing bytes, one with a non-minimal length prefix — would make the hash a
  statement about something the field never committed.
-}
module Testing.FraudProofsMissingNativeScriptTx (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.MissingNativeScriptTx (
  missingNativeScriptTxStep01Validator,
  missingNativeScriptTxStep02Validator,
  missingNativeScriptTxStep03Validator,
  missingNativeScriptTxStep04Validator,
  missingNativeScriptTxStep05Validator,
  missingNativeScriptTxStep06Validator,
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
    "Missing Native Script Fraud Proof Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    , testGroup "step-03" step03Tests
    , testGroup "step-04" step04Tests
    , testGroup "step-05" step05Tests
    , testGroup "step-06" step06Tests
    , testGroup "the two forgeries" forgeryTests
    ]

--------------------------------------------------------------------------------
-- The fixture
--------------------------------------------------------------------------------

{- | The reference encoders, checked against the format rather than against the
port — and against each other, where two of them are supposed to agree.
-}
fixtureTests :: [TestTree]
fixtureTests =
  [ testCase "slot 0 of every fixture transaction is locked by the native script" $
      outputItem 0 @?= midgardOutputCbor (scriptAddressBytes lockedScriptHash) 2_000_000 Nothing
  , testCase "slot 1 is locked by a key, so a step reading it finds no script" $
      outputItem 1
        @?= midgardOutputCbor
          (pubKeyAddressBytes (keyHashFor 1))
          2_000_001
          (Just (BS.replicate 4 0xc1))
  , -- The two address headers are @address_type * 16 + network_id@, types 7 and
    -- 6, which is the only thing distinguishing a script-locked output from a
    -- key-locked one at the byte level.
    testCase "the two address headers are 0x70 and 0x60" $
      (BS.head (scriptAddressBytes lockedScriptHash), BS.head (pubKeyAddressBytes (keyHashFor 1)))
        @?= (0x70, 0x60)
  , -- §5.3 field 2 is variable-width by construction, and the fixture has to be
    -- too or the walk would be indistinguishable from a stride multiplication.
    testCase "field 2's items are of unequal width" $
      assertBool "outputs are all the same size" $
        BS.length (outputItem 0) /= BS.length (outputItem 1)
  , testCase "the versioned-script hash prepends the language tag" $
      versionedScriptHashOf 0 nativeScriptBytes @?= blake2b224 (BS.cons 0x00 nativeScriptBytes)
  , -- The tag is the language tag, not the constructor index. For native scripts
    -- they coincide, which is why the fixture pins a second tag as well.
    testCase "a different language tag is a different hash" $
      assertBool "tags 0 and 3 hash alike" $
        versionedScriptHashOf 0 nativeScriptBytes /= versionedScriptHashOf 3 nativeScriptBytes
  , testCase "a one-script field-6 preimage is the §5.1 envelope over one item" $
      scriptWitnessCollectionPreimage [(0, nativeScriptBytes)]
        @?= arrayHeader 1 <> wrapItem (versionedScriptItem 0 nativeScriptBytes)
  , testCase "the bad transaction witnesses no script at all" $
      scriptWitnessesPreimage txScriptSpend @?= arrayHeader 0
  , testCase "the bad transaction spends slot 0 of tx3" $
      spendInputsOf txScriptSpend @?= [(tx3Id, 0)]
  , {- The fact the whole family's anchor exists for. §3's id preimage is the
       body, and these two transactions have the same body — so they have the
       same id and differ only in a tail the id does not reach. -}
    testCase "the witnessed variant has the same transaction id" $
      txWitnessedId @?= txScriptSpendId
  , testCase "…and a different witness-set hash" $
      assertBool "the two witness sets hash alike" $
        witnessSetHashOf txWitnessed /= witnessSetHashOf txScriptSpend
  ]

--------------------------------------------------------------------------------
-- step-01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds the transaction and writes both halves of the anchor" $
      psucceeds $ step01 (context01 default01)
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ step01 (context01 default01 {s1OutputScript = otherScript})
  , -- The half only step-01 can supply honestly: it is read off the compact
    -- structure the block's counted root committed, not taken from a redeemer.
    testCase "rejects a state naming another witness-set hash" $
      pfails $
        step01
          ( context01
              default01
                {s1OutputState = Just (state02 txScriptSpendId (witnessSetHashOf tx1))}
          )
  , testCase "rejects a state naming another transaction" $
      pfails $
        step01
          (context01 default01 {s1OutputState = Just (state02 tx1Id badWsHash)})
  , testCase "rejects an inclusion proof against a root the header does not commit" $
      pfails $ step01 (context01 default01 {s1PhasRoot = otherRoot})
  , {- Recorded because the opposite is the natural guess. Step-01 binds its
       input state and never looks at it — Aiken's does the same, naming it
       @_m_input_state_data@ — so a thread arriving with state already written
       passes. What keeps that from mattering is upstream: a thread only reaches
       step-01 through the computation-thread policy's mint, which is what fixes
       the initial state. The step is not the place that check lives, and adding
       one here would be a divergence rather than a hardening. -}
    testCase "ignores a state already written into the thread" $
      psucceeds $
        step01 (context01 default01 {s1InputState = Just (state02 txScriptSpendId badWsHash)})
  ]

--------------------------------------------------------------------------------
-- step-02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ testCase "forwards the spent input alongside both halves of the anchor" $
      psucceeds $ step02 (context02 default02)
  , testCase "rejects a preimage the transaction never committed" $
      pfails $
        step02
          ( context02
              default02
                { s2Preimage = Just (spendInputsPreimage tx1)
                , s2OutputState = Just (state03 (inputData sharedInputRef) txScriptSpendId badWsHash)
                }
          )
  , -- §4 removed field-index domain separation, so the slot is the door's
    -- argument rather than anything the preimage says about itself. Handing it
    -- the reference-input collection is a read of the wrong §2.5 slot.
    testCase "rejects field 1's preimage in field 0's slot" $
      pfails $ step02 (context02 default02 {s2Preimage = Just (referenceInputsPreimage tx3)})
  , testCase "rejects an index past the end of the collection" $
      pfails $ step02 (context02 default02 {s2InputIndex = 1})
  , testCase "rejects a state that drops the witness-set hash" $
      pfails $
        step02
          ( context02
              default02
                { s2OutputState =
                    Just (state03 (inputData (tx3Id, 0)) txScriptSpendId (witnessSetHashOf tx1))
                }
          )
  , testCase "rejects an output at a script that is not step-03's" $
      pfails $ step02 (context02 default02 {s2OutputScript = otherScript})
  ]

--------------------------------------------------------------------------------
-- step-03
--------------------------------------------------------------------------------

step03Tests :: [TestTree]
step03Tests =
  [ testCase "binds the producing transaction and carries the disputed index" $
      psucceeds $ step03 (context03 default03)
  , -- The check that makes the change of subject sound. Without it a prover
    -- binds any committed transaction and step-04 reads its slot 0 instead.
    testCase "rejects a bound transaction that is not the one the input names" $
      pfails $
        step03
          ( context03
              default03
                { s3ProducingId = tx1Id
                , s3ProducingCbor = tx1Cbor
                , s3OutputState = Just (state04 tx1Id 0 txScriptSpendId badWsHash)
                }
          )
  , testCase "rejects a state naming an output index the input did not" $
      pfails $
        step03 (context03 default03 {s3OutputState = Just (state04 tx3Id 1 txScriptSpendId badWsHash)})
  , testCase "rejects a state that drops the bad transaction's anchor" $
      pfails $
        step03
          ( context03
              default03
                {s3OutputState = Just (state04 tx3Id 0 tx3Id (witnessSetHashOf tx3))}
          )
  , testCase "rejects an output at a script that is not step-04's" $
      pfails $ step03 (context03 default03 {s3OutputScript = otherScript})
  ]

--------------------------------------------------------------------------------
-- step-04
--------------------------------------------------------------------------------

step04Tests :: [TestTree]
step04Tests =
  [ testCase "reads the spent output's script credential out of field 2" $
      psucceeds $ step04 (context04 default04)
  , -- The claim is about a /script/-locked output. A key-locked one has no
    -- script to be missing, so the family has nothing to say about it.
    testCase "refuses a key-locked output" $
      pfails $
        step04
          ( context04
              default04
                { s4StateIndex = 1
                , s4OutputState = Just (state05 (keyHashFor 1) txScriptSpendId badWsHash)
                }
          )
  , testCase "rejects a preimage the producing transaction never committed" $
      pfails $ step04 (context04 default04 {s4Preimage = Just (outputsPreimage tx1)})
  , testCase "rejects an opening of a transaction the thread did not bind" $
      pfails $ step04 (context04 default04 {s4OpeningCbor = tx1Cbor})
  , testCase "rejects a state naming a credential the output does not carry" $
      pfails $
        step04
          ( context04
              default04
                {s4OutputState = Just (state05 (keyHashFor 1) txScriptSpendId badWsHash)}
          )
  , testCase "rejects an output at a script that is not step-05's" $
      pfails $ step04 (context04 default04 {s4OutputScript = otherScript})
  ]

--------------------------------------------------------------------------------
-- step-05
--------------------------------------------------------------------------------

step05Tests :: [TestTree]
step05Tests =
  [ testCase "accepts script bytes hashing to the credential under the native tag" $
      psucceeds $ step05 (context05 default05)
  , testCase "rejects other script bytes" $
      pfails $ step05 (context05 default05 {s5ScriptBytes = otherNativeScriptBytes})
  , {- The step's whole purpose. The credential is a bare 28-byte hash and says
       nothing about which language produced it; a family that skipped this step
       would convict a transaction for not witnessing a /Plutus/ script, which is
       a different fault under a different rule. -}
    testCase "rejects a credential that is the same bytes under a Plutus tag" $
      pfails $
        step05
          ( context05
              default05
                { s5StateHash = versionedScriptHashOf 3 nativeScriptBytes
                , s5OutputState =
                    Just
                      ( state06
                          (versionedScriptHashOf 3 nativeScriptBytes)
                          txScriptSpendId
                          badWsHash
                      )
                }
          )
  , -- Step-05 establishes something about what it carries; it must not change it.
    testCase "rejects a state that changes the hash it proved" $
      pfails $
        step05
          ( context05
              default05
                {s5OutputState = Just (state06 (keyHashFor 1) txScriptSpendId badWsHash)}
          )
  , testCase "rejects an output at a script that is not step-06's" $
      pfails $ step05 (context05 default05 {s5OutputScript = otherScript})
  ]

--------------------------------------------------------------------------------
-- step-06
--------------------------------------------------------------------------------

step06Tests :: [TestTree]
step06Tests =
  [ testCase "convicts a transaction whose field 6 is empty" $
      psucceeds $ step06 (context06 default06)
  , -- Absence, not emptiness: a field carrying some other script is still a
    -- field the required one is absent from.
    testCase "convicts a transaction witnessing some other script" $
      psucceeds $
        step06
          ( context06
              default06
                { s6StateWsHash = witnessSetHashOf txOtherScript
                , s6OpeningCbor = compactWith txScriptSpend (witnessSetHashOf txOtherScript)
                , s6WitnessSetOf = txOtherScript
                }
          )
  , -- Where an honest transaction survives the challenge.
    testCase "refuses a transaction that does witness the required script" $
      pfails $
        step06
          ( context06
              default06
                { s6StateWsHash = witnessSetHashOf txWitnessed
                , s6OpeningCbor = compactWith txScriptSpend (witnessSetHashOf txWitnessed)
                , s6WitnessSetOf = txWitnessed
                }
          )
  , {- §6.1 canonicality, re-established by re-encoding. The decoder reads a
       prefix and says nothing about bytes after it, so an item with a tail
       decodes to a script the field did not commit. -}
    testCase "refuses an item carrying trailing bytes" $
      pfails $ step06 (context06 (nonCanonical (versionedScriptItem 0 nativeScriptBytes <> "\xff")))
  , -- The other half of the same guard: @58 05@ where @45@ was canonical.
    testCase "refuses an item with a non-minimal length prefix" $
      pfails $ step06 (context06 (nonCanonical ("\x82\x00\x58\x05" <> shortScript)))
  , testCase "rejects a preimage the witness set does not commit" $
      pfails $ step06 (context06 default06 {s6Preimage = Just (scriptWitnessesPreimage txWitnessed)})
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $ step06 (context06 default06 {s6FraudProofAddress = otherAddress})
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $ step06 (context06 default06 {s6FraudProofName = otherThreadName})
  ]

--------------------------------------------------------------------------------
-- The two forgeries
--------------------------------------------------------------------------------

{- | Both directions of the substitution §3's id preimage permits, driven end to
end rather than described.

'txScriptSpend' and 'txWitnessed' share a body, hence an id. A door that checked
only the id would accept either transaction's witness set against the other, and
the family's conviction would say nothing at all.
-}
forgeryTests :: [TestTree]
forgeryTests =
  [ -- The one that matters: convicting the transaction that /did/ witness the
    -- script, by handing the door the empty witness set of its twin.
    testCase "refuses the empty witness set against the honest twin's anchor" $
      pfails $
        step06
          ( context06
              default06
                { s6StateWsHash = witnessSetHashOf txWitnessed
                , s6OpeningCbor = compactWith txScriptSpend (witnessSetHashOf txWitnessed)
                , s6WitnessSetOf = txScriptSpend
                }
          )
  , -- …and the mirror, which shows the refusal is the anchor's doing and not
    -- something about the compact bytes: the same empty witness set under its
    -- own anchor convicts.
    testCase "the same witness set convicts under its own anchor" $
      psucceeds $ step06 (context06 default06)
  , -- The id is genuinely no help here, which is the point: the bytes the
    -- forgery hands the door are the honest transaction's own compact structure.
    testCase "the forged opening is the honest twin's compact structure" $
      compactWith txScriptSpend (witnessSetHashOf txWitnessed) @?= compactOf txWitnessed
  ]

--------------------------------------------------------------------------------
-- The transactions this family adds
--------------------------------------------------------------------------------

{- | 'txScriptSpend' with the required script witnessed — the honest twin.

Only 'tScripts' differs, and 'tScripts' reaches the compact structure through the
witness-set hash alone, so the body and therefore the §3 id are unchanged.
-}
txWitnessed :: Tx
txWitnessed = txScriptSpend {tScripts = [(0, nativeScriptBytes)]}

-- | A transaction witnessing a script, but not the one the output requires.
txOtherScript :: Tx
txOtherScript = txScriptSpend {tScripts = [(0, otherNativeScriptBytes)]}

txWitnessedId :: BS.ByteString
txWitnessedId = txIdOf txWitnessed

-- | Short enough that @45@ is its canonical §5.1 wrapper and @58 05@ is not.
shortScript :: BS.ByteString
shortScript = BS.replicate 5 0x99

-- | The bad transaction's own witness-set hash, which its thread carries throughout.
badWsHash :: BS.ByteString
badWsHash = witnessSetHashOf txScriptSpend

--------------------------------------------------------------------------------
-- Thread states
--------------------------------------------------------------------------------

state02 :: BS.ByteString -> BS.ByteString -> PD.Data
state02 txId wsHash = PD.Constr 0 [PD.B txId, PD.B wsHash]

state03 :: PD.Data -> BS.ByteString -> BS.ByteString -> PD.Data
state03 input txId wsHash = PD.Constr 0 [input, PD.B txId, PD.B wsHash]

state04 :: BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString -> PD.Data
state04 producingId index txId wsHash =
  PD.Constr 0 [PD.B producingId, PD.I index, PD.B txId, PD.B wsHash]

state05, state06 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
state05 scriptHash txId wsHash = PD.Constr 0 [PD.B scriptHash, PD.B txId, PD.B wsHash]
state06 = state05

--------------------------------------------------------------------------------
-- Driving the validators
--------------------------------------------------------------------------------

step01, step02, step03, step04, step05, step06 ::
  forall s. ScriptContext -> Term s PUnit
step01 ctx =
  missingNativeScriptTxStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  missingNativeScriptTxStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
step03 ctx =
  missingNativeScriptTxStep03Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step04 ctx =
  missingNativeScriptTxStep04Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
step05 ctx =
  missingNativeScriptTxStep05Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pconstant ctx
step06 ctx =
  missingNativeScriptTxStep06Validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

--------------------------------------------------------------------------------
-- step-01's context
--------------------------------------------------------------------------------

data Step01 = Step01
  { s1InputState :: Maybe PD.Data
  , s1OutputScript :: BS.ByteString
  , s1OutputState :: Maybe PD.Data
  , s1PhasRoot :: BS.ByteString
  }

default01 :: Step01
default01 =
  Step01
    { s1InputState = Nothing
    , s1OutputScript = nextScript
    , s1OutputState = Just (state02 txScriptSpendId badWsHash)
    , s1PhasRoot = phasRoot
    }

context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum (s1InputState s))
    (PD.Constr 1 [bareInclusionArgs txScriptSpendId txScriptSpendCbor (s1PhasRoot s)])
    [threadInput]
    [stepOutput (s1OutputScript s) (s1OutputState s)]
    referenceInputs
    [phasEntry (s1PhasRoot s) txScriptSpendId txScriptSpendCbor]
    mempty

--------------------------------------------------------------------------------
-- step-02's context
--------------------------------------------------------------------------------

data Step02 = Step02
  { s2OpeningCbor :: BS.ByteString
  , s2Preimage :: Maybe BS.ByteString
  , s2InputIndex :: Integer
  , s2OutputScript :: BS.ByteString
  , s2OutputState :: Maybe PD.Data
  }

default02 :: Step02
default02 =
  Step02
    { s2OpeningCbor = txScriptSpendCbor
    , s2Preimage = Nothing
    , s2InputIndex = 0
    , s2OutputScript = nextScript
    , s2OutputState = Just (state03 (inputData (tx3Id, 0)) txScriptSpendId badWsHash)
    }

context02 :: Step02 -> ScriptContext
context02 s =
  spendContext
    (stepDatum (Just (state02 txScriptSpendId badWsHash)))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , PD.I (s2InputIndex s)
            , bodyOpening (s2OpeningCbor s) preimage
            ]
        ]
    )
    [threadInput]
    [stepOutput (s2OutputScript s) (s2OutputState s)]
    referenceInputs
    []
    mempty
  where
    preimage = maybe (spendInputsPreimage txScriptSpend) id (s2Preimage s)

--------------------------------------------------------------------------------
-- step-03's context
--------------------------------------------------------------------------------

data Step03 = Step03
  { s3ProducingId :: BS.ByteString
  , s3ProducingCbor :: BS.ByteString
  , s3OutputScript :: BS.ByteString
  , s3OutputState :: Maybe PD.Data
  }

default03 :: Step03
default03 =
  Step03
    { s3ProducingId = tx3Id
    , s3ProducingCbor = tx3Cbor
    , s3OutputScript = nextScript
    , s3OutputState = Just (state04 tx3Id 0 txScriptSpendId badWsHash)
    }

context03 :: Step03 -> ScriptContext
context03 s =
  spendContext
    (stepDatum (Just (state03 (inputData (tx3Id, 0)) txScriptSpendId badWsHash)))
    (PD.Constr 1 [bareInclusionArgs (s3ProducingId s) (s3ProducingCbor s) phasRoot])
    [threadInput]
    [stepOutput (s3OutputScript s) (s3OutputState s)]
    referenceInputs
    [phasEntry phasRoot (s3ProducingId s) (s3ProducingCbor s)]
    mempty

--------------------------------------------------------------------------------
-- step-04's context
--------------------------------------------------------------------------------

data Step04 = Step04
  { s4StateIndex :: Integer
  , s4OpeningCbor :: BS.ByteString
  , s4Preimage :: Maybe BS.ByteString
  , s4OutputScript :: BS.ByteString
  , s4OutputState :: Maybe PD.Data
  }

default04 :: Step04
default04 =
  Step04
    { s4StateIndex = 0
    , s4OpeningCbor = tx3Cbor
    , s4Preimage = Nothing
    , s4OutputScript = nextScript
    , s4OutputState = Just (state05 lockedScriptHash txScriptSpendId badWsHash)
    }

context04 :: Step04 -> ScriptContext
context04 s =
  spendContext
    (stepDatum (Just (state04 tx3Id (s4StateIndex s) txScriptSpendId badWsHash)))
    ( PD.Constr
        1
        [PD.Constr 0 [PD.I 0, PD.I 0, bodyOpening (s4OpeningCbor s) preimage]]
    )
    [threadInput]
    [stepOutput (s4OutputScript s) (s4OutputState s)]
    referenceInputs
    []
    mempty
  where
    preimage = maybe (outputsPreimage tx3) id (s4Preimage s)

--------------------------------------------------------------------------------
-- step-05's context
--------------------------------------------------------------------------------

data Step05 = Step05
  { s5StateHash :: BS.ByteString
  , s5ScriptBytes :: BS.ByteString
  , s5OutputScript :: BS.ByteString
  , s5OutputState :: Maybe PD.Data
  }

default05 :: Step05
default05 =
  Step05
    { s5StateHash = lockedScriptHash
    , s5ScriptBytes = nativeScriptBytes
    , s5OutputScript = nextScript
    , s5OutputState = Just (state06 lockedScriptHash txScriptSpendId badWsHash)
    }

context05 :: Step05 -> ScriptContext
context05 s =
  spendContext
    (stepDatum (Just (state05 (s5StateHash s) txScriptSpendId badWsHash)))
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B (s5ScriptBytes s)]])
    [threadInput]
    [stepOutput (s5OutputScript s) (s5OutputState s)]
    referenceInputs
    []
    mempty

--------------------------------------------------------------------------------
-- step-06's context
--------------------------------------------------------------------------------

data Step06 = Step06
  { s6StateHash :: BS.ByteString
  , s6StateWsHash :: BS.ByteString
  , s6OpeningCbor :: BS.ByteString
  , s6WitnessSetOf :: Tx
  , s6WitnessSetHashes :: Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
  , s6Preimage :: Maybe BS.ByteString
  , s6FraudProofAddress :: Address
  , s6FraudProofName :: BS.ByteString
  }

default06 :: Step06
default06 =
  Step06
    { s6StateHash = lockedScriptHash
    , s6StateWsHash = badWsHash
    , s6OpeningCbor = txScriptSpendCbor
    , s6WitnessSetOf = txScriptSpend
    , s6WitnessSetHashes = Nothing
    , s6Preimage = Nothing
    , s6FraudProofAddress = fraudProofAddress
    , s6FraudProofName = threadName
    }

{- | A step-06 case whose field 6 commits one hand-built item.

The item is not what any canonical encoder would write, so the whole witness set
has to be built around it — which is also the only honest way to test the
re-encoding guard: an item the fixture could produce would pass it.
-}
nonCanonical :: BS.ByteString -> Step06
nonCanonical item =
  default06
    { s6StateWsHash = blake2b256 (witnessSetCborFrom hashes)
    , s6OpeningCbor = compactWith txScriptSpend (blake2b256 (witnessSetCborFrom hashes))
    , s6WitnessSetHashes = Just hashes
    , s6Preimage = Just preimage
    }
  where
    preimage = arrayHeader 1 <> wrapItem item
    hashes = (addr, blake2b256 preimage, redeemer)
    (addr, _, redeemer) = witnessSetHashesOf txScriptSpend

context06 :: Step06 -> ScriptContext
context06 s =
  spendContext
    (stepDatum (Just (state06 (s6StateHash s) txScriptSpendId (s6StateWsHash s))))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , PD.I 0
            , witnessOpeningRaw (s6OpeningCbor s) hashes preimage
            ]
        ]
    )
    [threadInput]
    [convictionOutput (s6FraudProofAddress s) (s6FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (s6FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (s6FraudProofName s))) 1)
  where
    hashes = maybe (witnessSetHashesOf (s6WitnessSetOf s)) id (s6WitnessSetHashes s)
    preimage = maybe (scriptWitnessesPreimage (s6WitnessSetOf s)) id (s6Preimage s)
