{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsDaHashPreimage
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/da-hash-preimage/step-0{1,2}.ak@.

The family that proves a block committed a leaf nothing else can open: a
@transactions_root@ entry whose key is not the canonical native-V1 transaction id
of its own value.

It is the odd one out in two ways, and the suite is organised around both.

__Step-01 must not run the codec precondition.__ Every other native family opens
its transaction through @verify_native_tx_compact_cbor_v1@, which requires
@derived_id == key@. Here that equality is the thing in dispute, so running it
would make a violating leaf /abort/ the step rather than be convicted by it. The
cases below therefore include leaves that no other family's step-01 would accept
— a genuine transaction committed under a foreign key, and bytes that are not a
transaction at all — and both are expected to bind.

__The derivation is arithmetic, not decoding.__ The canonical compact encoding is
fixed-framed: two bytes of head, thirty-five of tail, so an honest leaf's body
preimage is exactly @slice(value, 2, len - 37)@. The framing constants are pinned
against the fixture's own encoder below rather than trusted, because the whole
soundness argument rests on them: get the tail wrong and an honest leaf derives a
foreign id, which would convict every operator in the network.
-}
module Testing.FraudProofsDaHashPreimage (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.DaHashPreimage (
  daHashPreimageStep01Validator,
  daHashPreimageStep02Validator,
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
    "DA Hash Preimage Fraud Proof Tests"
    [ testGroup "the framing constants" framingTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    ]

--------------------------------------------------------------------------------
-- The framing constants
--------------------------------------------------------------------------------

{- | Both constants pinned against the fixture's canonical encoder, which is
written from §3 and not taken from the port.
-}
framingTests :: [TestTree]
framingTests =
  [ {- @0x84@ and the version byte in front, @0x58 0x20@ plus 32 bytes of
       witness-set hash plus the one-byte validity code behind. The validity code
       is bounded to @0..=5@, so it never widens past one byte and the tail is a
       constant rather than a parse. -}
    testCase "the canonical frame is 2 + 35 bytes around the body" $
      BS.length tx1Cbor @?= BS.length (compactBodyOf tx1) + 37
  , testCase "the framed slice recovers the encoder's body preimage" $
      leafBody tx1Cbor @?= compactBodyOf tx1
  , testCase "so the derived id is the genuine transaction id" $
      derivedId tx1Cbor @?= tx1Id
  , -- Every fixture transaction, because the body length varies between them and
    -- a constant that happened to fit one would be worth nothing.
    testCase "and likewise for every fixture transaction" $
      map derivedId [tx1Cbor, tx2Cbor, tx3Cbor, txEmptyCbor, txScriptSpendCbor]
        @?= [tx1Id, tx2Id, tx3Id, txEmptyId, txScriptSpendId]
  , testCase "a leaf shorter than the frame clamps to the empty body" $
      leafBody (BS.replicate 36 0x00) @?= ""
  ]

--------------------------------------------------------------------------------
-- step-01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds an honest leaf and forwards the evidence triple" $
      psucceeds $ step01 (context01 default01)
  , {- The case that separates this family from every other native one. A leaf
       committed under a key that is not its own id would fail
       @verify_native_tx_compact_cbor_v1@, so no other step-01 could bind it —
       and this one must, or the fault would be unprovable. -}
    testCase "binds a genuine transaction committed under a foreign key" $
      psucceeds $ step01 (context01 (leafUnder foreignKey tx1Cbor))
  , -- …and bytes that are not a transaction at all, for the same reason.
    testCase "binds arbitrary bytes committed as a leaf" $
      psucceeds $ step01 (context01 (leafUnder foreignKey junkLeaf))
  , -- …including a leaf too short to carry the frame, whose body clamps empty.
    testCase "binds an underframed leaf" $
      psucceeds $ step01 (context01 (leafUnder foreignKey shortLeaf))
  , testCase "rejects a state naming a derived id the leaf does not commit" $
      pfails $
        step01
          (context01 default01 {d1OutputState = Just (state02 tx1Id tx2Id (leafLength tx1Cbor))})
  , testCase "rejects a state naming a committed key that is not the leaf's" $
      pfails $
        step01
          (context01 default01 {d1OutputState = Just (state02 tx2Id tx1Id (leafLength tx1Cbor))})
  , testCase "rejects a state naming the wrong byte count" $
      pfails $
        step01
          (context01 default01 {d1OutputState = Just (state02 tx1Id tx1Id (leafLength tx1Cbor - 1))})
  , testCase "rejects a leaf the block's transactions root does not commit" $
      pfails $ step01 (context01 default01 {d1PhasRoot = otherRoot})
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ step01 (context01 default01 {d1OutputScript = otherScript})
  ]

--------------------------------------------------------------------------------
-- step-02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ testCase "convicts a leaf committed under a key that is not its own id" $
      psucceeds $ step02 (context02 (adjudicate foreignKey tx1Id (leafLength tx1Cbor)))
  , {- Underframing convicts on its own, and short-circuits: a leaf too short to
       frame has no meaningful derived id, so the two ids agreeing here is not
       evidence of anything and must not rescue it. -}
    testCase "convicts an underframed leaf even when the two ids agree" $
      psucceeds $ step02 (context02 (adjudicate tx1Id tx1Id 36))
  , testCase "convicts an empty leaf" $
      psucceeds $ step02 (context02 (adjudicate tx1Id tx1Id 0))
  , -- Where an honest block survives the challenge. This is the direction that
    -- matters most: a wrong tail constant would convict here.
    testCase "refuses an honest leaf" $
      pfails $ step02 (context02 (adjudicate tx1Id tx1Id (leafLength tx1Cbor)))
  , testCase "refuses an honest leaf at exactly the frame length" $
      pfails $ step02 (context02 (adjudicate tx1Id tx1Id 37))
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $
        step02
          ( context02
              (adjudicate foreignKey tx1Id (leafLength tx1Cbor)) {d2FraudProofAddress = otherAddress}
          )
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $
        step02
          ( context02
              (adjudicate foreignKey tx1Id (leafLength tx1Cbor)) {d2FraudProofName = otherThreadName}
          )
  ]

--------------------------------------------------------------------------------
-- The rule, reimplemented from the format
--------------------------------------------------------------------------------

-- | @rule.compact_v1_frame_byte_count@: 2 bytes of head, 35 of tail.
frameByteCount :: Int
frameByteCount = 37

-- | @rule.committed_leaf_body_cbor_v1@, clamping rather than erroring.
leafBody :: BS.ByteString -> BS.ByteString
leafBody value
  | bodyLength <= 0 = ""
  | otherwise = BS.take bodyLength (BS.drop 2 value)
  where
    bodyLength = BS.length value - frameByteCount

-- | @rule.derive_committed_leaf_tx_id_v1@ — §3's id over the framed slice.
derivedId :: BS.ByteString -> BS.ByteString
derivedId value = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> leafBody value)

leafLength :: BS.ByteString -> Integer
leafLength = fromIntegral . BS.length

{- | The fixture's compact body, recovered from the compact bytes it builds.

Taken this way rather than exported separately so that the framing assertion
above is a statement about the /encoder's/ output and not about a second copy of
the same arithmetic.
-}
compactBodyOf :: Tx -> BS.ByteString
compactBodyOf tx = BS.take (BS.length cbor - frameByteCount) (BS.drop 2 cbor)
  where
    cbor = compactOf tx

--------------------------------------------------------------------------------
-- Leaves
--------------------------------------------------------------------------------

-- | A key no fixture transaction hashes to.
foreignKey :: BS.ByteString
foreignKey = BS.replicate 32 0x99

-- | Bytes long enough to frame, and not a transaction.
junkLeaf :: BS.ByteString
junkLeaf = BS.replicate 80 0x5a

-- | Too short to carry the canonical frame at all.
shortLeaf :: BS.ByteString
shortLeaf = BS.replicate 20 0x5b

--------------------------------------------------------------------------------
-- Thread state
--------------------------------------------------------------------------------

state02 :: BS.ByteString -> BS.ByteString -> Integer -> PD.Data
state02 committed derived byteCount =
  PD.Constr 0 [PD.B committed, PD.B derived, PD.I byteCount]

--------------------------------------------------------------------------------
-- Driving the validators
--------------------------------------------------------------------------------

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  daHashPreimageStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  daHashPreimageStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

--------------------------------------------------------------------------------
-- step-01's context
--------------------------------------------------------------------------------

data Step01 = Step01
  { d1Key :: BS.ByteString
  , d1Value :: BS.ByteString
  , d1OutputScript :: BS.ByteString
  , d1OutputState :: Maybe PD.Data
  , d1PhasRoot :: BS.ByteString
  }

default01 :: Step01
default01 = leafUnder tx1Id tx1Cbor

{- | A step-01 case binding the given @(key, value)@ leaf, with the state step-01
is supposed to write for it.
-}
leafUnder :: BS.ByteString -> BS.ByteString -> Step01
leafUnder key value =
  Step01
    { d1Key = key
    , d1Value = value
    , d1OutputScript = nextScript
    , d1OutputState = Just (state02 key (derivedId value) (leafLength value))
    , d1PhasRoot = phasRoot
    }

context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [bareInclusionArgs (d1Key s) (d1Value s) (d1PhasRoot s)])
    [threadInput]
    [stepOutput (d1OutputScript s) (d1OutputState s)]
    referenceInputs
    [phasEntry (d1PhasRoot s) (d1Key s) (d1Value s)]
    mempty

--------------------------------------------------------------------------------
-- step-02's context
--------------------------------------------------------------------------------

data Step02 = Step02
  { d2State :: PD.Data
  , d2FraudProofAddress :: Address
  , d2FraudProofName :: BS.ByteString
  }

-- | A step-02 case adjudicating the given evidence triple.
adjudicate :: BS.ByteString -> BS.ByteString -> Integer -> Step02
adjudicate committed derived byteCount =
  Step02
    { d2State = state02 committed derived byteCount
    , d2FraudProofAddress = fraudProofAddress
    , d2FraudProofName = threadName
    }

context02 :: Step02 -> ScriptContext
context02 s =
  spendContext
    (stepDatum (Just (d2State s)))
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
    [threadInput]
    [convictionOutput (d2FraudProofAddress s) (d2FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (d2FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (d2FraudProofName s))) 1)
