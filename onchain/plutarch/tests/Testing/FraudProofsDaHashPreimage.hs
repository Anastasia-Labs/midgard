{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsDaHashPreimage (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofs.DaHashPreimage (
  PCompactInspectionV1,
  PVerdictV1 (..),
  pfieldLengthsAreCanonicalV1,
  pinspectCompactV1,
  pwitnessSetIsCanonicalV1,
 )
import Midgard.Validators.FraudProofs.DaHashPreimage (
  daHashPreimageStep01Validator,
  daHashPreimageStep02Validator,
 )
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "DA Hash Preimage Fraud Proof Tests"
    [ testGroup "total source rule" ruleTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    ]

ruleTests :: [TestTree]
ruleTests =
  [ testCase "maximum Cardano compact is canonical" $
      psucceeds (compactInspection maximumCardanoCompactCbor)
  , testCase "maximum Cardano witness set is canonical" $
      passertEval $ pwitnessSetIsCanonicalV1 # pconstant maximumCardanoWitnessSetCbor
  , testCase "maximum Cardano field lengths are canonical" $
      passertEval $ pfieldLengthsAreCanonicalV1 # pconstant maximumCardanoFieldLengthsCbor
  , testCase "noncanonical compact encoding is rejected" $
      pfails (compactInspection noncanonicalCompactCbor)
  , testCase "malformed witness set is rejected without aborting" $
      passertEval $ pnot # (pwitnessSetIsCanonicalV1 # pconstant "deadbeef")
  , testCase "malformed field lengths are rejected without aborting" $
      passertEval $ pnot # (pfieldLengthsAreCanonicalV1 # pconstant "deadbeef")
  ]
  where
    compactInspection :: forall s. BS.ByteString -> Term s PUnit
    compactInspection bytes =
      pmatch (pinspectCompactV1 # pconstant bytes) $ \case
        PNothing -> perror
        PJust (_ :: Term s PCompactInspectionV1) -> pconstant ()

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds a miskeyed source leaf" $
      psucceeds $ step01 (context01 $ leafUnder foreignKey (sourceCbor tx1Id tx1Cbor validWitness validLengths))
  , testCase "binds a malformed source leaf" $
      psucceeds $ step01 (context01 $ leafUnder foreignKey "deadbeef")
  , testCase "binds a noncanonical source leaf" $
      psucceeds $ step01 (context01 $ leafUnder tx1Id noncanonicalSourceCbor)
  , testCase "binds a malformed compact proof source" $
      psucceeds $ step01 (context01 $ leafUnder tx1Id (sourceCbor tx1Id "deadbeef" validWitness validLengths))
  , testCase "binds a malformed witness proof source" $
      psucceeds $ step01 (context01 $ leafUnder tx1Id (sourceCbor tx1Id tx1Cbor "deadbeef" validLengths))
  , testCase "binds a forged witness-set hash" $
      psucceeds $ step01 (context01 $ leafUnder tx1Id (sourceCbor tx1Id tx1Cbor forgedWitness validLengths))
  , testCase "binds malformed field lengths" $
      psucceeds $ step01 (context01 $ leafUnder tx1Id (sourceCbor tx1Id tx1Cbor validWitness "deadbeef"))
  , testCase "binds a derived-id mismatch" $
      psucceeds $ step01 (context01 $ leafUnder foreignKey (sourceCbor foreignKey tx1Cbor validWitness validLengths))
  , testCase "binds a valid leaf without convicting" $
      psucceeds $ step01 (context01 default01)
  , testCase "accepts the maximum valid source leaf" $
      psucceeds $ step01 (context01 maximum01)
  , testCase "rejects a fabricated verdict" $
      pfails $ step01 (context01 default01 {d1OutputState = Just (state02 PKeyMismatch)})
  , testCase "rejects a forged transactions root" $
      pfails $ step01 (context01 default01 {d1PhasRoot = otherRoot})
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ step01 (context01 default01 {d1OutputScript = otherScript})
  ]

step02Tests :: [TestTree]
step02Tests =
  [ testCase "convicts malformed source" $ psucceeds $ step02 (context02 $ adjudicate PMalformedSource)
  , testCase "convicts key mismatch" $ psucceeds $ step02 (context02 $ adjudicate PKeyMismatch)
  , testCase "convicts malformed proof source" $ psucceeds $ step02 (context02 $ adjudicate PMalformedProofSource)
  , testCase "convicts derived-id mismatch" $ psucceeds $ step02 (context02 $ adjudicate PDerivedIdMismatch)
  , testCase "rejects a valid-block challenge" $ pfails $ step02 (context02 $ adjudicate PNoViolation)
  , testCase "rejects a conviction parked outside the fraud-proof address" $
      pfails $ step02 (context02 (adjudicate PKeyMismatch) {d2FraudProofAddress = otherAddress})
  , testCase "rejects a conviction under a name other than the thread's" $
      pfails $ step02 (context02 (adjudicate PKeyMismatch) {d2FraudProofName = otherThreadName})
  ]

--------------------------------------------------------------------------------
-- Canonical source fixtures
--------------------------------------------------------------------------------

validWitness, validLengths :: BS.ByteString
validWitness = witnessSetCborOf tx1
validLengths = "\x89\x00\x00\x00\x00\x00\x00\x00\x00\x00"

forgedWitness :: BS.ByteString
forgedWitness = witnessSetCborFrom (foreignKey, foreignKey, foreignKey)

sourceCbor :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
sourceCbor txId compact witness lengths =
  serialise $ PD.Constr 0 [PD.B txId, PD.Constr 0 [PD.B compact, PD.B witness, PD.B lengths]]

-- Same Data as 'sourceCbor', with definite constructor-field lists instead of
-- the canonical serialiseData spelling.
noncanonicalSourceCbor :: BS.ByteString
noncanonicalSourceCbor =
  "\xd8\x79\x82"
    <> definiteBytes tx1Id
    <> "\xd8\x79\x83"
    <> definiteBytes tx1Cbor
    <> definiteBytes validWitness
    <> definiteBytes validLengths

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | n <= 23 = BS.cons (fromIntegral $ 0x40 + n) bytes
  | n <= 255 = BS.pack [0x58, fromIntegral n] <> bytes
  | otherwise = BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral n] <> bytes
  where
    n = BS.length bytes

noncanonicalCompactCbor :: BS.ByteString
noncanonicalCompactCbor = "\x9f" <> BS.drop 1 tx1Cbor <> "\xff"

foreignKey :: BS.ByteString
foreignKey = BS.replicate 32 0x99

maximumCardanoTxId, maximumCardanoCompactCbor, maximumCardanoWitnessSetCbor, maximumCardanoFieldLengthsCbor :: BS.ByteString
maximumCardanoTxId = hex "7b4e4657e0083544359f4398fb092c482766220cd53ad99b598239297d1e9813"
maximumCardanoCompactCbor = hex "84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820095c12f5790acc50dbbf52c0b47fe4ebd1dfd9ab308b14701543d6d4d78a06ae1a000d59492020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820e2d5bb3b4c4475d516516e5396ec041b553ada06379a53f665b47e1485e0451f582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820ad12ff89400f2f7975c77231241032e6a7bf49d0f2ab388425b3de0cefef003000"
maximumCardanoWitnessSetCbor = hex "835820689afcab7a4406fa8da9a4f97b325f34458bd2114d6b2ae9eb357e681acc0e97582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
maximumCardanoFieldLengthsCbor = hex "89182901183001190e8a01011931e601"

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BS8.pack

--------------------------------------------------------------------------------
-- Validator contexts
--------------------------------------------------------------------------------

state02 :: PVerdictV1 s -> PD.Data
state02 verdict = PD.Constr 0 [verdictData verdict]

verdictData :: PVerdictV1 s -> PD.Data
verdictData verdict = PD.Constr tag []
  where
    tag = case verdict of
      PMalformedSource -> 0
      PKeyMismatch -> 1
      PMalformedProofSource -> 2
      PDerivedIdMismatch -> 3
      PNoViolation -> 4

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

data Step01 = Step01
  { d1Key :: BS.ByteString
  , d1Value :: BS.ByteString
  , d1OutputScript :: BS.ByteString
  , d1OutputState :: Maybe PD.Data
  , d1PhasRoot :: BS.ByteString
  }

default01, maximum01 :: Step01
default01 = leafUnder tx1Id (sourceCbor tx1Id tx1Cbor validWitness validLengths)
maximum01 =
  leafUnder
    maximumCardanoTxId
    (sourceCbor maximumCardanoTxId maximumCardanoCompactCbor maximumCardanoWitnessSetCbor maximumCardanoFieldLengthsCbor)

leafUnder :: BS.ByteString -> BS.ByteString -> Step01
leafUnder key value =
  Step01
    { d1Key = key
    , d1Value = value
    , d1OutputScript = nextScript
    , d1OutputState = Just (stateForLeaf key value)
    , d1PhasRoot = phasRoot
    }

stateForLeaf :: BS.ByteString -> BS.ByteString -> PD.Data
stateForLeaf committed value
  | value == "deadbeef" = state02 PMalformedSource
  | value == noncanonicalSourceCbor = state02 PMalformedSource
  | committed == foreignKey && value == sourceCbor tx1Id tx1Cbor validWitness validLengths = state02 PKeyMismatch
  | committed == foreignKey = state02 PDerivedIdMismatch
  | value == sourceCbor tx1Id tx1Cbor validWitness validLengths = state02 PNoViolation
  | value == sourceCbor maximumCardanoTxId maximumCardanoCompactCbor maximumCardanoWitnessSetCbor maximumCardanoFieldLengthsCbor = state02 PNoViolation
  | otherwise = state02 PMalformedProofSource

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

data Step02 = Step02
  { d2State :: PD.Data
  , d2FraudProofAddress :: Address
  , d2FraudProofName :: BS.ByteString
  }

adjudicate :: PVerdictV1 s -> Step02
adjudicate verdict =
  Step02
    { d2State = state02 verdict
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
