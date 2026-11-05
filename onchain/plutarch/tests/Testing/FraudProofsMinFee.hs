{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsMinFee
Description : Current-Aiken parity tests for the native-V1 min-fee family.
-}
module Testing.FraudProofsMinFee (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.MinFee (
  minFeeStep01Validator,
  minFeeStep02Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Min Fee Fraud Proof Tests"
    [ testGroup "step-01" step01Tests
    , testGroup
        "forced binding"
        [ testCase "binds an exact FeeBelowMinimum rejection" $ psucceeds $ runForcedStep01 1 (PD.Constr 1 [PD.Constr 6 []]) 1
        , testCase "refuses forced acceptance in this family" $ pfails $ runForcedStep01 0 (PD.Constr 0 []) 0
        , testCase "refuses a foreign rejection reason" $ pfails $ runForcedStep01 1 (PD.Constr 1 [PD.Constr 5 []]) 1
        , testCase "refuses acceptance direction for a rejected verdict" $ pfails $ runForcedStep01 1 (PD.Constr 1 [PD.Constr 6 []]) 0
        ]
    , testGroup "step-02" step02Tests
    ]

--------------------------------------------------------------------------------
-- Step 01: bind the current ABI into thread state
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds a valid native-V1 block fixture" $
      psucceeds $
        step01 $
          context01 default01
  , testCase "forwards the header fee schedule" $
      psucceeds $
        step01 $
          context01
            default01
              { f1MinFeeA = 44
              , f1MinFeeB = 155_381
              , f1OutputState = stateFor tx1 44 155_381
              }
  , testCase "rejects a forged transactions root" $
      pfails $
        step01 $
          context01 default01 {f1ClaimedRawRoot = otherRoot}
  , testCase "rejects a forged fee schedule in state" $
      pfails $
        step01 $
          context01 default01 {f1OutputState = stateFor tx1 0 1}
  ]

data Step01 = Step01
  { f1MinFeeA :: Integer
  , f1MinFeeB :: Integer
  , f1ClaimedRawRoot :: BS.ByteString
  , f1OutputState :: PD.Data
  }

default01 :: Step01
default01 =
  Step01
    { f1MinFeeA = 0
    , f1MinFeeB = 0
    , f1ClaimedRawRoot = honestRawRoot
    , f1OutputState = stateFor tx1 0 0
    }

context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [PD.Constr 0 [PD.Constr 0 [inclusionArgs honestTxId honestSourceCbor (f1ClaimedRawRoot s)]]])
    [threadInput]
    [stepOutput nextScript (Just $ f1OutputState s)]
    ( referenceInputsWithTransactionsRootAndMinFee
        honestCountedRoot
        (f1MinFeeA s)
        (f1MinFeeB s)
    )
    [phasEntry (f1ClaimedRawRoot s) honestTxId honestSourceCbor]
    mempty

--------------------------------------------------------------------------------
-- Step 02: authenticate all fields and price the canonical transaction
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ testCase "accepts a fee below the flat minimum" $
      psucceeds $
        step02 $
          context02 $
            normalStep02 0 (tFee tx1 + 1)
  , testCase "accepts a fee below the sized minimum" $
      psucceeds $
        step02 $
          context02 $
            normalStep02 1 (tFee tx1 - canonicalSize tx1 + 1)
  , testCase "rejects a fee exactly at the minimum" $
      pfails $
        step02 $
          context02 $
            normalStep02 0 (tFee tx1)
  , testCase "rejects a fee exactly at the sized minimum" $
      pfails $
        step02 $
          context02 $
            normalStep02 1 (tFee tx1 - canonicalSize tx1)
  , testCase "rejects a fee above the minimum" $
      pfails $
        step02 $
          context02 $
            normalStep02 0 (tFee tx1 - 1)
  , testCase "convicts wrongful fee rejection at equality" $
      psucceeds $
        step02 $
          context02 $
            forcedBoundary 6 (tFee tx1)
  , testCase "convicts wrongful fee rejection above minimum" $
      psucceeds $
        step02 $
          context02 $
            forcedBoundary 6 (tFee tx1 - 1)
  , testCase "refuses an honest fee rejection" $
      pfails $
        step02 $
          context02 $
            forcedBoundary 6 (tFee tx1 + 1)
  , testCase "refuses another rejection reason" $
      pfails $
        step02 $
          context02 $
            forcedBoundary 5 (tFee tx1)
  , testCase "refuses altered raw bytes in the rejection direction" $
      pfails $
        step02 $
          context02 $
            (forcedBoundary 6 (tFee tx1))
              { f2FieldPreimages = fieldPreimages (tx1 {tOutputCount = tOutputCount tx1 + 1})
              }
  , testCase "rejects an inflated body-field preimage" $
      pfails $
        step02 $
          context02 $
            (normalStep02 1 (tFee tx1 - canonicalSize tx1))
              { f2FieldPreimages = fieldPreimages (tx1 {tOutputCount = tOutputCount tx1 + 1})
              }
  , testCase "rejects a forged witness-set inflation" $
      pfails $
        step02 $
          context02 $
            (normalStep02 1 (tFee tx1 - canonicalSize tx1))
              { f2OpenedCompactCbor = compactCborFor fattenedWitnessTx
              , f2WitnessSet = witnessSetData fattenedWitnessTx
              , f2FieldPreimages = fieldPreimages fattenedWitnessTx
              }
  ]

data Step02 = Step02
  { f2State :: PD.Data
  , f2OpenedCompactCbor :: BS.ByteString
  , f2WitnessSet :: PD.Data
  , f2FieldPreimages :: [BS.ByteString]
  , f2FraudProofAddress :: Address
  , f2FraudProofName :: BS.ByteString
  }

normalStep02 :: Integer -> Integer -> Step02
normalStep02 minFeeA minFeeB =
  Step02
    { f2State = stateFor tx1 minFeeA minFeeB
    , f2OpenedCompactCbor = honestCompactCbor
    , f2WitnessSet = witnessSetData tx1
    , f2FieldPreimages = fieldPreimages tx1
    , f2FraudProofAddress = fraudProofAddress
    , f2FraudProofName = threadName
    }

context02 :: Step02 -> ScriptContext
context02 s =
  spendContext
    (stepDatum $ Just $ f2State s)
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , PD.I 0
            , PD.B (f2OpenedCompactCbor s)
            , f2WitnessSet s
            , PD.List [PD.Constr 0 [PD.B preimage] | preimage <- f2FieldPreimages s]
            ]
        ]
    )
    [threadInput]
    [convictionOutput (f2FraudProofAddress s) (f2FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (f2FraudProofName s)]
    (singleton fpPolicy (TokenName $ toBuiltin $ f2FraudProofName s) 1)

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  minFeeStep01Validator
    # pdata (pconstant (ScriptHash $ toBuiltin nextScript))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  minFeeStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

--------------------------------------------------------------------------------
-- Independent native-V1 producer for the current min-fee fixture
--------------------------------------------------------------------------------

emptyField :: BS.ByteString
emptyField = "\x80"

fieldPreimages :: Tx -> [BS.ByteString]
fieldPreimages tx =
  [ spendInputsPreimage tx
  , referenceInputsPreimage tx
  , outputsPreimage tx
  , emptyField
  , requiredSignersPreimage tx
  , emptyField
  , scriptWitnessesPreimage tx
  , addressWitnessesPreimage tx
  , emptyField
  ]

witnessHashes :: Tx -> (BS.ByteString, BS.ByteString, BS.ByteString)
witnessHashes tx =
  ( blake2b256 $ addressWitnessesPreimage tx
  , blake2b256 $ scriptWitnessesPreimage tx
  , blake2b256 emptyField
  )

witnessSetCbor :: Tx -> BS.ByteString
witnessSetCbor tx =
  let (addressHash, scriptHash, redeemerHash) = witnessHashes tx
   in BS.concat ["\x83", defBytes32 addressHash, defBytes32 scriptHash, defBytes32 redeemerHash]

witnessSetData :: Tx -> PD.Data
witnessSetData tx =
  let (addressHash, scriptHash, redeemerHash) = witnessHashes tx
   in PD.Constr 0 [PD.B addressHash, PD.B scriptHash, PD.B redeemerHash]

compactBodyCbor :: Tx -> BS.ByteString
compactBodyCbor tx =
  let spend = spendInputsPreimage tx
      reference = referenceInputsPreimage tx
      outputs = outputsPreimage tx
      observers = emptyField
      signers = requiredSignersPreimage tx
      mint = emptyField
   in BS.concat
        [ "\x8c"
        , defBytes32 $ blake2b256 spend
        , defBytes32 $ blake2b256 reference
        , defBytes32 $ blake2b256 outputs
        , cborInt $ tFee tx
        , cborInt $ tValidityStart tx
        , cborInt $ tValidityEnd tx
        , defBytes32 $ blake2b256 observers
        , defBytes32 $ blake2b256 signers
        , defBytes32 $ blake2b256 mint
        , defBytes32 $ hash32 0x07
        , defBytes32 $ hash32 0x08
        , cborInt 1
        ]

compactCborFor :: Tx -> BS.ByteString
compactCborFor tx =
  BS.concat
    [ "\x84"
    , cborInt 1
    , compactBodyCbor tx
    , defBytes32 $ blake2b256 $ witnessSetCbor tx
    , cborInt 0
    ]

compactDataFor :: Tx -> PD.Data
compactDataFor tx =
  let spend = spendInputsPreimage tx
      reference = referenceInputsPreimage tx
      outputs = outputsPreimage tx
      observers = emptyField
      signers = requiredSignersPreimage tx
      mint = emptyField
   in PD.Constr
        0
        [ PD.Constr
            0
            [ PD.B $ blake2b256 spend
            , PD.B $ blake2b256 reference
            , PD.B $ blake2b256 outputs
            , PD.I $ tFee tx
            , PD.I $ tValidityStart tx
            , PD.I $ tValidityEnd tx
            , PD.B $ blake2b256 observers
            , PD.B $ blake2b256 signers
            , PD.B $ blake2b256 mint
            , PD.B $ hash32 0x07
            , PD.B $ hash32 0x08
            , PD.I 1
            ]
        , PD.B $ blake2b256 $ witnessSetCbor tx
        , PD.I 0
        ]

txIdFor :: Tx -> BS.ByteString
txIdFor tx = blake2b256 $ "MidgardNativeTxBodyV1" <> cborInt 1 <> compactBodyCbor tx

stateFor :: Tx -> Integer -> Integer -> PD.Data
stateFor tx minFeeA minFeeB =
  PD.Constr
    0
    [ acceptedSubject (txIdFor tx)
    , compactDataFor tx
    , PD.I $ tFee tx
    , PD.B $ txIdFor tx
    , PD.I minFeeA
    , PD.I minFeeB
    ]

canonicalSize :: Tx -> Integer
canonicalSize tx = fromIntegral $ BS.length $ canonicalTransactionBytes tx

canonicalTransactionBytes :: Tx -> BS.ByteString
canonicalTransactionBytes tx =
  let spend = spendInputsPreimage tx
      reference = referenceInputsPreimage tx
      outputs = outputsPreimage tx
      observers = emptyField
      signers = requiredSignersPreimage tx
      mint = emptyField
      scripts = scriptWitnessesPreimage tx
      addresses = addressWitnessesPreimage tx
      redeemers = emptyField
      body =
        BS.concat
          [ "\x8c"
          , definiteBytes spend
          , definiteBytes reference
          , definiteBytes outputs
          , cborInt $ tFee tx
          , cborInt $ tValidityStart tx
          , cborInt $ tValidityEnd tx
          , definiteBytes observers
          , definiteBytes signers
          , definiteBytes mint
          , defBytes32 $ hash32 0x07
          , defBytes32 $ hash32 0x08
          , cborInt 1
          ]
      witnesses = BS.concat ["\x83", definiteBytes addresses, definiteBytes scripts, definiteBytes redeemers]
   in BS.concat ["\x84", cborInt 1, body, witnesses, cborInt 0]

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | n <= 23 = BS.cons (fromIntegral $ 0x40 + n) bytes
  | n <= 255 = BS.pack [0x58, fromIntegral n] <> bytes
  | n <= 65_535 = BS.pack [0x59, fromIntegral $ n `div` 256, fromIntegral n] <> bytes
  | otherwise = error "min-fee test fixture field exceeds the supported test encoding"
  where
    n = BS.length bytes

honestCompactCbor, honestSourceCbor, honestTxId, honestRawRoot, honestCountedRoot :: BS.ByteString
honestCompactCbor = compactCborFor tx1
honestTxId = txIdFor tx1
honestSourceCbor = sourceCborFor honestTxId honestCompactCbor (witnessSetCbor tx1) (fieldPreimageLengthsCborOf tx1)
honestRawRoot = singleEntryPhasRoot honestTxId honestSourceCbor
honestCountedRoot = commitCountedRoot transactionsDomain honestRawRoot l2Count

fattenedWitnessTx :: Tx
fattenedWitnessTx = tx1 {tWitnesses = concat $ replicate 4 $ tWitnesses tx1}

acceptedSubject :: BS.ByteString -> PD.Data
acceptedSubject txId = PD.Constr 0 [PD.I 1, PD.I 0, PD.I 0, PD.B txId, PD.B "", PD.Constr 1 []]

rejectionState :: Integer -> Integer -> PD.Data
rejectionState reason minFeeB = case stateFor tx1 0 minFeeB of
  PD.Constr 0 (_ : fields) -> PD.Constr 0 (subject : fields)
  _ -> error "min-fee fixture state"
  where
    subject = PD.Constr 0 [PD.I 1, PD.I 1, PD.I 1, PD.B honestTxId, PD.B "key", PD.Constr 0 [PD.Constr reason []]]

forcedBoundary :: Integer -> Integer -> Step02
forcedBoundary reason minFeeB = (normalStep02 0 minFeeB) {f2State = rejectionState reason minFeeB}

forcedKey :: PD.Data
forcedKey = PD.Constr 0 [PD.B $ BS.replicate 32 0x77, PD.I 0]

runForcedStep01 :: forall s. Integer -> PD.Data -> Integer -> Term s PUnit
runForcedStep01 validity verdict direction =
  step01 $
    spendContext
      (stepDatum Nothing)
      (PD.Constr 1 [PD.Constr 0 [PD.Constr 1 [PD.I 0, PD.I 0, header, membership, PD.I direction]]])
      [threadInputWithName name]
      [stepOutputWithName nextScript (Just state) name]
      []
      []
      mempty
  where
    tx = tx1
    txId = txIdFor tx
    source =
      PD.Constr
        0
        [ PD.B $ BS.init (compactCborFor tx) <> cborInt validity
        , PD.B $ witnessSetCbor tx
        , PD.B $ fieldPreimageLengthsCborOf tx
        ]
    leaf = PD.Constr 0 [PD.B txId, source, verdict]
    rawRoot = singleEntryPhasRoot (serialise forcedKey) (serialise leaf)
    root = commitCountedRoot 1 rawRoot 1
    membership = membershipProof 1 root rawRoot 1 forcedKey leaf
    header =
      PD.Constr 0 $
        [PD.B "", PD.B "", PD.B "", PD.B root]
          ++ replicate 5 (PD.B "")
          ++ [PD.I 0, PD.I 1]
          ++ replicate 11 (PD.I 0)
          ++ [PD.B "", PD.B "", PD.I 1]
    name = BS.pack [0, 0, 0, 5] <> blake2b224 (serialise header)
    reason = case verdict of
      PD.Constr 1 [r] -> Just r
      _ -> Nothing
    subject = PD.Constr 0 [PD.I 1, PD.I direction, PD.I 1, PD.B txId, PD.B $ serialise forcedKey, maybe (PD.Constr 1 []) (\r -> PD.Constr 0 [r]) reason]
    compactData = case compactDataFor tx of
      PD.Constr 0 [body, witnesses, _] -> PD.Constr 0 [body, witnesses, PD.I validity]
      _ -> error "compact fixture"
    state = PD.Constr 0 [subject, compactData, PD.I $ tFee tx, PD.B txId, PD.I 0, PD.I 0]
