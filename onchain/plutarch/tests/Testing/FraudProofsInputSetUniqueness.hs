{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsInputSetUniqueness (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Credential (..), Redeemer (..), ScriptContext (..), ScriptHash (..), ScriptPurpose (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofs.InputSetUniqueness qualified as U
import Midgard.NativeTxFieldAccess qualified as Field
import Midgard.Validators.FraudProofs.InputSetUniqueness
import Plutarch.Unsafe (punsafeCoerce)
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Input-set uniqueness fraud proof"
    ( forcedTests
        ++ [ testCase "step 01 binds an accepted duplicate-input transaction" $ psucceeds $ step01 $ context01 duplicateSpendTx 0 False
           , testCase "step 01 rejects an honestly rejected leaf" $ pfails $ step01 $ context01 duplicateSpendTx 1 False
           , testCase "step 01 rejects a forged transactions root" $ pfails $ step01 $ context01 duplicateSpendTx 0 True
           , testCase "step 01 binds a published-chunk membership proof" $ psucceeds $ step01 $ publishedContext duplicateSpendTx
           , testCase "step 01 cancels under the prover signature" $ psucceeds $ step01 cancelContext
           , testCase "step 01 rejects unsigned cancellation" $ pfails $ step01 $ withoutSignatories cancelContext
           , testCase "step 02 convicts a non-adjacent spend duplicate" $ psucceeds $ step02 $ duplicateSpendContext duplicateSpendTx 0 2 Nothing
           , testCase "step 02 convicts an adjacent spend duplicate" $ psucceeds $ step02 $ duplicateSpendContext adjacentSpendTx 0 1 Nothing
           , testCase "step 02 convicts a reference duplicate" $ psucceeds $ step02 $ duplicateReferenceContext duplicateReferenceTx 0 2
           , testCase "step 02 convicts a spend-reference overlap" $ psucceeds $ step02 $ overlapContext overlappingTx 1 1
           , testCase "step 02 rejects distinct spend inputs" $ pfails $ step02 $ duplicateSpendContext distinctTx 0 1 Nothing
           , testCase "step 02 rejects a self-comparison" $ pfails $ step02 $ duplicateSpendContext duplicateSpendTx 2 2 Nothing
           , testCase "step 02 rejects a reversed index pair" $ pfails $ step02 $ duplicateSpendContext duplicateSpendTx 2 0 Nothing
           , testCase "step 02 rejects an out-of-range index" $ pfails $ step02 $ duplicateSpendContext duplicateSpendTx 0 3 Nothing
           , testCase "step 02 rejects an overlap claim on disjoint sets" $ pfails $ step02 $ overlapContext disjointTx 0 0
           , testCase "step 02 rejects a fabricated duplicate preimage" $
               pfails $
                 step02 $
                   duplicateSpendContext distinctTx 0 1 (Just $ spendInputsPreimage adjacentSpendTx)
           , testCase "step 02 rejects a foreign anchor" $ pfails $ step02 foreignAnchorContext
           ]
    )

shared, other, third, fourth :: (BS.ByteString, Integer)
shared = sharedInputRef
other = otherInputRef
third = (hash32 0x31, 3)
fourth = (hash32 0x41, 4)

duplicateSpendTx, adjacentSpendTx, duplicateReferenceTx, overlappingTx, distinctTx, disjointTx :: Tx
duplicateSpendTx = tx1 {tSpendInputs = [shared, other, shared], tReferenceInputs = [third]}
adjacentSpendTx = tx1 {tSpendInputs = [shared, shared], tReferenceInputs = [third]}
duplicateReferenceTx = tx1 {tSpendInputs = [fourth], tReferenceInputs = [shared, other, shared]}
overlappingTx = tx1 {tSpendInputs = [other, shared], tReferenceInputs = [third, shared]}
distinctTx = tx1 {tSpendInputs = [shared, other], tReferenceInputs = [third]}
disjointTx = tx1 {tSpendInputs = [shared, other], tReferenceInputs = [third, fourth]}

acceptedCbor :: Tx -> BS.ByteString
acceptedCbor tx = compactWithValidity tx (witnessSetHashOf tx) 0

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  inputSetUniquenessStep01Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  inputSetUniquenessStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

context01 :: Tx -> Integer -> Bool -> ScriptContext
context01 tx validity forgedHeader =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [PD.Constr 0 [PD.Constr 0 [inclusionArgs txId source rawRoot]]])
    [threadInput]
    [stepOutput nextScript $ Just $ PD.Constr 0 [PD.B txId]]
    (referenceInputsWithTransactionsRoot headerRoot)
    [phasEntry rawRoot txId source]
    mempty
  where
    txId = txIdOf tx
    source = sourceCborWithValidity tx validity
    rawRoot = singleEntryPhasRoot txId source
    genuineHeader = commitCountedRoot transactionsDomain rawRoot l2Count
    headerRoot = if forgedHeader then headerTransactionsRoot else genuineHeader

cancelContext :: ScriptContext
cancelContext =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    []
    [cancelMintEntry threadName]
    mempty

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo

duplicateSpendContext :: Tx -> Integer -> Integer -> Maybe BS.ByteString -> ScriptContext
duplicateSpendContext tx first second replacement =
  finalizeContext tx $
    PD.Constr
      0
      [ PD.I 0
      , PD.I 0
      , PD.I 0
      , PD.I first
      , PD.I second
      , bodyOpening (acceptedCbor tx) $ maybe (spendInputsPreimage tx) id replacement
      ]

duplicateReferenceContext :: Tx -> Integer -> Integer -> ScriptContext
duplicateReferenceContext tx first second =
  finalizeContext tx $
    PD.Constr
      1
      [ PD.I 0
      , PD.I 0
      , PD.I 0
      , PD.I first
      , PD.I second
      , bodyOpening (acceptedCbor tx) $ referenceInputsPreimage tx
      ]

overlapContext :: Tx -> Integer -> Integer -> ScriptContext
overlapContext tx spendIndex referenceIndex =
  finalizeContext tx $
    PD.Constr
      2
      [ PD.I 0
      , PD.I 0
      , PD.I 0
      , PD.I spendIndex
      , PD.I referenceIndex
      , PD.B $ acceptedCbor tx
      , PD.Constr 0 [PD.B $ spendInputsPreimage tx]
      , PD.Constr 0 [PD.B $ referenceInputsPreimage tx]
      ]

finalizeContext :: Tx -> PD.Data -> ScriptContext
finalizeContext tx = finalizeContextWithAnchor (txIdOf tx)

finalizeContextWithAnchor :: BS.ByteString -> PD.Data -> ScriptContext
finalizeContextWithAnchor anchor args =
  spendContext
    (stepDatum $ Just $ PD.Constr 0 [PD.B anchor])
    (PD.Constr 1 [args])
    [threadInput]
    [convictionOutput fraudProofAddress threadName]
    referenceInputs
    [fraudProofMintEntry threadName]
    (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

foreignAnchorContext :: ScriptContext
foreignAnchorContext =
  finalizeContextWithAnchor (txIdOf distinctTx) $
    PD.Constr
      0
      [ PD.I 0
      , PD.I 0
      , PD.I 0
      , PD.I 0
      , PD.I 2
      , bodyOpening (acceptedCbor duplicateSpendTx) $ spendInputsPreimage duplicateSpendTx
      ]

publishedContext :: Tx -> ScriptContext
publishedContext tx =
  spendContext
    (stepDatum Nothing)
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.Constr
                0
                [ PD.Constr
                    1
                    [ PD.Constr
                        0
                        [PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.B txId, PD.B source, PD.B rawRoot, PD.List []]
                    ]
                ]
            ]
        ]
    )
    [threadInput]
    [stepOutput nextScript $ Just $ PD.Constr 0 [PD.B txId]]
    (referenceInputsWithTransactionsRoot $ commitCountedRoot transactionsDomain rawRoot l2Count)
    [publishedClaim rawRoot txId source]
    mempty
  where
    txId = txIdOf tx
    source = sourceCborWithValidity tx 0
    rawRoot = singleEntryPhasRoot txId source

publishedClaim :: BS.ByteString -> BS.ByteString -> BS.ByteString -> (ScriptPurpose, Redeemer)
publishedClaim rawRoot txId cbor =
  ( Rewarding $ ScriptCredential $ ScriptHash $ toBuiltin chunkedVerifyHash
  , Redeemer $
      dataToBuiltinData $
        PD.Constr
          0
          [PD.Constr 0 [], PD.B rawRoot, PD.B txId, PD.B $ blake2b256 cbor, PD.List []]
  )

chunkedVerifyHash :: BS.ByteString
chunkedVerifyHash = Base16.decodeLenient "ea8d998a1396392158fa85afb0d202df7bd6d6ede7d3fbc05f55acd6"

-- Construct checkpoint preimages independently, using the target's separate
-- integer encodings and definite six-field subject array, not state Data.
scanTx :: Tx
scanTx = tx1 {tSpendInputs = [(BS.replicate 32 0x11, 0), (BS.replicate 32 0x22, 0)], tReferenceInputs = [(BS.replicate 32 0x33, 0)]}

scanKey, scanReason, scanSubject, scanBound :: PD.Data
scanKey = inputData sharedInputRef
scanReason = PD.Constr 3 [PD.I 0, PD.I 0, PD.I 1, PD.I 0]
scanSubject = PD.Constr 0 [PD.I 1, PD.I 1, PD.I 1, PD.B $ txIdOf scanTx, PD.B $ serialise scanKey, PD.Constr 0 [scanReason]]
scanBound = PD.Constr 0 [scanSubject, PD.I 0, PD.I 0, PD.I 1, PD.I 0]

scanItem :: Int -> BS.ByteString
scanItem index = encodedInput $ (tSpendInputs scanTx ++ tReferenceInputs scanTx) !! index

scanCheckpoint :: PD.Data -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
scanCheckpoint (PD.Constr 0 [PD.Constr 0 [version, direction, sourceKind, txId, key, PD.Constr 0 [reason]], firstField, firstItem, secondField, secondItem]) spendCount referenceCount cursor previous nextHash =
  blake2b256 $
    "midgard/fraud-proofs/input-set-uniqueness/checkpoint-v1"
      <> BS.singleton 0x86
      <> BS.concat (map serialise [version, direction, sourceKind, txId, key])
      <> BS.singleton 0x81
      <> serialise reason
      <> BS.concat (map serialise [firstField, firstItem, secondField, secondItem, PD.I spendCount, PD.I referenceCount, PD.I cursor, PD.B previous, PD.B nextHash])
scanCheckpoint _ _ _ _ _ _ = error "canonical forced subject fixture required"

scanStateWith :: PD.Data -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> PD.Data
scanStateWith bound spendCount referenceCount cursor previous nextHash =
  PD.Constr
    0
    [ bound
    , PD.I spendCount
    , PD.I referenceCount
    , PD.I cursor
    , PD.B previous
    , PD.B nextHash
    , PD.B $ scanCheckpoint bound spendCount referenceCount cursor previous nextHash
    ]

scanState :: Integer -> PD.Data
scanState cursor = scanStateWith scanBound 2 1 cursor (if cursor == 0 then "" else scanItem $ fromInteger cursor - 1) stepScript

fromData :: (PIsData a) => PD.Data -> Term s a
fromData = pfromData . punsafeCoerce . pconstant @PData

field :: Int -> PD.Data -> PD.Data -> PD.Data
field index replacement (PD.Constr tag fields) = PD.Constr tag [if n == index then replacement else value | (n, value) <- zip [0 ..] fields]
field _ _ _ = error "constructor fixture required"

forced01 :: PD.Data -> Maybe PD.Data -> Term s PUnit
forced01 reason priorState =
  step01 $
    spendContext
      (stepDatum priorState)
      (PD.Constr 1 [PD.Constr 0 [PD.Constr 1 [PD.I 0, PD.I 0, header, membership]]])
      [threadInputWithName name]
      [stepOutputWithName nextScript (Just $ PD.Constr 0 [scanBound]) name]
      []
      []
      mempty
  where
    source =
      PD.Constr
        0
        [ PD.B $ compactWithValidity scanTx (witnessSetHashOf scanTx) 1
        , PD.B $ witnessSetCborOf scanTx
        , PD.B $ fieldPreimageLengthsCborOf scanTx
        ]
    leaf = PD.Constr 0 [PD.B $ txIdOf scanTx, source, PD.Constr 1 [reason]]
    rawRoot = singleEntryPhasRoot (serialise scanKey) (serialise leaf)
    root = commitCountedRoot 1 rawRoot 1
    membership = membershipProof 1 root rawRoot 1 scanKey leaf
    header =
      PD.Constr 0 $
        [PD.B "", PD.B "", PD.B "", PD.B root, PD.B "", PD.B "", PD.B "", PD.B "", PD.B ""]
          ++ [PD.I 0, PD.I 1, PD.I 0, PD.I 0, PD.I 0, PD.I 0]
          ++ replicate 7 (PD.I 0)
          ++ [PD.B "", PD.B "", PD.I 1]
    name = BS.pack [0, 0, 0, 5] <> blake2b224 (serialise header)

forced03 :: PD.Data -> Tx -> PD.Data -> Term s PUnit
forced03 bound openedTx expected =
  inputSetUniquenessStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin stepScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant context
  where
    context =
      spendContext
        (stepDatum $ Just $ PD.Constr 0 [bound])
        ( PD.Constr
            1
            [ PD.Constr
                0
                [ PD.I 0
                , PD.I 0
                , PD.B $ compactWithValidity openedTx (witnessSetHashOf openedTx) 1
                , PD.Constr 0 [PD.B $ spendInputsPreimage openedTx]
                , PD.Constr 0 [PD.B $ referenceInputsPreimage openedTx]
                ]
            ]
        )
        [threadInput]
        [stepOutput stepScript $ Just expected]
        []
        []
        mempty

forced04 :: PD.Data -> Tx -> Bool -> BS.ByteString -> PD.Data -> Term s PUnit
forced04 state openedTx readingSpend outputHash expected =
  inputSetUniquenessStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant context
  where
    opening =
      bodyOpening
        (compactWithValidity openedTx (witnessSetHashOf openedTx) 1)
        (if readingSpend then spendInputsPreimage openedTx else referenceInputsPreimage openedTx)
    context =
      spendContext
        (stepDatum $ Just state)
        (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, opening]])
        [threadInput]
        [stepOutput outputHash $ Just expected]
        []
        []
        mempty

finish04 :: PD.Data -> Term s PUnit
finish04 state =
  inputSetUniquenessStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant context
  where
    context =
      spendContext
        (stepDatum $ Just state)
        (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, PD.I 0]])
        [threadInput]
        [convictionOutput fraudProofAddress threadName]
        []
        [fraudProofMintEntry threadName]
        (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

batch :: BS.ByteString -> Integer -> Integer -> Integer -> Term s (PBuiltinList PByteString)
batch bytes count start requested =
  Field.pfixedStrideItemBatch
    # pcon (Field.PWholeView (pconstant bytes) (pconstant count) 40)
    # pconstant start
    # pconstant requested

forcedTests :: [TestTree]
forcedTests =
  [ testCase "forced source binds exact DuplicateInput coordinates" $ psucceeds $ forced01 scanReason Nothing
  , testCase "forced source refuses another reason" $ pfails $ forced01 (PD.Constr 2 []) Nothing
  , testCase "forced source refuses a prior state" $ pfails $ forced01 scanReason (Just $ PD.I 0)
  , testCase "step 03 opens both complete sets and seeds target checkpoint" $ psucceeds $ forced03 scanBound scanTx (scanState 0)
  , testCase "step 03 refuses another transaction" $ pfails $ forced03 scanBound distinctTx (scanState 0)
  , testCase "step 03 refuses out-of-range reference coordinate" $ pfails $ forced03 (field 4 (PD.I 1) scanBound) scanTx (scanState 0)
  , testCase "step 03 refuses reversed coordinates" $ pfails $ forced03 (field 1 (PD.I 1) $ field 3 (PD.I 0) scanBound) scanTx (scanState 0)
  , testCase "step 04 consumes the complete spend batch" $ psucceeds $ forced04 (scanState 0) scanTx True stepScript (scanState 2)
  , testCase "step 04 continues across the field boundary" $ psucceeds $ forced04 (scanState 2) scanTx False stepScript (scanState 3)
  , testCase "step 04 refuses a skipped item" $ pfails $ forced04 (scanState 0) scanTx True stepScript (scanState 1)
  , testCase "step 04 refuses a changed checkpoint" $ pfails $ forced04 (field 6 (PD.B $ hash32 0xff) $ scanState 0) scanTx True stepScript (scanState 2)
  , testCase "step 04 refuses an authenticated foreign next script" $ pfails $ forced04 (scanStateWith scanBound 2 1 0 "" nextScript) scanTx True stepScript (scanState 2)
  , testCase "step 04 refuses successor substitution" $ pfails $ forced04 (scanState 0) scanTx True nextScript (scanState 2)
  , testCase "step 04 refuses the wrong field opening" $ pfails $ forced04 (scanState 2) scanTx True stepScript (scanState 3)
  , testCase "step 04 finalizes only the complete strict union" $ psucceeds $ finish04 (scanState 3)
  , testCase "step 04 refuses incomplete scan finalization" $ pfails $ finish04 (scanState 2)
  , testCase "step 04 refuses a forged terminal checkpoint" $ pfails $ finish04 (field 6 (PD.B $ hash32 0xff) $ scanState 3)
  , testCase "batch checkpoint matches independent byte encoding" $
      passertEval $
        pforgetData
          ( pdata $
              U.padvanceUniqueBatch
                # fromData (scanState 0)
                # pconstant [scanItem 0, scanItem 1, scanItem 2]
                # pconstant stepScript
          )
          #== pconstant (scanState 3)
  , testCase "equal adjacent item refuses complete negation" $
      pfails $
        U.padvanceUniqueBatch # fromData (scanState 0) # pconstant [scanItem 0, scanItem 0, scanItem 2] # pconstant stepScript
  , testCase "descending item refuses complete negation" $
      pfails $
        U.padvanceUniqueBatch # fromData (scanState 0) # pconstant [scanItem 1, scanItem 0, scanItem 2] # pconstant stepScript
  , testCase "batch cannot pass the authenticated total" $
      pfails $
        U.padvanceUniqueBatch # fromData (scanState 0) # pconstant [scanItem 0, scanItem 1, scanItem 2, scanItem 2] # pconstant stepScript
  , testCase "batch cannot be empty" $
      pfails $
        U.padvanceUniqueBatch # fromData (scanState 0) # pnil # pconstant stepScript
  , testCase "one-item continuation has the same checkpoint" $
      passertEval $
        pforgetData (pdata $ U.padvanceUniqueScan # fromData (scanState 0) # pconstant (scanItem 0) # pconstant stepScript) #== pconstant (scanState 1)
  , testCase "fixed-stride batch strips every wrapper" $
      passertEval $
        batch (spendInputsPreimage scanTx) 2 0 2 #== pconstant [scanItem 0, scanItem 1]
  , testCase "fixed-stride batch refuses malformed later wrapper" $
      pfails $
        batch (BS.take 41 (spendInputsPreimage scanTx) <> BS.pack [0x58, 0x25] <> scanItem 1) 2 0 2
  , testCase "fixed-stride batch refuses truncated range" $
      pfails $
        batch (BS.init $ spendInputsPreimage scanTx) 2 0 2
  , testCase "fixed-stride batch refuses range overflow" $
      pfails $
        batch (spendInputsPreimage scanTx) 2 1 2
  ]
