{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsInputSetUniqueness (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Credential (..), Redeemer (..), ScriptContext (..), ScriptHash (..), ScriptPurpose (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.InputSetUniqueness
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Input-set uniqueness fraud proof"
  [ testCase "step 01 binds an accepted duplicate-input transaction" $ psucceeds $ step01 $ context01 duplicateSpendTx 0 False
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
  , testCase "step 02 rejects a fabricated duplicate preimage" $ pfails $ step02 $
      duplicateSpendContext distinctTx 0 1 (Just $ spendInputsPreimage adjacentSpendTx)
  , testCase "step 02 rejects a foreign anchor" $ pfails $ step02 foreignAnchorContext
  ]

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
step01 ctx = inputSetUniquenessStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant ctx
step02 ctx = inputSetUniquenessStep02Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant certificatePolicy)
  # pconstant ctx

context01 :: Tx -> Integer -> Bool -> ScriptContext
context01 tx validity forgedHeader = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [inclusionArgs txId source rawRoot])
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
cancelContext = spendContext
  (stepDatum Nothing) cancelRedeemer [threadInput] [] [] [cancelMintEntry threadName] mempty

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo

duplicateSpendContext :: Tx -> Integer -> Integer -> Maybe BS.ByteString -> ScriptContext
duplicateSpendContext tx first second replacement = finalizeContext tx $ PD.Constr 0
  [ PD.I 0, PD.I 0, PD.I 0, PD.I first, PD.I second
  , bodyOpening (acceptedCbor tx) $ maybe (spendInputsPreimage tx) id replacement
  ]

duplicateReferenceContext :: Tx -> Integer -> Integer -> ScriptContext
duplicateReferenceContext tx first second = finalizeContext tx $ PD.Constr 1
  [ PD.I 0, PD.I 0, PD.I 0, PD.I first, PD.I second
  , bodyOpening (acceptedCbor tx) $ referenceInputsPreimage tx
  ]

overlapContext :: Tx -> Integer -> Integer -> ScriptContext
overlapContext tx spendIndex referenceIndex = finalizeContext tx $ PD.Constr 2
  [ PD.I 0, PD.I 0, PD.I 0, PD.I spendIndex, PD.I referenceIndex, PD.B $ acceptedCbor tx
  , PD.Constr 0 [PD.B $ spendInputsPreimage tx]
  , PD.Constr 0 [PD.B $ referenceInputsPreimage tx]
  ]

finalizeContext :: Tx -> PD.Data -> ScriptContext
finalizeContext tx = finalizeContextWithAnchor (txIdOf tx)

finalizeContextWithAnchor :: BS.ByteString -> PD.Data -> ScriptContext
finalizeContextWithAnchor anchor args = spendContext
  (stepDatum $ Just $ PD.Constr 0 [PD.B anchor])
  (PD.Constr 1 [args])
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  referenceInputs
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

foreignAnchorContext :: ScriptContext
foreignAnchorContext = finalizeContextWithAnchor (txIdOf distinctTx) $ PD.Constr 0
  [ PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.I 2
  , bodyOpening (acceptedCbor duplicateSpendTx) $ spendInputsPreimage duplicateSpendTx
  ]

publishedContext :: Tx -> ScriptContext
publishedContext tx = spendContext
  (stepDatum Nothing)
  ( PD.Constr 1
      [ PD.Constr 1
          [ PD.Constr 0
              [ PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.B txId, PD.B source
              , PD.B rawRoot, PD.List []
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
  , Redeemer $ dataToBuiltinData $ PD.Constr 0
      [PD.Constr 0 [], PD.B rawRoot, PD.B txId, PD.B $ blake2b256 cbor, PD.List []]
  )

chunkedVerifyHash :: BS.ByteString
chunkedVerifyHash = BS.pack
  [ 0xcb, 0x5a, 0x7e, 0xc4, 0xde, 0xf3, 0x5c, 0xe3, 0xec, 0x75, 0xc4, 0x09, 0x19, 0x99
  , 0x2e, 0x1b, 0x4e, 0x88, 0x39, 0xb4, 0xf6, 0xb6, 0xa2, 0xd3, 0xb0, 0x6e, 0x74, 0x69
  ]
