{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsWithdrawnInput (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.WithdrawnInput
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Withdrawn input fraud proof"
  [ testCase "step 01 binds a valid native transaction" $ psucceeds $ step01 $ context01 honestSource honestRaw expected02
  , testCase "step 01 rejects a code-one transaction" $ pfails $ step01 $ context01 codeOneSource codeOneRaw expected02
  , testCase "step 01 rejects altered carried state" $ pfails $ step01 $ context01 honestSource honestRaw (state02 tx1Id otherRoot withdrawalCount)
  , testCase "step 02 forwards the challenged spend input" $ psucceeds $ step02 $ context02 (spendInputsPreimage tx1) 0 expected03
  , testCase "step 02 rejects a substituted preimage" $ pfails $ step02 $ context02 (spendInputsPreimage txEmpty) 0 expected03
  , testCase "step 02 rejects an out-of-range index" $ pfails $ step02 $ context02 (spendInputsPreimage tx1) 7 expected03
  , testCase "step 03 accepts a committed valid withdrawal" $ psucceeds $ step03 $ context03 sharedInputRef valid
  , testCase "step 03 rejects a withdrawal of another UTxO" $ pfails $ step03 $ context03 otherInputRef valid
  , testCase "step 03 rejects invalid withdrawal evidence" $ pfails $ step03 $ context03 sharedInputRef invalid
  ]

honestCbor, honestSource, codeOneSource, honestRaw, codeOneRaw :: BS.ByteString
honestCbor = compactWithValidity tx1 (witnessSetHashOf tx1) 0
honestSource = sourceCborWithValidity tx1 0
codeOneSource = sourceCborWithValidity tx1 1
honestRaw = singleEntryPhasRoot tx1Id honestSource
codeOneRaw = singleEntryPhasRoot tx1Id codeOneSource

expected02, expected03 :: PD.Data
expected02 = state02 tx1Id headerWithdrawalsRoot withdrawalCount
expected03 = state03 (inputData sharedInputRef) headerWithdrawalsRoot withdrawalCount

state02 :: BS.ByteString -> BS.ByteString -> Integer -> PD.Data
state02 txId root count = PD.Constr 0 [PD.B txId, PD.B root, PD.I count]

state03 :: PD.Data -> BS.ByteString -> Integer -> PD.Data
state03 input root count = PD.Constr 0 [input, PD.B root, PD.I count]

step01, step02, step03 :: forall s. ScriptContext -> Term s PUnit
step01 ctx = withdrawnInputStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant ctx
step02 ctx = withdrawnInputStep02Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant certificatePolicy)
  # pconstant ctx
step03 ctx = withdrawnInputStep03Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

context01 :: BS.ByteString -> BS.ByteString -> PD.Data -> ScriptContext
context01 cbor rawRoot outputState = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [inclusionArgs tx1Id cbor rawRoot])
  [threadInput]
  [stepOutput nextScript $ Just outputState]
  (referenceInputsWithTransactionsRoot $ commitCountedRoot transactionsDomain rawRoot l2Count)
  [phasEntry rawRoot tx1Id cbor]
  mempty

context02 :: BS.ByteString -> Integer -> PD.Data -> ScriptContext
context02 preimage badInputIndex outputState = spendContext
  (stepDatum $ Just expected02)
  ( PD.Constr 1
      [ PD.Constr 0
          [PD.I 0, PD.I 0, bodyOpening honestCbor preimage, PD.I badInputIndex]
      ]
  )
  [threadInput]
  [stepOutput nextScript $ Just outputState]
  referenceInputs
  []
  mempty

valid, invalid :: PD.Data
valid = PD.Constr 0 []
invalid = PD.Constr 1 []

context03 :: (BS.ByteString, Integer) -> PD.Data -> ScriptContext
context03 committedOutref validity = spendContext
  (stepDatum $ Just $ state03 (inputData sharedInputRef) countedRoot 1)
  ( PD.Constr 1
      [ PD.Constr 0
          [ PD.I 0, PD.I 0, PD.I 0
          , membershipProof withdrawalsDomain countedRoot rawRoot 1 withdrawalId claimedEvent
          ]
      ]
  )
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  []
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)
  where
    committedEvent = withdrawalInfoData committedOutref validity
    claimedEvent = withdrawalInfoData sharedInputRef validity
    rawRoot = singleEntryPhasRoot withdrawalKeyBytes $ serialise committedEvent
    countedRoot = commitCountedRoot withdrawalsDomain rawRoot 1
