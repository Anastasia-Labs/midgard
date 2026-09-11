{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsL2TxMistag (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Credential (..), Redeemer (..), ScriptContext, ScriptHash (..), ScriptPurpose (..), TokenName (..))
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.MpfProof (phasV1)
import Midgard.MpfProof.Types (PProof (..))
import Midgard.Validators.FraudProofs.L2TxMistag (l2TxMistagStep01Validator, l2TxMistagStep02Validator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "L2 transaction mistag fraud proof"
  [ testCase "step 01 binds a committed nonzero validity leaf" $ psucceeds $ runStep01 badCbor badRoot badCountedRoot (state 1)
  , testCase "step 01 rejects an honest code-zero leaf" $ pfails $ runStep01 honestCbor honestRoot honestCountedRoot (state 0)
  , testCase "step 01 rejects a flipped scalar forgery" $ pfails runFlippedScalar
  , testCase "step 01 rejects a forged transactions root" $ pfails $ runStep01WithHeader badCbor badRoot honestCountedRoot (state 1)
  , testCase "step 01 rejects a lie about the forwarded code" $ pfails $ runStep01 badCbor badRoot badCountedRoot (state 0)
  , testCase "step 01 binds a published-chunk membership proof" $ psucceeds $ runPublished badRoot badCountedRoot
  , testCase "step 01 rejects published chunks under a forged root" $ pfails $ runPublished honestRoot badCountedRoot
  , testCase "step 02 finalizes a nonzero validity code" $ psucceeds $ runStep02 1
  , testCase "step 02 rejects a code-zero state" $ pfails $ runStep02 0
  ]

badCbor, honestCbor, badRoot, honestRoot, badCountedRoot, honestCountedRoot :: BS.ByteString
badCbor = sourceCborWithValidity tx1 1
honestCbor = sourceCborWithValidity tx1 0
badRoot = singleEntryPhasRoot tx1Id badCbor
honestRoot = singleEntryPhasRoot tx1Id honestCbor
badCountedRoot = commitCountedRoot transactionsDomain badRoot l2Count
honestCountedRoot = commitCountedRoot transactionsDomain honestRoot l2Count

state :: Integer -> PD.Data
state validityCode = PD.Constr 0 [PD.B tx1Id, PD.I validityCode]

runStep01 :: forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data -> Term s PUnit
runStep01 cbor rawRoot countedRoot = runStep01WithHeader cbor rawRoot countedRoot

runStep01WithHeader :: forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data -> Term s PUnit
runStep01WithHeader cbor rawRoot countedRoot outputState = l2TxMistagStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant (step01Context cbor rawRoot countedRoot outputState)

runFlippedScalar :: forall s. Term s PUnit
runFlippedScalar =
  pif
    ( phasV1
        # pconstant honestRoot
        # pconstant tx1Id
        # pconstant badCbor
        # pcon (PProof $ pcon PNil)
    )
    (runStep01WithHeader badCbor honestRoot honestCountedRoot $ state 1)
    perror

step01Context :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data -> ScriptContext
step01Context cbor rawRoot countedRoot outputState = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [inclusionArgs tx1Id cbor rawRoot])
  [threadInput]
  [stepOutput nextScript $ Just outputState]
  (referenceInputsWithTransactionsRoot countedRoot)
  [phasEntry rawRoot tx1Id cbor]
  mempty

runPublished :: forall s. BS.ByteString -> BS.ByteString -> Term s PUnit
runPublished rawRoot countedRoot = l2TxMistagStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant (publishedContext rawRoot countedRoot)

publishedContext :: BS.ByteString -> BS.ByteString -> ScriptContext
publishedContext rawRoot countedRoot = spendContext
  (stepDatum Nothing)
  ( PD.Constr 1
      [ PD.Constr 1
          [ PD.Constr 0
              [ PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.B tx1Id, PD.B badCbor
              , PD.B rawRoot, PD.List []
              ]
          ]
      ]
  )
  [threadInput]
  [stepOutput nextScript $ Just $ state 1]
  (referenceInputsWithTransactionsRoot countedRoot)
  [publishedClaim rawRoot]
  mempty

publishedClaim :: BS.ByteString -> (ScriptPurpose, Redeemer)
publishedClaim rawRoot =
  ( Rewarding $ ScriptCredential $ ScriptHash $ toBuiltin chunkedVerifyHash
  , Redeemer $ dataToBuiltinData $ PD.Constr 0
      [ PD.Constr 0 [], PD.B rawRoot, PD.B tx1Id, PD.B $ blake2b256 badCbor, PD.List []]
  )

chunkedVerifyHash :: BS.ByteString
chunkedVerifyHash = BS.pack
  [ 0xcb, 0x5a, 0x7e, 0xc4, 0xde, 0xf3, 0x5c, 0xe3, 0xec, 0x75, 0xc4, 0x09, 0x19, 0x99
  , 0x2e, 0x1b, 0x4e, 0x88, 0x39, 0xb4, 0xf6, 0xb6, 0xa2, 0xd3, 0xb0, 0x6e, 0x74, 0x69
  ]

runStep02 :: forall s. Integer -> Term s PUnit
runStep02 validityCode = l2TxMistagStep02Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pconstant (finalizeContext validityCode)

finalizeContext :: Integer -> ScriptContext
finalizeContext validityCode = spendContext
  (stepDatum $ Just $ state validityCode)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  []
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)
