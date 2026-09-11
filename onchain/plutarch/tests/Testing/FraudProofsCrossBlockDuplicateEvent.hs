{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsCrossBlockDuplicateEvent (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName (..), singleton, unCurrencySymbol)
import PlutusLedgerApi.V3 (Datum (..), OutputDatum (..), ScriptContext (..), ScriptHash (..), TxInInfo (..), TxInfo (..), TxOut (..))
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.CrossBlockDuplicateEvent
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Cross-block duplicate-event fraud proof"
  [ testCase "step 01 accepts a committed deposit" $ psucceeds $ step01 $ step01Context depositEvent (eProof depositEvent) depositKind duplicatedEventId duplicateThreadName
  , testCase "step 01 accepts a committed withdrawal" $ psucceeds $ step01 $ step01Context withdrawalEvent (eProof withdrawalEvent) withdrawalKind duplicatedEventId duplicateThreadName
  , testCase "step 01 accepts a committed forced transaction" $ psucceeds $ step01 $ step01Context forcedEvent (eProof forcedEvent) forcedKind duplicatedEventId duplicateThreadName
  , testCase "step 01 rejects a forged forced-order root" $ pfails $ step01 $ step01Context forcedEvent (eProof foreignForcedEvent) forcedKind foreignEventId duplicateThreadName
  , testCase "step 01 rejects a forged deposit root" $ pfails $ step01 $ step01Context depositEvent (eProof foreignDepositEvent) depositKind foreignEventId duplicateThreadName
  , testCase "step 01 rejects a wrong count" $ pfails $ step01 $ step01Context depositEvent (withCount 2 $ eProof depositEvent) depositKind duplicatedEventId duplicateThreadName
  , testCase "step 01 rejects another fraud category" $ pfails $ step01 $ step01Context withdrawalEvent (eProof withdrawalEvent) withdrawalKind duplicatedEventId threadName
  , testCase "step 01 cancels under the prover signature" $ psucceeds $ step01 $ cancellationContext Nothing True
  , testCase "step 01 rejects unsigned cancellation" $ pfails $ step01 $ cancellationContext Nothing False
  , testCase "step 02 convicts a confirmed deposit duplicate" $ psucceeds $ step02 $ step02Context depositEvent depositEvent historicalHeaderHash (eProof depositEvent)
  , testCase "step 02 convicts a confirmed withdrawal duplicate" $ psucceeds $ step02 $ step02Context withdrawalEvent withdrawalEvent historicalHeaderHash (eProof withdrawalEvent)
  , testCase "step 02 convicts a confirmed forced-order duplicate" $ psucceeds $ step02 $ step02Context forcedEvent forcedEvent historicalHeaderHash (eProof forcedEvent)
  , testCase "step 02 rejects a different forced-order identity" $ pfails $ step02 $ step02Context forcedEvent foreignForcedEvent historicalHeaderHash (eProof foreignForcedEvent)
  , testCase "step 02 rejects a different deposit identity" $ pfails $ step02 $ step02Context depositEvent foreignDepositEvent historicalHeaderHash (eProof foreignDepositEvent)
  , testCase "step 02 rejects a cross-domain event" $ pfails $ step02 $ step02Context depositEvent withdrawalEvent historicalHeaderHash (eProof withdrawalEvent)
  , testCase "step 02 rejects the challenged header itself" $ pfails $ step02 $ step02Context depositEvent depositEvent challengedHeaderHash (eProof depositEvent)
  , testCase "step 02 rejects a removed-ancestor copy under another policy" $ pfails $ step02 $ step02ContextWithInput withdrawalEvent (eProof withdrawalEvent) (settlementInputWithPolicy withdrawalEvent historicalHeaderHash foreignSettlementPolicy) duplicateThreadName
  , testCase "step 02 rejects a forged historical root" $ pfails $ step02 $ step02ContextWithInput depositEvent (eProof foreignDepositEvent) (settlementInput depositEvent historicalHeaderHash) duplicateThreadName
  , testCase "step 02 rejects a carried header mismatch" $ pfails $ step02 $ step02ContextWithState (stateData historicalHeaderHash depositKind duplicatedEventId) (eProof depositEvent) (settlementInput depositEvent historicalHeaderHash) duplicateThreadName
  , testCase "step 02 rejects another fraud category" $ pfails $ step02 $ step02ContextWithInput depositEvent (eProof depositEvent) (settlementInput depositEvent historicalHeaderHash) threadName
  , testCase "step 02 cancels under the prover signature" $ psucceeds $ step02 $ cancellationContext (Just $ stateFor depositEvent) True
  , testCase "step 02 rejects unsigned cancellation" $ pfails $ step02 $ cancellationContext (Just $ stateFor depositEvent) False
  ]

data Event = Event
  { eArm :: Integer
  , eKey :: PD.Data
  , eCountedRoot :: BS.ByteString
  , eProof :: PD.Data
  }

mkEvent :: Integer -> Integer -> PD.Data -> PD.Data -> Event
mkEvent arm domain key value = event
  where
    rawRoot = singleEntryPhasRoot (serialise key) (serialise value)
    countedRoot = commitCountedRoot domain rawRoot 1
    proof = PD.Constr arm [membershipProof domain countedRoot rawRoot 1 key value]
    event = Event arm key countedRoot proof

duplicatedEventId, foreignEventId :: PD.Data
duplicatedEventId = inputData (BS.replicate 32 0x81, 2)
foreignEventId = inputData (BS.replicate 32 0x92, 0)

depositInfo, withdrawalInfo, forcedTransaction :: PD.Data
depositInfo = PD.Constr 0
  [ PD.Constr 0 [PD.Constr 0 [PD.B $ BS.replicate 28 0x1a], PD.Constr 1 []]
  , PD.I 0
  , PD.Constr 1 []
  ]
withdrawalInfo = withdrawalInfoData (BS.replicate 32 0x2b, 1) (PD.Constr 0 [])
forcedTransaction = PD.Constr 0
  [ PD.B $ BS.replicate 32 0x11
  , PD.Constr 0 [PD.B "\x80", PD.B "\x80", PD.B "\x80"]
  , PD.Constr 0 []
  ]

depositEvent, foreignDepositEvent, withdrawalEvent, forcedEvent, foreignForcedEvent :: Event
depositEvent = mkEvent 0 depositsDomain duplicatedEventId depositInfo
foreignDepositEvent = mkEvent 0 depositsDomain foreignEventId depositInfo
withdrawalEvent = mkEvent 1 withdrawalsDomain duplicatedEventId withdrawalInfo
forcedEvent = mkEvent 2 forcedTransactionsDomain duplicatedEventId forcedTransaction
foreignForcedEvent = mkEvent 2 forcedTransactionsDomain foreignEventId forcedTransaction

depositKind, withdrawalKind, forcedKind :: PD.Data
depositKind = PD.Constr 0 []
withdrawalKind = PD.Constr 1 []
forcedKind = PD.Constr 2 []

challengedHeaderHash, historicalHeaderHash, duplicateThreadName :: BS.ByteString
challengedHeaderHash = BS.replicate 28 0xaa
historicalHeaderHash = BS.replicate 28 0xbb
duplicateThreadName = BS.pack [0, 0, 0, 0x16] <> challengedHeaderHash

settlementPolicy, foreignSettlementPolicy :: CurrencySymbol
settlementPolicy = policyFor 0x4a
foreignSettlementPolicy = policyFor 0x74

stateFor :: Event -> PD.Data
stateFor event = stateData challengedHeaderHash (kindFor event) (eKey event)

stateData :: BS.ByteString -> PD.Data -> PD.Data -> PD.Data
stateData header kind key = PD.Constr 0 [PD.B header, PD.B $ unCS settlementPolicy, kind, key]

kindFor :: Event -> PD.Data
kindFor event = PD.Constr (eArm event) []

withCount :: Integer -> PD.Data -> PD.Data
withCount count (PD.Constr arm [PD.Constr 0 [domain, root, rawRoot, _, key, value, proof]]) =
  PD.Constr arm [PD.Constr 0 [domain, root, rawRoot, PD.I count, key, value, proof]]
withCount _ _ = error "invalid duplicate-event proof fixture"

eventReferenceInputs :: Event -> [TxInInfo]
eventReferenceInputs event = case eArm event of
  0 -> referenceInputsWithDepositsRoot (eCountedRoot event) 1
  1 -> referenceInputsWithWithdrawalsRoot (eCountedRoot event) 1
  2 -> referenceInputsWithForcedTransactionsRoot (eCountedRoot event) 1
  _ -> error "invalid duplicate-event arm"

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx = crossBlockDuplicateEventStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant ctx
step02 ctx = crossBlockDuplicateEventStep02Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

step01Context :: Event -> PD.Data -> PD.Data -> PD.Data -> BS.ByteString -> ScriptContext
step01Context challenged committedEvent kind key assetName = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 1, committedEvent]])
  [threadInputWithName assetName]
  [stepOutputWithName nextScript (Just $ stateData challengedHeaderHash kind key) assetName]
  (eventReferenceInputs challenged)
  []
  mempty

step02Context :: Event -> Event -> BS.ByteString -> PD.Data -> ScriptContext
step02Context challenged settled settledHash settledProof =
  step02ContextWithInput challenged settledProof (settlementInput settled settledHash) duplicateThreadName

step02ContextWithInput :: Event -> PD.Data -> TxInInfo -> BS.ByteString -> ScriptContext
step02ContextWithInput challenged = step02ContextWithState (stateFor challenged)

step02ContextWithState :: PD.Data -> PD.Data -> TxInInfo -> BS.ByteString -> ScriptContext
step02ContextWithState state settledProof settlementRef assetName = spendContext
  (stepDatum $ Just state)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0, settledProof]])
  [threadInputWithName assetName]
  [convictionOutput fraudProofAddress assetName]
  [settlementRef]
  [fraudProofMintEntry assetName]
  (singleton fpPolicy (TokenName $ toBuiltin assetName) 1)

settlementInput :: Event -> BS.ByteString -> TxInInfo
settlementInput event settledHash = settlementInputWithPolicy event settledHash settlementPolicy

settlementInputWithPolicy :: Event -> BS.ByteString -> CurrencySymbol -> TxInInfo
settlementInputWithPolicy event settledHash policy = TxInInfo
  (outRefN 7)
  ( TxOut
      (scriptHashAddress $ ScriptHash $ unCurrencySymbol policy)
      (adaValue 3_000_000 <> singleton policy (TokenName $ toBuiltin settledHash) 1)
      (OutputDatum $ Datum $ dataToBuiltinData $ settlementDatum event)
      Nothing
  )

settlementDatum :: Event -> PD.Data
settlementDatum event = PD.Constr 0
  [ PD.B $ rootFor 0 (hash32 0xd1)
  , PD.B $ rootFor 1 (hash32 0xd2)
  , PD.B $ rootFor 2 (hash32 0xd3)
  , PD.B $ hash32 0xd4
  , PD.Constr 1 []
  ]
  where
    rootFor arm fallback
      | eArm event == arm = eCountedRoot event
      | otherwise = fallback

cancellationContext :: Maybe PD.Data -> Bool -> ScriptContext
cancellationContext state signedByProver =
  let context = spendContext
        (stepDatum state)
        cancelRedeemer
        [threadInputWithName duplicateThreadName]
        []
        []
        [cancelMintEntry duplicateThreadName]
        mempty
   in if signedByProver then context else withoutSignatories context

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo
