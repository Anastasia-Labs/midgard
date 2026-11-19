{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsCanonicalDecodability (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptHash (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofs.CanonicalDecodability
import Midgard.Validators.FraudProofs.CanonicalDecodability
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Canonical-decodability fraud proof"
  [ testGroup "total envelope verdict"
      [ verdictCase "grammatical" "\x81\x44\xde\xad\xbe\xef" 0
      , verdictCase "missing array header" "" 1
      , verdictCase "not an array header" "\x9a\x00\x00\x00\x00" 2
      , verdictCase "non-minimal array header" "\x98\x17" 3
      , verdictCase "truncated array header" "\x98" 4
      , verdictCase "missing item header" "\x81" 5
      , verdictCase "not an item header" "\x81\x00" 6
      , verdictCase "non-minimal item header" "\x81\x58\x17" 7
      , verdictCase "truncated item header" "\x81\x58" 8
      , verdictCase "truncated item payload" "\x81\x42\xff" 9
      , verdictCase "trailing bytes" "\x80\x00" 10
      ]
  , testCase "step 01 binds a miscounted body field" $
      psucceeds $ step01 $ bodyStep01Context miscounted 2 10 nextScript True
  , testCase "step 01 binds an empty committed preimage" $
      psucceeds $ step01 $ bodyStep01Context "" 2 1 nextScript True
  , testCase "step 01 binds a grammatical field without convicting" $
      psucceeds $ step01 $ bodyStep01Context grammatical 2 0 nextScript True
  , testCase "step 01 binds a witness-set field" $
      psucceeds $ step01 witnessStep01Context
  , testCase "step 01 rejects a fabricated verdict" $
      pfails $ step01 $ bodyStep01Context grammatical 0 10 nextScript True
  , testCase "step 01 rejects a fabricated field index" $
      pfails $ step01 $ bodyStep01Context miscounted 1 10 nextScript True
  , testCase "step 01 rejects uncommitted bytes" $
      pfails $ step01 $ bodyStep01ContextWithClaim grammatical miscounted 2 10 nextScript True
  , testCase "step 01 rejects a foreign next-step script" $
      pfails $ step01 $ bodyStep01Context miscounted 2 10 otherScript True
  , testCase "step 01 rejects a forged transactions root" $
      pfails $ step01 $ bodyStep01Context miscounted 2 10 nextScript False
  , testCase "step 01 rejects a code-one transaction" $
      pfails $ step01 $ bodyStep01ContextWithValidity miscounted 2 10 1
  , testCase "step 01 rejects a body claim at a witness-set field" $
      pfails $ step01 bodyClaimAtWitnessContext
  , testCase "step 01 rejects a witness claim at a body field" $
      pfails $ step01 witnessClaimAtBodyContext
  , testCase "step 01 cancels under the prover signature" $
      psucceeds $ step01 step01CancellationContext
  , testCase "step 01 rejects an unsigned cancellation" $
      pfails $ step01 $ withoutSignatories step01CancellationContext
  , testCase "step 02 convicts trailing bytes" $ psucceeds $ step02 $ finalizeContext 2 10
  , testCase "step 02 convicts an empty committed preimage" $ psucceeds $ step02 $ finalizeContext 2 1
  , testCase "step 02 convicts a witness-set field" $ psucceeds $ step02 $ finalizeContext 6 5
  , testCase "step 02 rejects a grammatical field" $ pfails $ step02 $ finalizeContext 2 0
  , testCase "step 02 rejects an out-of-range field index" $ pfails $ step02 $ finalizeContext 9 10
  , testCase "step 02 rejects a negative field index" $ pfails $ step02 $ finalizeContext (-1) 10
  , testCase "step 02 rejects an unknown verdict" $ pfails $ step02 $ finalizeContext 2 11
  , testCase "step 02 cancels under the prover signature" $ psucceeds $ step02 cancellationContext
  , testCase "step 02 rejects an unsigned cancellation" $ pfails $ step02 $ withoutSignatories cancellationContext
  ]

verdictCase :: String -> BS.ByteString -> Integer -> TestTree
verdictCase label bytes expected = testCase label $
  passertEval $ penvelopeVerdictV1 # pconstant bytes #== pconstant expected

grammatical, miscounted, witnessMalformed :: BS.ByteString
grammatical = "\x81\x44\xde\xad\xbe\xef"
miscounted = "\x81\x44\xde\xad\xbe\xef\x42\xbe\xef"
witnessMalformed = "\x81"

step01 :: forall s. ScriptContext -> Term s PUnit
step01 ctx = canonicalDecodabilityStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pdata (pconstant certificatePolicy)
  # pconstant ctx

bodyStep01Context :: BS.ByteString -> Integer -> Integer -> BS.ByteString -> Bool -> ScriptContext
bodyStep01Context committedPreimage outputField outputVerdict outputScript genuineRoot =
  bodyStep01ContextWithClaim committedPreimage committedPreimage outputField outputVerdict outputScript genuineRoot

bodyStep01ContextWithClaim :: BS.ByteString -> BS.ByteString -> Integer -> Integer -> BS.ByteString -> Bool -> ScriptContext
bodyStep01ContextWithClaim committedPreimage claimedPreimage outputField outputVerdict outputScript genuineRoot =
  bodyContext committedPreimage claimedPreimage outputField outputVerdict outputScript genuineRoot 0

bodyStep01ContextWithValidity :: BS.ByteString -> Integer -> Integer -> Integer -> ScriptContext
bodyStep01ContextWithValidity preimage outputField outputVerdict validity =
  bodyContext preimage preimage outputField outputVerdict nextScript True validity

bodyContext :: BS.ByteString -> BS.ByteString -> Integer -> Integer -> BS.ByteString -> Bool -> Integer -> ScriptContext
bodyContext committedPreimage claimedPreimage outputField outputVerdict outputScript genuineRoot validity = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [inclusionArgs customTxId customSource rawRoot, bodyClaim 2 claimedPreimage]])
  [threadInput]
  [stepOutput outputScript $ Just $ state customTxId outputField outputVerdict]
  (referenceInputsWithTransactionsRoot $ if genuineRoot then countedRoot else headerTransactionsRoot)
  [phasEntry rawRoot customTxId customSource]
  mempty
  where
    body = compactBodyWithOutputCommitment committedPreimage
    customTxId = customBodyId body
    customCompact = BS.concat ["\x84", cborInt 1, body, defBytes32 (witnessSetHashOf tx1), cborInt validity]
    customSource = sourceCborFor customTxId customCompact (witnessSetCborOf tx1) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot customTxId customSource
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count

witnessStep01Context :: ScriptContext
witnessStep01Context = witnessContext (witnessClaim 6 witnessCompactData witnessMalformed) 6

bodyClaimAtWitnessContext :: ScriptContext
bodyClaimAtWitnessContext = witnessContext (bodyClaim 6 witnessMalformed) 6

witnessClaimAtBodyContext :: ScriptContext
witnessClaimAtBodyContext = bodyContextWithClaim $ witnessClaim 2 witnessCompactData miscounted

bodyContextWithClaim :: PD.Data -> ScriptContext
bodyContextWithClaim claim = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [inclusionArgs customTxId customSource rawRoot, claim]])
  [threadInput]
  [stepOutput nextScript $ Just $ state customTxId 2 10]
  (referenceInputsWithTransactionsRoot countedRoot)
  [phasEntry rawRoot customTxId customSource]
  mempty
  where
    body = compactBodyWithOutputCommitment miscounted
    customTxId = customBodyId body
    customCompact = BS.concat ["\x84", cborInt 1, body, defBytes32 (witnessSetHashOf tx1), cborInt 0]
    customSource = sourceCborFor customTxId customCompact (witnessSetCborOf tx1) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot customTxId customSource
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count

witnessContext :: PD.Data -> Integer -> ScriptContext
witnessContext claim outputField = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [inclusionArgs tx1Id witnessSource rawRoot, claim]])
  [threadInput]
  [stepOutput nextScript $ Just $ state tx1Id outputField 5]
  (referenceInputsWithTransactionsRoot countedRoot)
  [phasEntry rawRoot tx1Id witnessSource]
  mempty
  where
    witnessSource = sourceCborFor tx1Id witnessCompact (witnessSetCborFrom witnessHashes) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot tx1Id witnessSource
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count

witnessHashes :: (BS.ByteString, BS.ByteString, BS.ByteString)
witnessHashes = let (addressHash, _, redeemerHash) = witnessSetHashesOf tx1
  in (addressHash, blake2b256 witnessMalformed, redeemerHash)

witnessCompactData :: PD.Data
witnessCompactData = let (addressHash, scriptHash, redeemerHash) = witnessHashes
  in PD.Constr 0 [PD.B addressHash, PD.B scriptHash, PD.B redeemerHash]

witnessCompact :: BS.ByteString
witnessCompact = compactWithValidity tx1 (blake2b256 $ witnessSetCborFrom witnessHashes) 0

bodyClaim :: Integer -> BS.ByteString -> PD.Data
bodyClaim fieldIndex preimage = PD.Constr 0 [PD.I fieldIndex, PD.Constr 0 [PD.B preimage]]

witnessClaim :: Integer -> PD.Data -> BS.ByteString -> PD.Data
witnessClaim fieldIndex witnessSet preimage =
  PD.Constr 1 [PD.I fieldIndex, witnessSet, PD.Constr 0 [PD.B preimage]]

state :: BS.ByteString -> Integer -> Integer -> PD.Data
state txId fieldIndex verdict = PD.Constr 0 [PD.B txId, PD.I fieldIndex, PD.I verdict]

compactBodyWithOutputCommitment :: BS.ByteString -> BS.ByteString
compactBodyWithOutputCommitment outputs = BS.concat
  [ "\x8c"
  , defBytes32 $ blake2b256 $ spendInputsPreimage tx1
  , defBytes32 $ blake2b256 $ referenceInputsPreimage tx1
  , defBytes32 $ blake2b256 outputs
  , cborInt $ tFee tx1
  , cborInt $ tValidityStart tx1
  , cborInt $ tValidityEnd tx1
  , defBytes32 $ hash32 0x04
  , defBytes32 $ blake2b256 $ requiredSignersPreimage tx1
  , defBytes32 $ hash32 0x06
  , defBytes32 $ hash32 0x07
  , defBytes32 $ hash32 0x08
  , cborInt 1
  ]

customBodyId :: BS.ByteString -> BS.ByteString
customBodyId body = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> body)

step01CancellationContext :: ScriptContext
step01CancellationContext = spendContext
  (stepDatum Nothing) cancelRedeemer [threadInput] [] [] [cancelMintEntry threadName] mempty

step02 :: forall s. ScriptContext -> Term s PUnit
step02 ctx = canonicalDecodabilityStep02Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

finalizeContext :: Integer -> Integer -> ScriptContext
finalizeContext fieldIndex verdict = spendContext
  (stepDatum $ Just $ PD.Constr 0 [PD.B tx1Id, PD.I fieldIndex, PD.I verdict])
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  []
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

cancellationContext :: ScriptContext
cancellationContext = spendContext
  (stepDatum $ Just $ PD.Constr 0 [PD.B tx1Id, PD.I 2, PD.I 10])
  cancelRedeemer
  [threadInput]
  []
  []
  [cancelMintEntry threadName]
  mempty

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo
