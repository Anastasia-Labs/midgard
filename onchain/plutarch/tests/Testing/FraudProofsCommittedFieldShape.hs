{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsCommittedFieldShape (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptHash (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofs.CommittedFieldShape
import Midgard.Validators.FraudProofs.CommittedFieldShape
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Committed-field-shape fraud proof"
  [ testGroup "shape verdict"
      [ verdictCase "empty walked field is admissible" 2 "\x80" 0
      , verdictCase "empty fixed-stride field is admissible" 0 "\x80" 0
      , verdictCase "honest spend-input stride is admissible" 0 honestSpend 0
      , verdictCase "four-byte item is admissible at a walked field" 2 wrongStride 0
      , verdictCase "four-byte item has wrong spend-input stride" 0 wrongStride 3
      , verdictCase "four-byte item has wrong reference-input stride" 1 wrongStride 3
      , verdictCase "four-byte item has wrong observer stride" 3 wrongStride 3
      , verdictCase "four-byte item has wrong signer stride" 4 wrongStride 3
      , verdictCase "four-byte item has wrong address-witness stride" 7 wrongStride 3
      , verdictCase "one byte over the spend stride is wrong" 0 oneOverStride 3
      , verdictCase "one byte under the spend stride is wrong" 0 oneUnderStride 3
      , verdictCase "non-envelope is deferred to canonical-decodability" 2 nonEnvelope 1
      , verdictCase "field at byte bound is admissible" 2 atByteBound 0
      , verdictCase "field over byte bound is convicted" 2 aboveByteBound 2
      , verdictCase "byte bound precedes fixed-stride failure" 0 aboveByteBound 2
      , testCase "negative field index aborts" $ pfails $ pcommittedFieldShapeVerdictV1 # (-1) # pconstant "\x80"
      , testCase "out-of-range field index aborts" $ pfails $ pcommittedFieldShapeVerdictV1 # 9 # pconstant "\x80"
      ]
  , testCase "step 01 binds a wrong-stride body field" $
      psucceeds $ step01 $ bodyContext 0 wrongStride wrongStride 0 3 nextScript True
  , testCase "step 01 binds a right-stride body field without convicting" $
      psucceeds $ step01 $ bodyContext 0 honestSpend honestSpend 0 0 nextScript True
  , testCase "step 01 binds the same bytes at a walked field" $
      psucceeds $ step01 $ bodyContext 2 wrongStride wrongStride 2 0 nextScript True
  , testCase "step 01 binds a non-envelope without convicting" $
      psucceeds $ step01 $ bodyContext 2 nonEnvelope nonEnvelope 2 1 nextScript True
  , testCase "step 01 binds a wrong-stride witness field" $
      psucceeds $ step01 $ witnessContext wrongStride wrongStride 7 3
  , testCase "step 01 binds a right-stride witness field without convicting" $
      psucceeds $ step01 $ witnessContext honestAddressWitness honestAddressWitness 7 0
  , testCase "step 01 rejects a fabricated verdict" $
      pfails $ step01 $ bodyContext 0 honestSpend honestSpend 0 3 nextScript True
  , testCase "step 01 rejects a fabricated field index" $
      pfails $ step01 $ bodyContext 0 wrongStride wrongStride 1 3 nextScript True
  , testCase "step 01 rejects uncommitted bytes" $
      pfails $ step01 $ bodyContext 0 honestSpend wrongStride 0 3 nextScript True
  , testCase "step 01 rejects a foreign next-step script" $
      pfails $ step01 $ bodyContext 0 wrongStride wrongStride 0 3 otherScript True
  , testCase "step 01 rejects a forged transactions root" $
      pfails $ step01 $ bodyContext 0 wrongStride wrongStride 0 3 nextScript False
  , testCase "step 01 rejects a code-one transaction" $
      pfails $ step01 $ bodyContextWithValidity 0 wrongStride wrongStride 0 3 1
  , testCase "step 01 rejects a body claim at a witness field" $
      pfails $ step01 bodyClaimAtWitnessContext
  , testCase "step 01 rejects a witness claim at a body field" $
      pfails $ step01 witnessClaimAtBodyContext
  , testCase "step 01 cancels under the prover signature" $ psucceeds $ step01 step01CancellationContext
  , testCase "step 01 rejects unsigned cancellation" $ pfails $ step01 $ withoutSignatories step01CancellationContext
  , testCase "step 02 convicts wrong body stride" $ psucceeds $ step02 $ finalizeContext 0 3
  , testCase "step 02 convicts wrong witness stride" $ psucceeds $ step02 $ finalizeContext 7 3
  , testCase "step 02 convicts an oversize field" $ psucceeds $ step02 $ finalizeContext 2 2
  , testCase "step 02 rejects an admissible field" $ pfails $ step02 $ finalizeContext 0 0
  , testCase "step 02 rejects a non-envelope" $ pfails $ step02 $ finalizeContext 2 1
  , testCase "step 02 rejects an out-of-range field" $ pfails $ step02 $ finalizeContext 9 3
  , testCase "step 02 rejects a negative field" $ pfails $ step02 $ finalizeContext (-1) 3
  , testCase "step 02 rejects an unknown verdict" $ pfails $ step02 $ finalizeContext 2 4
  , testCase "step 02 cancels under the prover signature" $ psucceeds $ step02 step02CancellationContext
  , testCase "step 02 rejects unsigned cancellation" $ pfails $ step02 $ withoutSignatories step02CancellationContext
  ]

verdictCase :: String -> Integer -> BS.ByteString -> Integer -> TestTree
verdictCase label fieldIndex bytes expected = testCase label $
  passertEval $ pcommittedFieldShapeVerdictV1 # pconstant fieldIndex # pconstant bytes #== pconstant expected

wrongStride, honestSpend, honestAddressWitness, nonEnvelope, oneOverStride, oneUnderStride :: BS.ByteString
wrongStride = "\x81\x44\xde\xad\xbe\xef"
honestSpend = spendInputsPreimage tx1
honestAddressWitness = addressWitnessesPreimage tx1
nonEnvelope = "\x81\x44\xde\xad\xbe\xef\x42\xbe\xef"
oneOverStride = "\x81\x58\x27" <> BS.replicate 39 0xaa
oneUnderStride = "\x81\x58\x25" <> BS.replicate 37 0xaa

atByteBound, aboveByteBound :: BS.ByteString
atByteBound = sizedEnvelope 32768
aboveByteBound = sizedEnvelope 32769

sizedEnvelope :: Int -> BS.ByteString
sizedEnvelope totalLength =
  "\x81\x59" <> BS.pack [fromIntegral (payloadLength `div` 256), fromIntegral payloadLength]
    <> BS.replicate payloadLength 0xaa
  where
    payloadLength = totalLength - 4

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx = committedFieldShapeStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pdata (pconstant certificatePolicy)
  # pconstant ctx
step02 ctx = committedFieldShapeStep02Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

bodyContext :: Integer -> BS.ByteString -> BS.ByteString -> Integer -> Integer -> BS.ByteString -> Bool -> ScriptContext
bodyContext fieldIndex committedPreimage claimedPreimage outputField outputVerdict outputScript genuineRoot =
  bodyContextWith fieldIndex committedPreimage (bodyClaim fieldIndex claimedPreimage)
    outputField outputVerdict outputScript genuineRoot 0

bodyContextWithValidity :: Integer -> BS.ByteString -> BS.ByteString -> Integer -> Integer -> Integer -> ScriptContext
bodyContextWithValidity fieldIndex committedPreimage claimedPreimage outputField outputVerdict validity =
  bodyContextWith fieldIndex committedPreimage (bodyClaim fieldIndex claimedPreimage)
    outputField outputVerdict nextScript True validity

bodyContextWith :: Integer -> BS.ByteString -> PD.Data -> Integer -> Integer -> BS.ByteString -> Bool -> Integer -> ScriptContext
bodyContextWith fieldIndex committedPreimage claim outputField outputVerdict outputScript genuineRoot validity = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [inclusionArgs customTxId customSource rawRoot, claim]])
  [threadInput]
  [stepOutput outputScript $ Just $ state customTxId outputField outputVerdict]
  (referenceInputsWithTransactionsRoot $ if genuineRoot then countedRoot else headerTransactionsRoot)
  [phasEntry rawRoot customTxId customSource]
  mempty
  where
    body = compactBodyWithField fieldIndex committedPreimage
    customTxId = customBodyId body
    customCompact = BS.concat ["\x84", cborInt 1, body, defBytes32 (witnessSetHashOf tx1), cborInt validity]
    customSource = sourceCborFor customTxId customCompact (witnessSetCborOf tx1) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot customTxId customSource
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count

witnessContext :: BS.ByteString -> BS.ByteString -> Integer -> Integer -> ScriptContext
witnessContext committedPreimage claimedPreimage outputField outputVerdict = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [inclusionArgs tx1Id source rawRoot, witnessClaim 7 witnessData claimedPreimage]])
  [threadInput]
  [stepOutput nextScript $ Just $ state tx1Id outputField outputVerdict]
  (referenceInputsWithTransactionsRoot countedRoot)
  [phasEntry rawRoot tx1Id source]
  mempty
  where
    (_, scriptHash, redeemerHash) = witnessSetHashesOf tx1
    hashes = (blake2b256 committedPreimage, scriptHash, redeemerHash)
    witnessData = let (addressHash, scriptsHash, redsHash) = hashes
      in PD.Constr 0 [PD.B addressHash, PD.B scriptsHash, PD.B redsHash]
    compact = compactWithValidity tx1 (blake2b256 $ witnessSetCborFrom hashes) 0
    source = sourceCborFor tx1Id compact (witnessSetCborFrom hashes) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot tx1Id source
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count

bodyClaimAtWitnessContext :: ScriptContext
bodyClaimAtWitnessContext = witnessContextWithClaim wrongStride (bodyClaim 7 wrongStride) 7 3

witnessClaimAtBodyContext :: ScriptContext
witnessClaimAtBodyContext = bodyContextWith 0 wrongStride (witnessClaim 0 defaultWitnessData wrongStride)
  0 3 nextScript True 0

witnessContextWithClaim :: BS.ByteString -> PD.Data -> Integer -> Integer -> ScriptContext
witnessContextWithClaim committedPreimage claim outputField outputVerdict = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [inclusionArgs tx1Id source rawRoot, claim]])
  [threadInput]
  [stepOutput nextScript $ Just $ state tx1Id outputField outputVerdict]
  (referenceInputsWithTransactionsRoot countedRoot)
  [phasEntry rawRoot tx1Id source]
  mempty
  where
    (_, scriptHash, redeemerHash) = witnessSetHashesOf tx1
    hashes = (blake2b256 committedPreimage, scriptHash, redeemerHash)
    compact = compactWithValidity tx1 (blake2b256 $ witnessSetCborFrom hashes) 0
    source = sourceCborFor tx1Id compact (witnessSetCborFrom hashes) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot tx1Id source
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count

defaultWitnessData :: PD.Data
defaultWitnessData = let (addressHash, scriptHash, redeemerHash) = witnessSetHashesOf tx1
  in PD.Constr 0 [PD.B addressHash, PD.B scriptHash, PD.B redeemerHash]

bodyClaim :: Integer -> BS.ByteString -> PD.Data
bodyClaim fieldIndex preimage = PD.Constr 0 [PD.I fieldIndex, PD.Constr 0 [PD.B preimage]]

witnessClaim :: Integer -> PD.Data -> BS.ByteString -> PD.Data
witnessClaim fieldIndex witnessSet preimage = PD.Constr 1 [PD.I fieldIndex, witnessSet, PD.Constr 0 [PD.B preimage]]

state :: BS.ByteString -> Integer -> Integer -> PD.Data
state txId fieldIndex verdict = PD.Constr 0 [PD.B txId, PD.I fieldIndex, PD.I verdict]

compactBodyWithField :: Integer -> BS.ByteString -> BS.ByteString
compactBodyWithField fieldIndex preimage = BS.concat
  [ "\x8c"
  , defBytes32 $ commitment 0 $ spendInputsPreimage tx1
  , defBytes32 $ commitment 1 $ referenceInputsPreimage tx1
  , defBytes32 $ commitment 2 $ outputsPreimage tx1
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
  where
    commitment slot original = blake2b256 $ if fieldIndex == slot then preimage else original

customBodyId :: BS.ByteString -> BS.ByteString
customBodyId body = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> body)

finalizeContext :: Integer -> Integer -> ScriptContext
finalizeContext fieldIndex verdict = spendContext
  (stepDatum $ Just $ state tx1Id fieldIndex verdict)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  []
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

step01CancellationContext, step02CancellationContext :: ScriptContext
step01CancellationContext = spendContext (stepDatum Nothing) cancelRedeemer [threadInput] [] [] [cancelMintEntry threadName] mempty
step02CancellationContext = spendContext (stepDatum $ Just $ state tx1Id 0 3) cancelRedeemer [threadInput] [] [] [cancelMintEntry threadName] mempty

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) = ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo
