{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsFabricatedWithdrawal (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName (..), singleton, unCurrencySymbol)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
  ScriptContext,
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.FabricatedWithdrawal
import Testing.Eval (pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Fabricated withdrawal fraud proof"
    [ testGroup "step 01" step01Tests
    , testGroup "step 02" step02Tests
    , testGroup "step 03" step03Tests
    , testGroup "step 04" step04Tests
    ]

psucceeds, pfails :: (forall s. Term s a) -> Assertion
psucceeds = psucceedsNoTraceWithoutHoistChecks
pfails = pfailsNoTraceWithoutHoistChecks

step01Tests :: [TestTree]
step01Tests =
  [ testCase "accepts committed withdrawal membership" $
      psucceeds $
        step01 $
          step01Context authenticBlock (bProof authenticBlock) fabricatedThreadName
  , testCase "rejects forged root" $
      pfails $
        step01 $
          step01Context authenticBlock (bProof foreignBlock) fabricatedThreadName
  , testCase "rejects wrong count" $
      pfails $
        step01 $
          step01Context authenticBlock (withProofCount 2 $ bProof authenticBlock) fabricatedThreadName
  , testCase "rejects wrong category" $
      pfails $
        step01 $
          step01Context authenticBlock (bProof authenticBlock) threadName
  ]

step02Tests :: [TestTree]
step02Tests =
  [ testCase "accepts authenticated L1 withdrawal evidence" $ do
      psucceeds $ step02 $ absentContext fabricatedWithdrawalId
      psucceeds $ step02 $ presentContext $ eventReferenceInput withdrawalPolicy
  , testCase "rejects wrong NFT policy" $
      pfails $
        step02 $
          presentContext $
            eventReferenceInput (policyFor 0x1e)
  , testCase "rejects wrong event output reference" $
      pfails $
        step02 $
          absentContext foreignWithdrawalId
  , testCase "rejects untrusted evidence" $
      pfails $
        step02 $
          presentContext untrustedEventReferenceInput
  ]

step03Tests :: [TestTree]
step03Tests =
  [ testCase "accepts withdrawal content mismatches and absent identity" $ do
      psucceeds $ step03 $ fidelityContext divertedWithdrawalInfo
      psucceeds $ step03 $ fidelityContext forgedSignatureWithdrawalInfo
      psucceeds $ step03 $ fidelityContext overriddenValidityWithdrawalInfo
      psucceeds $ step03 absentOpeningContext
  , testCase "rejects exact info match" $
      pfails $
        step03 exactInfoContext
  , testCase "rejects wrong body bytes" $
      pfails $
        step03 wrongBodyContext
  , testCase "rejects forged verdict handoff" $
      pfails $
        step03 forgedHandoffContext
  ]

step04Tests :: [TestTree]
step04Tests =
  [ testCase "accepts exact fabricated-withdrawal faults" $ do
      psucceeds $ step04 $ finalContext mismatchFinalState fabricatedThreadName
      psucceeds $ step04 $ finalContext absentFinalState fabricatedThreadName
  , testCase "rejects authentic valid withdrawal" $
      pfails $
        step04 $
          finalContext authenticFinalState fabricatedThreadName
  , testCase "rejects stale evidence" $
      pfails $
        step04 $
          finalContext staleFinalState fabricatedThreadName
  , testCase "rejects wrong source identity" $
      pfails $
        step04 $
          finalContext mismatchFinalState wrongHeaderThreadName
  ]

data Block = Block
  { bId :: PD.Data
  , bInfo :: PD.Data
  , bRoot :: BS.ByteString
  , bProof :: PD.Data
  }

mkBlock :: PD.Data -> PD.Data -> Block
mkBlock withdrawalId info = Block withdrawalId info countedRoot proof
  where
    rawRoot = singleEntryPhasRoot (serialise withdrawalId) (serialise info)
    countedRoot = commitCountedRoot withdrawalsDomain rawRoot 1
    proof = membershipProof withdrawalsDomain countedRoot rawRoot 1 withdrawalId info

authenticWithdrawalId, fabricatedWithdrawalId, foreignWithdrawalId :: PD.Data
authenticWithdrawalId = PD.Constr 0 [PD.B $ BS.replicate 32 0x8b, PD.I 2]
fabricatedWithdrawalId = PD.Constr 0 [PD.B $ BS.replicate 32 0x3a, PD.I 0]
foreignWithdrawalId = PD.Constr 0 [PD.B $ BS.replicate 32 0xc4, PD.I 1]

authenticWithdrawalInfo, divertedWithdrawalInfo, forgedSignatureWithdrawalInfo, overriddenValidityWithdrawalInfo :: PD.Data
authenticWithdrawalInfo = withdrawalInfo address2b signatureBe (PD.Constr 0 [])
divertedWithdrawalInfo = withdrawalInfo address5d signatureBe (PD.Constr 0 [])
forgedSignatureWithdrawalInfo = withdrawalInfo address2b signatureF0 (PD.Constr 0 [])
overriddenValidityWithdrawalInfo = withdrawalInfo address2b signatureBe (PD.Constr 1 [])

withdrawalInfo :: PD.Data -> PD.Data -> PD.Data -> PD.Data
withdrawalInfo l1Address signature validity =
  PD.Constr
    0
    [ PD.Constr
        0
        [ PD.Constr 0 [PD.B $ BS.replicate 32 0x7e, PD.I 1]
        , PD.B $ BS.replicate 28 0x9c
        , PD.List []
        , l1Address
        , PD.Constr 0 []
        ]
    , signature
    , validity
    ]

address2b, address5d :: PD.Data
address2b = addressData 0x2b
address5d = addressData 0x5d

addressData :: Word -> PD.Data
addressData byte =
  PD.Constr 0 [PD.Constr 0 [PD.B $ BS.replicate 28 $ fromIntegral byte], PD.Constr 1 []]

signatureBe, signatureF0 :: PD.Data
signatureBe = PD.List [PD.B $ BS.replicate 32 0xad, PD.B $ BS.replicate 64 0xbe]
signatureF0 = PD.List [PD.B $ BS.replicate 32 0xad, PD.B $ BS.replicate 64 0xf0]

authenticBlock, fabricatedIdentityBlock, mismatchedBlock, foreignBlock :: Block
authenticBlock = mkBlock authenticWithdrawalId authenticWithdrawalInfo
fabricatedIdentityBlock = mkBlock fabricatedWithdrawalId authenticWithdrawalInfo
mismatchedBlock = mkBlock authenticWithdrawalId divertedWithdrawalInfo
foreignBlock = mkBlock foreignWithdrawalId authenticWithdrawalInfo

fabricatedThreadName, wrongHeaderThreadName :: BS.ByteString
fabricatedThreadName = BS.pack [0, 0, 0, 0x0c] <> BS.replicate 28 0xaa
wrongHeaderThreadName = BS.pack [0, 0, 0, 0x0c] <> BS.replicate 28 0xbb

withProofCount :: Integer -> PD.Data -> PD.Data
withProofCount count (PD.Constr tag [domain, root, rawRoot, _, key, value, proof]) =
  PD.Constr tag [domain, root, rawRoot, PD.I count, key, value, proof]
withProofCount _ _ = error "invalid membership proof fixture"

infoHash :: PD.Data -> BS.ByteString
infoHash = blake2b256 . serialise

step02State :: Block -> PD.Data
step02State block =
  PD.Constr
    0
    [ PD.B $ BS.replicate 28 0xaa
    , PD.I 100
    , PD.I 200
    , bId block
    , PD.B $ infoHash $ bInfo block
    ]

step03State :: Block -> PD.Data -> PD.Data
step03State block verdict =
  PD.Constr
    0
    [ PD.B $ BS.replicate 28 0xaa
    , PD.I 100
    , PD.I 200
    , bId block
    , PD.B $ infoHash $ bInfo block
    , verdict
    ]

step04State :: Block -> PD.Data -> PD.Data
step04State block fault =
  PD.Constr
    0
    [ PD.B $ BS.replicate 28 0xaa
    , PD.I 100
    , PD.I 200
    , bId block
    , fault
    ]

withdrawalIdentityAbsent, observedVerdict :: PD.Data
withdrawalIdentityAbsent = PD.Constr 0 []
observedVerdict = PD.Constr 1 [PD.B $ blake2b256 $ serialise authenticWithdrawalDatum, PD.I inclusionTime]

nonexistentFault, mismatchFault, authenticFault, staleFault :: PD.Data
nonexistentFault = PD.Constr 0 []
mismatchFault = mismatchFaultAt inclusionTime divertedWithdrawalInfo
authenticFault = mismatchFaultAt inclusionTime authenticWithdrawalInfo
staleFault = mismatchFaultAt 201 divertedWithdrawalInfo

mismatchFaultAt :: Integer -> PD.Data -> PD.Data
mismatchFaultAt time committedInfo =
  PD.Constr 1 [PD.B $ infoHash committedInfo, PD.B $ infoHash authenticWithdrawalInfo, PD.I time]

mismatchFinalState, absentFinalState, authenticFinalState, staleFinalState :: PD.Data
mismatchFinalState = step04State mismatchedBlock mismatchFault
absentFinalState = step04State fabricatedIdentityBlock nonexistentFault
authenticFinalState = step04State authenticBlock authenticFault
staleFinalState = step04State mismatchedBlock staleFault

step01, step02, step03, step04 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  fabricatedWithdrawalStep01Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  fabricatedWithdrawalStep02Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step03 ctx =
  fabricatedWithdrawalStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant ctx
step04 ctx =
  fabricatedWithdrawalStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

step01Context :: Block -> PD.Data -> BS.ByteString -> ScriptContext
step01Context challenged proof assetName =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 1, proof]])
    [threadInputWithName assetName]
    [stepOutputWithName nextScript (Just $ step02StateForProof proof) assetName]
    (referenceInputsWithWithdrawalsRoot (bRoot challenged) 1)
    []
    mempty

step02StateForProof :: PD.Data -> PD.Data
step02StateForProof (PD.Constr _ [_domain, _root, _rawRoot, _count, key, value, _proof]) =
  PD.Constr 0 [PD.B $ BS.replicate 28 0xaa, PD.I 100, PD.I 200, key, PD.B $ infoHash value]
step02StateForProof _ = error "invalid membership proof fixture"

withdrawalPolicy :: CurrencySymbol
withdrawalPolicy = policyFor 0x48

inclusionTime :: Integer
inclusionTime = 150

authenticWithdrawalDatum, tamperedWithdrawalDatum :: PD.Data
authenticWithdrawalDatum = withdrawalDatum authenticWithdrawalInfo
tamperedWithdrawalDatum = withdrawalDatum divertedWithdrawalInfo

withdrawalDatum :: PD.Data -> PD.Data
withdrawalDatum info =
  PD.Constr
    0
    [ PD.Constr 0 [authenticWithdrawalId, info]
    , PD.I inclusionTime
    , PD.B $ BS.replicate 28 0x57
    , address2b
    , PD.Constr 0 []
    ]

unspentReferenceInput :: TxOutRef -> TxInInfo
unspentReferenceInput outRef =
  TxInInfo
    outRef
    (TxOut (pubKeyHashAddress $ PubKeyHash $ toBuiltin $ BS.replicate 28 0x4e) (adaValue 9_000_000) NoOutputDatum Nothing)

eventReferenceInput :: CurrencySymbol -> TxInInfo
eventReferenceInput policy =
  TxInInfo
    (outRefN 8)
    ( TxOut
        (scriptHashAddress $ ScriptHash $ unCurrencySymbol policy)
        (adaValue 3_000_000 <> singleton policy eventName 1)
        (OutputDatum $ Datum $ dataToBuiltinData authenticWithdrawalDatum)
        Nothing
    )
  where
    eventName = TokenName $ toBuiltin $ blake2b256 $ serialise authenticWithdrawalId

untrustedEventReferenceInput :: TxInInfo
untrustedEventReferenceInput =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (pubKeyHashAddress $ PubKeyHash $ toBuiltin $ BS.replicate 28 0x3f)
        (adaValue 3_000_000)
        (OutputDatum $ Datum $ dataToBuiltinData authenticWithdrawalDatum)
        Nothing
    )

absentContext :: PD.Data -> ScriptContext
absentContext witnessedId =
  step02Context
    fabricatedIdentityBlock
    (PD.Constr 0 [PD.I 0])
    withdrawalIdentityAbsent
    [unspentReferenceInput $ outRefFromData witnessedId]

outRefFromData :: PD.Data -> TxOutRef
outRefFromData (PD.Constr 0 [PD.B txId, PD.I index]) = TxOutRef (TxId $ toBuiltin txId) index
outRefFromData _ = error "invalid output reference fixture"

presentContext :: TxInInfo -> ScriptContext
presentContext eventInput =
  step02Context
    mismatchedBlock
    (PD.Constr 1 [PD.I 0, PD.I 1])
    observedVerdict
    (take 1 referenceInputs <> [eventInput])

step02Context :: Block -> PD.Data -> PD.Data -> [TxInInfo] -> ScriptContext
step02Context block evidence verdict refInputs =
  spendContext
    (stepDatum $ Just $ step02State block)
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, evidence]])
    [threadInputWithName fabricatedThreadName]
    [stepOutputWithName nextScript (Just $ step03State block verdict) fabricatedThreadName]
    refInputs
    []
    mempty

fidelityContext :: PD.Data -> ScriptContext
fidelityContext committedInfo =
  let block = mkBlock authenticWithdrawalId committedInfo
      fault = mismatchFaultAt inclusionTime committedInfo
   in step03Context
        (step03State block observedVerdict)
        (PD.Constr 1 [authenticWithdrawalDatum])
        (step04State block fault)

absentOpeningContext :: ScriptContext
absentOpeningContext =
  step03Context
    (step03State fabricatedIdentityBlock withdrawalIdentityAbsent)
    (PD.Constr 0 [])
    absentFinalState

exactInfoContext :: ScriptContext
exactInfoContext =
  step03Context
    (step03State authenticBlock observedVerdict)
    (PD.Constr 1 [authenticWithdrawalDatum])
    mismatchFinalState

wrongBodyContext :: ScriptContext
wrongBodyContext =
  step03Context
    (step03State mismatchedBlock observedVerdict)
    (PD.Constr 1 [tamperedWithdrawalDatum])
    mismatchFinalState

forgedHandoffContext :: ScriptContext
forgedHandoffContext =
  step03Context
    (step03State mismatchedBlock observedVerdict)
    (PD.Constr 0 [])
    absentFinalState

step03Context :: PD.Data -> PD.Data -> PD.Data -> ScriptContext
step03Context inputState opening outputState =
  spendContext
    (stepDatum $ Just inputState)
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, opening]])
    [threadInputWithName fabricatedThreadName]
    [stepOutputWithName nextScript (Just outputState) fabricatedThreadName]
    []
    []
    mempty

finalContext :: PD.Data -> BS.ByteString -> ScriptContext
finalContext state assetName =
  spendContext
    (stepDatum $ Just state)
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
    [threadInputWithName assetName]
    [convictionOutput fraudProofAddress assetName]
    []
    [fraudProofMintEntry assetName]
    (singleton fpPolicy (TokenName $ toBuiltin assetName) 1)
