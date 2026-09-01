{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.NativeTxFaultStatement (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.NativeTxFaultStatement
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1 (..),
  PFieldViewV1,
  pchunkBytesK,
  pfieldTotalLength,
 )
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  popenFieldWalk,
  pwalkNextItemIndex,
  pwalkSkip,
 )
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.NativeTxFaultStatement"
  [ statementTests
  , itemTests
  , outputTests
  , mintTests
  , conservationTests
  , tier2Tests
  ]

statementTests :: TestTree
statementTests = testGroup "wire"
  [ testCase "item fault statement is constant width" $ passertEvalNoTrace constantWidth
  , testCase "hash is domain over wire form" $ passertEvalNoTrace hashIsDomainOverWire
  , testCase "item statement round trips" $ passertEvalNoTrace itemRoundTrip
  , testCase "conservation statement round trips" $ passertEvalNoTrace conservationRoundTrip
  , testCase "conservation width follows unit" $ passertEvalNoTrace conservationWidth
  , testCase "item fault naming a unit is refused" $ pfails itemNamesUnit
  , testCase "item fault claiming a quantity is refused" $ pfails itemClaimsQuantity
  , testCase "short policy id is refused" $ pfails shortPolicy
  , testCase "conservation statement naming another field is refused" $ pfails conservationOtherField
  , testCase "conservation statement naming a starting item is refused" $ pfails conservationStartingItem
  , testCase "non-canonical sign is refused" $ pfails nonCanonicalSign
  , testCase "unvalidated wrapper byte is refused" $ pfails badArrayWrapper
  ]

itemTests :: TestTree
itemTests = testGroup "item"
  [ testCase "fault statement carries no preimage bytes" $ passertEvalNoTrace carriesNoPreimage
  , testCase "statement about another transaction is refused" $ pfails anotherTransaction
  , testCase "reads exactly the named item" $ passertEvalNoTrace readsNamedItem
  , testCase "does not prove an honest item" $ passertEvalNoTrace honestItemNotProven
  , testCase "backwards statement is refused" $ pfails backwardsStatement
  , testCase "statement about another field is refused" $ pfails anotherField
  , testCase "malformed statement is refused" $ pfails malformedItemStatement
  , testCase "conservation statement is refused" $ pfails conservationAsItem
  ]

outputTests :: TestTree
outputTests = testGroup "output"
  [ testCase "reads one asset out of many" $ passertEvalNoTrace outputReadsAsset
  , testCase "reports absence as zero" $ passertEvalNoTrace outputReportsAbsence
  , testCase "accumulates across a budget boundary" $ passertEvalNoTrace outputResumes
  , testCase "unfinished running total is refused" $ pfails unfinishedOutputQuantity
  , testCase "walk over another field is refused" $ pfails outputAnotherField
  , testCase "fold opened past the start is refused" $ pfails outputOpenedPastStart
  ]

mintTests :: TestTree
mintTests = testGroup "mint"
  [ testCase "reads one asset out of many" $ passertEvalNoTrace mintReadsAsset
  , testCase "reads a burn" $ passertEvalNoTrace mintReadsBurn
  , testCase "reports absent policy and name as zero" $ passertEvalNoTrace mintReportsAbsence
  , testCase "unordered field is refused" $ pfails unorderedMint
  , testCase "zero quantity is refused" $ pfails zeroMintQuantity
  , testCase "resumes across a budget boundary" $ passertEvalNoTrace mintResumes
  , testCase "sweep opened past target is refused" $ pfails mintOpenedPastTarget
  , testCase "budget exhausted sweep is refused" $ pfails mintBudgetExhausted
  , testCase "order is enforced across budget rounds" $ pfails unorderedMintAcrossRounds
  , testCase "walk over another field is refused" $ pfails mintAnotherField
  ]

conservationTests :: TestTree
conservationTests = testGroup "conservation"
  [ testCase "fault is exactly the disagreement" $ passertEvalNoTrace conservationDisagreement
  , testCase "sweeps of another transaction are refused" $ pfails conservationOtherTransaction
  , testCase "sweep about another unit is refused" $ pfails conservationOtherUnit
  , testCase "unfinished mint sweep is refused" $ pfails conservationUnfinishedMint
  , testCase "unfinished output sweep is refused" $ pfails conservationUnfinishedOutput
  , testCase "statement naming another field is refused" $ pfails conservationStatementOtherField
  , testCase "statement naming a starting item is refused" $ pfails conservationStatementStartingItem
  , testCase "item statement is refused" $ pfails itemAsConservation
  , testCase "malformed statement is refused" $ pfails malformedConservationStatement
  ]

tier2Tests :: TestTree
tier2Tests = testGroup "tier2"
  [ testCase "fixtures sit at the tier-two bound" $ passertEvalNoTrace tier2FixturesBound
  , testCase "fixed stride one open" $ passertEvalNoTrace tier2FixedOneOpen
  , testCase "fixed stride two opens" $ passertEvalNoTrace tier2FixedTwoOpens
  , testCase "variable width one open" $ passertEvalNoTrace tier2VariableOneOpen
  , testCase "variable width two opens" $ passertEvalNoTrace tier2VariableTwoOpens
  ]

constantWidth :: forall s. Term s PBool
constantWidth =
  plengthBS # (pencodeFaultStatement # pitemStatement 6 0) #== pitemFaultStatementBytes
    #&& plengthBS # (pencodeFaultStatement # pitemStatement 6 65535) #== pitemFaultStatementBytes
    #&& plengthBS # (pencodeFaultStatement # pitemStatement 0 295) #== pitemFaultStatementBytes

hashIsDomainOverWire :: forall s. Term s PBool
hashIsDomainOverWire =
  plet (pitemStatement 6 4) $ \item ->
  plet (pconservationStatement (-42)) $ \asset ->
    pfaultStatementHash # item
      #== pblake2b_256 # (pconstant statementDomain <> (pencodeFaultStatement # item))
      #&& pfaultStatementHash # asset
      #== pblake2b_256 # (pconstant statementDomain <> (pencodeFaultStatement # asset))
      #&& plengthBS # (pfaultStatementHash # item) #== 32
      #&& pfaultStatementHash # item #/= pfaultStatementHash # asset

itemRoundTrip :: forall s. Term s PBool
itemRoundTrip = plet (pitemStatement 8 12) $ \statement ->
  pdecodeFaultStatement # (pencodeFaultStatement # statement) #== statement

conservationRoundTrip :: forall s. Term s PBool
conservationRoundTrip = plet (pconservationStatement (-42)) $ \statement ->
  pdecodeFaultStatement # (pencodeFaultStatement # statement) #== statement

conservationWidth :: forall s. Term s PBool
conservationWidth =
  plengthBS # (pencodeFaultStatement # pconservationStatement (-42))
    #== pfaultStatementFrameBytes + 30 + 5

itemNamesUnit, itemClaimsQuantity, shortPolicy, conservationOtherField,
  conservationStartingItem, nonCanonicalSign, badArrayWrapper :: forall s. Term s PBool
itemNamesUnit = pforceEncode $ pstatement sampleTxId pfaultItemPredicate 6 1 targetPolicy "" 0
itemClaimsQuantity = pforceEncode $ pstatement sampleTxId pfaultItemPredicate 6 1 "" "" 5
shortPolicy = pforceEncode $ pstatement sampleTxId pfaultAssetConservation 2 0 (BS.take 27 targetPolicy) targetName 0
conservationOtherField = pforceEncode $ pstatement sampleTxId pfaultAssetConservation 5 0 targetPolicy targetName 7
conservationStartingItem = pforceEncode $ pstatement sampleTxId pfaultAssetConservation 2 4 targetPolicy targetName 7
nonCanonicalSign = plet (pconservationStatement 7) $ \statement ->
  plet (pencodeFaultStatement # statement) $ \bytes ->
    pdecodeFaultStatement #
      (psliceBS # 0 # 79 # bytes <> pconstant "\x02" <> psliceBS # 80 # (plengthBS # bytes - 80) # bytes)
      #== statement
badArrayWrapper = plet (pconservationStatement 7) $ \statement ->
  plet (pencodeFaultStatement # statement) $ \bytes ->
    pdecodeFaultStatement # (pconstant "\x88" <> psliceBS # 1 # (plengthBS # bytes - 1) # bytes)
      #== statement

carriesNoPreimage :: forall s. Term s PBool
carriesNoPreimage =
  plet (pencodeFaultStatement # pitemStatementAgainst txIdA 6 4) $ \wireA ->
  plet (pencodeFaultStatement # pitemStatementAgainst txIdB 6 4) $ \wireB ->
  pmatch (popenInline txIdA 6 fraudulentItems) $ \(PPair viewA startA) ->
  pmatch
    (pproveItemFault # viewA # startA # (pdecodeFaultStatement # wireA) # pitemIsHonest)
    $ \(PPair provenA _) ->
  pmatch (popenInline txIdB 6 otherFraudulentItems) $ \(PPair viewB startB) ->
  pmatch
    (pproveItemFault # viewB # startB # (pdecodeFaultStatement # wireB) # potherItemIsHonest)
    $ \(PPair provenB _) ->
      pconstant (fraudulentItems /= otherFraudulentItems)
        #&& plengthBS # wireA #== pitemFaultStatementBytes
        #&& plengthBS # wireB #== pitemFaultStatementBytes
        #&& psliceBS # 0 # 3 # wireA #== psliceBS # 0 # 3 # wireB
        #&& psliceBS # 35 # (plengthBS # wireA - 35) # wireA
          #== psliceBS # 35 # (plengthBS # wireB - 35) # wireB
        #&& psliceBS # 3 # 32 # wireA #== pconstant txIdA
        #&& psliceBS # 3 # 32 # wireB #== pconstant txIdB
        #&& provenA
        #&& provenB

readsNamedItem :: forall s. Term s PBool
readsNamedItem = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  pmatch (pproveItemFault # view # start # pitemStatement 6 4 # pitemIsHonest) $ \(PPair proven next) ->
    proven #&& pwalkNextItemIndex # next #== 5

honestItemNotProven :: forall s. Term s PBool
honestItemNotProven = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  pmatch (pproveItemFault # view # start # pitemStatement 6 3 # pitemIsHonest) $ \(PPair proven next) ->
    pnot # proven #&& pwalkNextItemIndex # next #== 4

anotherTransaction, backwardsStatement, anotherField, malformedItemStatement,
  conservationAsItem :: forall s. Term s PBool
anotherTransaction = pmatch (popenInline txIdA 6 fraudulentItems) $ \(PPair view start) ->
  pforceProof $ pproveItemFault # view # start # pitemStatementAgainst txIdB 6 4 # pitemIsHonest
backwardsStatement = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  plet (pwalkSkip # view # start # 6) $ \advanced ->
    pforceProof $ pproveItemFault # view # advanced # pitemStatement 6 4 # pitemIsHonest
anotherField = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  pforceProof $ pproveItemFault # view # start # pitemStatement 8 4 # pitemIsHonest
malformedItemStatement = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  pforceProof $ pproveItemFault # view # start
    # pstatement sampleTxId pfaultItemPredicate 6 4 targetPolicy "" 0 # pitemIsHonest
conservationAsItem = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  pforceProof $ pproveItemFault # view # start # pconservationStatement 0 # pitemIsHonest

outputReadsAsset :: forall s. Term s PBool
outputReadsAsset = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 3) $ \swept ->
    poutputSweepIsFinal # swept
      #&& poutputSweepQuantity # swept #== 18
      #&& pwalkNextItemIndex # (poutputSweepCheckpoint # swept) #== 3

outputReportsAbsence :: forall s. Term s PBool
outputReportsAbsence = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant targetPolicy # pconstant (assetName 9)) # 3) $ \swept ->
    poutputSweepIsFinal # swept #&& poutputSweepQuantity # swept #== 0

outputResumes :: forall s. Term s PBool
outputResumes = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 1) $ \middle ->
  plet (paccumulateOutputUnit # view # middle # 2) $ \finished ->
    pnot # (poutputSweepIsFinal # middle)
      #&& poutputSweepIsFinal # finished
      #&& poutputSweepQuantity # finished #== 18
      #&& pwalkNextItemIndex # (poutputSweepCheckpoint # middle) #== 1
      #&& pwalkNextItemIndex # (poutputSweepCheckpoint # finished) #== 3

unfinishedOutputQuantity, outputAnotherField, outputOpenedPastStart :: forall s. Term s PBool
unfinishedOutputQuantity = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 1) $ \middle ->
    poutputSweepQuantity # middle #== 7
outputAnotherField = pmatch (popenInline sampleTxId 6 fraudulentItems) $ \(PPair view start) ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 1) $ \swept ->
    poutputSweepQuantity # swept #== 0
outputOpenedPastStart = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  plet (pwalkSkip # view # start # 1) $ \pastFirst ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # pastFirst # pconstant targetPolicy # pconstant targetName) # 3) $ \swept ->
    poutputSweepIsFinal # swept #&& poutputSweepQuantity # swept #== 11

mintReadsAsset, mintReadsBurn, mintReportsAbsence, unorderedMint, zeroMintQuantity,
  mintResumes, mintOpenedPastTarget, mintBudgetExhausted, unorderedMintAcrossRounds,
  mintAnotherField :: forall s. Term s PBool
mintReadsAsset = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair view start) ->
  pmatch (pmintUnitQuantity # view # start # pconstant targetPolicy # pconstant targetName # 4) $ \(PPair quantity _) ->
    quantity #== 4
mintReadsBurn = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair view start) ->
  pmatch (pmintUnitQuantity # view # start # pconstant (policyId 4) # pconstant (assetName 1) # 4) $ \(PPair quantity _) ->
    quantity #== (-8)
mintReportsAbsence = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair viewA startA) ->
  pmatch (pmintUnitQuantity # viewA # startA # pconstant (policyId 2) # pconstant (assetName 0) # 4) $ \(PPair missingPolicy _) ->
  pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair viewB startB) ->
  pmatch (pmintUnitQuantity # viewB # startB # pconstant targetPolicy # pconstant (assetName 9) # 4) $ \(PPair missingName _) ->
    missingPolicy #== 0 #&& missingName #== 0
unorderedMint = pmatch (popenInline sampleTxId 5 unorderedMintPreimage) $ \(PPair view start) ->
  pmatch (pmintUnitQuantity # view # start # pconstant (policyId 6) # pconstant (assetName 3) # 4) $ \(PPair quantity _) ->
    quantity #== 30
zeroMintQuantity = pmatch (popenInline sampleTxId 5 zeroQuantityMintPreimage) $ \(PPair view start) ->
  pmatch (pmintUnitQuantity # view # start # pconstant targetPolicy # pconstant targetName # 4) $ \(PPair quantity _) ->
    quantity #== 0
mintResumes = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair view start) ->
  plet (psweepMintUnit # view # (popenMintUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 1) $ \middle ->
  plet (psweepMintUnit # view # middle # 1) $ \finished ->
    pnot # (pmintSweepIsFinal # middle)
      #&& pwalkNextItemIndex # (pmintSweepCheckpoint # middle) #== 1
      #&& pmintSweepIsFinal # finished
      #&& pmintSweepQuantity # finished #== 4
      #&& pwalkNextItemIndex # (pmintSweepCheckpoint # finished) #== 2
mintOpenedPastTarget = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair view start) ->
  plet (pwalkSkip # view # start # 2) $ \pastTarget ->
  plet (psweepMintUnit # view # (popenMintUnitSweep # pastTarget # pconstant targetPolicy # pconstant targetName) # 4) $ \swept ->
    pmintSweepIsFinal # swept #&& pmintSweepQuantity # swept #== 0
mintBudgetExhausted = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair view start) ->
  pmatch (pmintUnitQuantity # view # start # pconstant targetPolicy # pconstant targetName # 1) $ \(PPair quantity _) ->
    quantity #== 0
unorderedMintAcrossRounds = pmatch (popenInline sampleTxId 5 unorderedMintPreimage) $ \(PPair view start) ->
  plet (psweepMintUnit # view # (popenMintUnitSweep # start # pconstant (policyId 6) # pconstant (assetName 3)) # 2) $ \middle ->
  plet (psweepMintUnit # view # middle # 2) $ \finished -> pmintSweepQuantity # finished #== 30
mintAnotherField = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  pmatch (pmintUnitQuantity # view # start # pconstant targetPolicy # pconstant targetName # 3) $ \(PPair quantity _) ->
    quantity #== 0

conservationDisagreement :: forall s. Term s PBool
conservationDisagreement =
  plet (pfinishedOutput sampleTxId targetPolicy targetName) $ \outputs ->
  plet (pfinishedMint sampleTxId targetPolicy targetName) $ \mint ->
    poutputSweepQuantity # outputs #== 18
      #&& pmintSweepQuantity # mint #== 4
      #&& pnot # (passetConservationFaultIsProven # pconservationStatement 14 # outputs # mint)
      #&& passetConservationFaultIsProven # pconservationStatement 13 # outputs # mint
      #&& passetConservationFaultIsProven # pconservationStatement 18 # outputs # mint

conservationOtherTransaction, conservationOtherUnit, conservationUnfinishedMint,
  conservationUnfinishedOutput, conservationStatementOtherField,
  conservationStatementStartingItem, itemAsConservation,
  malformedConservationStatement :: forall s. Term s PBool
conservationOtherTransaction =
  passetConservationFaultIsProven
    # pconservationStatement 13
    # pfinishedOutput txIdA targetPolicy targetName
    # pfinishedMint txIdA targetPolicy targetName
conservationOtherUnit =
  passetConservationFaultIsProven
    # pconservationStatement 14
    # pfinishedOutput sampleTxId targetPolicy targetName
    # pfinishedMint sampleTxId (policyId 4) (assetName 1)
conservationUnfinishedMint = pmatch (popenInline sampleTxId 5 mintPreimage) $ \(PPair view start) ->
  plet (psweepMintUnit # view # (popenMintUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 1) $ \unfinished ->
    passetConservationFaultIsProven
      # pconservationStatement 14
      # pfinishedOutput sampleTxId targetPolicy targetName
      # unfinished
conservationUnfinishedOutput = pmatch (popenInline sampleTxId 2 outputsPreimage) $ \(PPair view start) ->
  plet (paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant targetPolicy # pconstant targetName) # 1) $ \unfinished ->
    passetConservationFaultIsProven
      # pconservationStatement 14
      # unfinished
      # pfinishedMint sampleTxId targetPolicy targetName
conservationStatementOtherField =
  passetConservationFaultIsProven
    # pstatement sampleTxId pfaultAssetConservation 5 0 targetPolicy targetName 13
    # pfinishedOutput sampleTxId targetPolicy targetName
    # pfinishedMint sampleTxId targetPolicy targetName
conservationStatementStartingItem =
  passetConservationFaultIsProven
    # pstatement sampleTxId pfaultAssetConservation 2 1 targetPolicy targetName 13
    # pfinishedOutput sampleTxId targetPolicy targetName
    # pfinishedMint sampleTxId targetPolicy targetName
itemAsConservation =
  passetConservationFaultIsProven
    # pitemStatement 2 0
    # pfinishedOutput sampleTxId targetPolicy targetName
    # pfinishedMint sampleTxId targetPolicy targetName
malformedConservationStatement =
  passetConservationFaultIsProven
    # pstatement sampleTxId pfaultAssetConservation 2 0 targetPolicy
      (targetName <> BS.replicate 31 0) 13
    # pfinishedOutput sampleTxId targetPolicy targetName
    # pfinishedMint sampleTxId targetPolicy targetName

pfinishedOutput :: forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> Term s POutputUnitSweepV1
pfinishedOutput txId policy name = pmatch (popenInline txId 2 outputsPreimage) $ \(PPair view start) ->
  paccumulateOutputUnit # view # (popenOutputUnitSweep # start # pconstant policy # pconstant name) # 3

pfinishedMint :: forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> Term s PMintUnitSweepV1
pfinishedMint txId policy name = pmatch (popenInline txId 5 mintPreimage) $ \(PPair view start) ->
  psweepMintUnit # view # (popenMintUnitSweep # start # pconstant policy # pconstant name) # 4

tier2FixturesBound, tier2FixedOneOpen, tier2FixedTwoOpens,
  tier2VariableOneOpen, tier2VariableTwoOpens :: forall s. Term s PBool
tier2FixturesBound =
  (pconstant (fromIntegral (BS.length tier2FixedPreimage) :: Integer) :: Term s PInteger) #== 15883
    #&& (pconstant (fromIntegral (BS.length tier2VariablePreimage) :: Integer) :: Term s PInteger) #== 15865
    #&& 15883 #<= pchunkBytesK
    #&& 15865 #<= pchunkBytesK
tier2FixedOneOpen = pmatch (popenRaw sampleTxId 1 tier2FixedPreimage) $ \(PPair view _) ->
  pfieldTotalLength # view #== 15883
tier2FixedTwoOpens =
  pmatch (popenRaw sampleTxId 1 tier2FixedPreimage) $ \(PPair first _) ->
  pmatch (popenRaw sampleTxId 1 tier2FixedPreimage) $ \(PPair second _) ->
    pfieldTotalLength # first #== 15883 #&& pfieldTotalLength # second #== 15883
tier2VariableOneOpen = pmatch (popenRaw sampleTxId 6 tier2VariablePreimage) $ \(PPair view _) ->
  pfieldTotalLength # view #== pconstant (fromIntegral (BS.length tier2VariablePreimage) :: Integer)
tier2VariableTwoOpens =
  pmatch (popenRaw sampleTxId 6 tier2VariablePreimage) $ \(PPair first _) ->
  pmatch (popenRaw sampleTxId 6 tier2VariablePreimage) $ \(PPair second _) ->
    pfieldTotalLength # first #== pconstant (fromIntegral (BS.length tier2VariablePreimage) :: Integer)
      #&& pfieldTotalLength # second #== pconstant (fromIntegral (BS.length tier2VariablePreimage) :: Integer)

pforceProof :: forall s. Term s (PPair PBool PFieldWalkCheckpointV1) -> Term s PBool
pforceProof proof = pmatch proof $ \(PPair proven _) -> proven #|| pnot # proven

pitemIsHonest, potherItemIsHonest :: forall s. Term s (PInteger :--> PByteString :--> PBool)
pitemIsHonest = plam $ \index item ->
  pif (index #== 4) (item #== pconstant (scriptItem 4)) (pconstant True)
potherItemIsHonest = plam $ \index item ->
  pif (index #== 4) (item #== pconstant (otherScriptItem 4)) (pconstant True)

pforceEncode :: forall s. Term s PFaultStatementV1 -> Term s PBool
pforceEncode statement = plet (pencodeFaultStatement # statement) $ \bytes -> plengthBS # bytes #>= 0

pitemStatement :: forall s. Integer -> Integer -> Term s PFaultStatementV1
pitemStatement fieldIndex itemIndex =
  pitemStatementAgainst sampleTxId fieldIndex itemIndex

pitemStatementAgainst :: forall s. BS.ByteString -> Integer -> Integer -> Term s PFaultStatementV1
pitemStatementAgainst txId fieldIndex itemIndex =
  pstatement txId pfaultItemPredicate fieldIndex itemIndex "" "" 0

pconservationStatement :: forall s. Integer -> Term s PFaultStatementV1
pconservationStatement claimed =
  pstatement sampleTxId pfaultAssetConservation 2 0 targetPolicy targetName claimed

pstatement :: forall s.
  BS.ByteString -> Term s PInteger -> Integer -> Integer -> BS.ByteString -> BS.ByteString ->
  Integer -> Term s PFaultStatementV1
pstatement txId code fieldIndex itemIndex policyId assetName claimed =
  pcon $ PFaultStatementV1
    { pfault'txId = pdata $ pconstant txId
    , pfault'code = pdata code
    , pfault'fieldIndex = pdata $ pconstant fieldIndex
    , pfault'itemIndex = pdata $ pconstant itemIndex
    , pfault'policyId = pdata $ pconstant policyId
    , pfault'assetName = pdata $ pconstant assetName
    , pfault'claimed = pdata $ pconstant claimed
    }

sampleTxId, targetPolicy, targetName, statementDomain :: BS.ByteString
sampleTxId = BS.replicate 32 0x11
targetPolicy = BS.singleton 3 <> BS.replicate 27 0xb0
targetName = BS.pack [0xc0, 0xc0, 0xc0, 1]
statementDomain = "MidgardNativeTxFaultStatementV1"

fraudulentItems, otherFraudulentItems :: BS.ByteString
fraudulentItems = fieldPreimage
  [if index == 4 then otherScriptItem index else scriptItem index | index <- [0 .. 7]]
otherFraudulentItems = fieldPreimage
  [if index == 4 then scriptItem index else otherScriptItem index | index <- [0 .. 7]]

scriptItem, otherScriptItem :: Int -> BS.ByteString
scriptItem index = BS.singleton 3 <> BS.replicate (4 + index `mod` 3) (fromIntegral $ 0x40 + index)
otherScriptItem index = BS.singleton 0 <> BS.replicate (4 + index `mod` 3) (fromIntegral $ 0x70 + index)

fieldPreimage :: [BS.ByteString] -> BS.ByteString
fieldPreimage items = arrayHeader (length items) <> BS.concat (map wrapItem items)

arrayHeader :: Int -> BS.ByteString
arrayHeader count
  | count <= 23 = BS.singleton (0x80 + fromIntegral count)
  | count <= 0xff = BS.pack [0x98, fromIntegral count]
  | otherwise = BS.pack [0x99, fromIntegral (count `div` 0x100), fromIntegral count]

wrapItem :: BS.ByteString -> BS.ByteString
wrapItem = bytesItem

type AssetGroup = (BS.ByteString, [(BS.ByteString, Integer)])

outputsPreimage :: BS.ByteString
outputsPreimage = fieldPreimage
  [ outputItem 5_000_000
      [ (policyId 1, [(assetName 0, 100)])
      , (policyId 3, [(assetName 0, 5), (assetName 1, 7)])
      , (policyId 5, [(assetName 2, 9)])
      ]
  , outputItem 2_000_000 [(policyId 0, [(assetName 1, 55)])]
  , outputItem 3_000_000
      [ (policyId 3, [(assetName 1, 11), (assetName 4, 13)])
      , (policyId 7, [(assetName 0, 17)])
      ]
  ]

outputItem :: Integer -> [AssetGroup] -> BS.ByteString
outputItem lovelace groups =
  BS.pack [0xa2, 0x00] <> bytesItem addressPayload <> BS.pack [0x01, 0x82]
    <> uintItem lovelace <> mapHead (length groups) <> BS.concat (map encodeGroup groups)

encodeGroup :: AssetGroup -> BS.ByteString
encodeGroup (policy, assets) =
  bytesItem policy <> mapHead (length assets)
    <> BS.concat [bytesItem name <> uintItem quantity | (name, quantity) <- assets]

bytesItem :: BS.ByteString -> BS.ByteString
bytesItem bytes
  | BS.length bytes <= 23 = BS.singleton (0x40 + fromIntegral (BS.length bytes)) <> bytes
  | otherwise = BS.pack [0x58, fromIntegral (BS.length bytes)] <> bytes

mapHead :: Int -> BS.ByteString
mapHead count = BS.singleton (0xa0 + fromIntegral count)

uintItem :: Integer -> BS.ByteString
uintItem value
  | value <= 23 = BS.singleton (fromIntegral value)
  | value <= 0xff = BS.pack [0x18, fromIntegral value]
  | value <= 0xffff = BS.pack [0x19, fromIntegral (value `div` 0x100), fromIntegral value]
  | otherwise = BS.pack
      [ 0x1a
      , fromIntegral (value `div` 0x1000000)
      , fromIntegral (value `div` 0x10000)
      , fromIntegral (value `div` 0x100)
      , fromIntegral value
      ]

policyId, assetName :: Int -> BS.ByteString
policyId index = BS.singleton (fromIntegral index) <> BS.replicate 27 0xb0
assetName index = BS.replicate 3 0xc0 <> BS.singleton (fromIntegral index)

addressPayload :: BS.ByteString
addressPayload = BS.singleton 0x60 <> BS.replicate 28 0xaa

mintPreimage, unorderedMintPreimage, zeroQuantityMintPreimage :: BS.ByteString
mintPreimage = fieldPreimage
  [ mintItem (policyId 0) [(assetName 0, 21)]
  , mintItem (policyId 3) [(assetName 0, 2), (assetName 1, 4), (assetName 6, 6)]
  , mintItem (policyId 4) [(assetName 1, -8)]
  , mintItem (policyId 6) [(assetName 3, 30)]
  ]
unorderedMintPreimage = fieldPreimage
  [ mintItem (policyId 0) [(assetName 0, 21)]
  , mintItem (policyId 4) [(assetName 1, -8)]
  , mintItem (policyId 3) [(assetName 0, 2), (assetName 1, 4), (assetName 6, 6)]
  , mintItem (policyId 6) [(assetName 3, 30)]
  ]
zeroQuantityMintPreimage = fieldPreimage [mintItem (policyId 3) [(assetName 1, 0)]]

mintItem :: BS.ByteString -> [(BS.ByteString, Integer)] -> BS.ByteString
mintItem policy assets = BS.singleton 0x82 <> bytesItem policy <> mapHead (length assets)
  <> BS.concat [bytesItem name <> intItem quantity | (name, quantity) <- assets]

intItem :: Integer -> BS.ByteString
intItem value
  | value >= 0 = uintItem value
  | otherwise =
      let magnitude = -1 - value
       in if magnitude <= 23
            then BS.singleton (0x20 + fromIntegral magnitude)
            else BS.pack [0x38, fromIntegral magnitude]

popenInline :: forall s.
  BS.ByteString -> Integer -> BS.ByteString ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
popenInline txId fieldIndex preimage =
  popenFieldWalk
    # pverified txId fieldIndex preimage
    # pwitnessSet fieldIndex preimage
    # pconstant fieldIndex
    # pcon (PInline $ pdata $ pconstant preimage)
    # pnil
    # pdata (pconstant certificatePolicy)

pverified :: forall s. BS.ByteString -> Integer -> BS.ByteString -> Term s PVerifiedMidgardNativeTxCompact
pverified txId fieldIndex preimage = pcon $ PVerifiedMidgardNativeTxCompact
  { pverified'txId = pconstant txId
  , pverified'version = 1
  , pverified'txCompact = pcon $ PNativeTxCompact
      { pcompact'body = pbody fieldIndex preimage
      , pcompact'witnessSetHash = pconstant $ witnessSetHash fieldIndex preimage
      , pcompact'validityCode = 0
      }
  }

pbody :: forall s. Integer -> BS.ByteString -> Term s PNativeTxBodyCompact
pbody fieldIndex preimage = pcon $ PNativeTxBodyCompact
  { pbodyCompact'spendInputsHash = slot 0
  , pbodyCompact'referenceInputsHash = slot 1
  , pbodyCompact'outputsHash = slot 2
  , pbodyCompact'fee = 0
  , pbodyCompact'validityIntervalStart = -1
  , pbodyCompact'validityIntervalEnd = -1
  , pbodyCompact'requiredObserversHash = slot 3
  , pbodyCompact'requiredSignersHash = slot 4
  , pbodyCompact'mintHash = slot 5
  , pbodyCompact'scriptIntegrityHash = pconstant zeroHash
  , pbodyCompact'auxiliaryDataHash = pconstant zeroHash
  , pbodyCompact'networkId = 255
  }
  where
    slot index = pconstant $ if fieldIndex == index then blake2b256 preimage else zeroHash

pwitnessSet :: forall s. Integer -> BS.ByteString -> Term s PNativeTxWitnessSetCompact
pwitnessSet fieldIndex preimage = pcon $ PNativeTxWitnessSetCompact
  { pwitnessSetCompact'addrTxWitsHash = pdata $ pconstant $ slot 7
  , pwitnessSetCompact'scriptTxWitsHash = pdata $ pconstant $ slot 6
  , pwitnessSetCompact'redeemerTxWitsHash = pdata $ pconstant $ slot 8
  }
  where
    slot index = if fieldIndex == index then blake2b256 preimage else zeroHash

witnessSetHash :: Integer -> BS.ByteString -> BS.ByteString
witnessSetHash fieldIndex preimage = blake2b256 $ BS.concat
  ["\x83\x58\x20", slot 7, "\x58\x20", slot 6, "\x58\x20", slot 8]
  where
    slot index = if fieldIndex == index then blake2b256 preimage else zeroHash

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

txIdA, txIdB, zeroHash :: BS.ByteString
txIdA = BS.replicate 32 0x44
txIdB = BS.replicate 32 0x55
zeroHash = BS.replicate 32 0

certificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x22

tier2FixedPreimage, tier2VariablePreimage :: BS.ByteString
tier2FixedPreimage = fieldPreimage [tier2InputItem index | index <- [0 .. 396]]
tier2VariablePreimage = fieldPreimage [tier2ScriptItem index | index <- [0 .. 1441]]

tier2InputItem :: Int -> BS.ByteString
tier2InputItem index = BS.pack [0x82, 0x58, 0x20] <> txIdA
  <> BS.pack [0x19, fromIntegral (index `div` 0x100), fromIntegral index]

tier2ScriptItem :: Int -> BS.ByteString
tier2ScriptItem index =
  let len = 4 + index `mod` 7
   in BS.pack [0x82, 0x03, 0x40 + fromIntegral len] <> BS.take len txIdA

popenRaw :: forall s.
  BS.ByteString -> Integer -> BS.ByteString ->
  Term s (PPair PFieldViewV1 PFieldWalkCheckpointV1)
popenRaw txId fieldIndex preimage =
  popenFieldWalk
    # pverified txId fieldIndex preimage
    # pwitnessSet fieldIndex preimage
    # pconstant fieldIndex
    # pcon (PRawUtxo $ pdata 0)
    # pconstant [rawBytesInput preimage]
    # pdata (pconstant certificatePolicy)

rawBytesInput :: BS.ByteString -> TxInInfo
rawBytesInput bytes = TxInInfo
  (TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x99) 0)
  (TxOut
    (scriptHashAddress $ ScriptHash $ toBuiltin $ BS.replicate 28 0x98)
    (adaValue 2_000_000)
    (OutputDatum $ Datum $ dataToBuiltinData $ PD.B bytes)
    Nothing)

adaValue :: Integer -> Value
adaValue = singleton (CurrencySymbol "") (TokenName "")
