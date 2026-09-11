{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.AvailabilityChallengeValidator
Description : Transaction tests for @validators/availability-challenge.ak@.
-}
module Testing.AvailabilityChallengeValidator (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Address,
  POSIXTime (..),
  PubKeyHash (..),
  ScriptContext,
  ScriptHash (..),
  TxId (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusTx.Builtins (
  blake2b_224,
  blake2b_256,
  builtinDataToData,
  dataToBuiltinData,
  fromBuiltin,
  serialiseData,
  toBuiltin,
 )
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.LedgerApi.V3 qualified as PV3
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.AvailabilityChallenge
import Midgard.Validators.AvailabilityChallenge (
  availabilityChallengeSpendValidator,
  availabilityChallengeValidator,
  pvalidateInitialTerminalAccumulatorOutputV1,
  pvalidateInitialTrancheOutputsV1,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (
  ScriptContextBuilder,
  addInput,
  buildScriptContext,
  mkInput,
  mkAdaValue,
  withAddress,
  withFee,
  withInlineDatum,
  withInput,
  withMint,
  withMintingScript,
  withOutRef,
  withOutput,
  withReferenceInput,
  withRedeemer,
  withScriptInput,
  withSigner,
  withSpendingScript,
  withTxOutAddress,
  withTxOutInlineDatum,
  withTxOutValue,
  withValidRange,
  withValue,
 )

tests :: TestTree
tests =
  testGroup
    "Availability Challenge Validator"
    [ testGroup
        "AdvanceTranche"
        [ testCase "accepts an exact committed publication" $
            psucceeds $ runSpend validPublicationContext
        , testCase "rejects a zero publication fee" $
            pfails $ runSpend $ publicationContext {publicationFee = 0}
        , testCase "rejects a publication fee above the authenticated cap" $
            pfails $ runSpend $ publicationContext {publicationFee = 500_001}
        , testCase "rejects publication after the response deadline" $
            pfails $ runSpend $ publicationContext {publicationUpperBound = responseDeadline + 1}
        , testCase "rejects aliased thread and carrier outputs" $
            pfails $ runSpend $ publicationContext {publicationCarrierOutputIndex = 0}
        , testCase "rejects broken lovelace conservation" $
            pfails $ runSpend $ publicationContext {publicationCarrierLovelace = fromIntegral carrierLovelace + 1}
        , testCase "rejects a substituted next thread datum" $
            pfails $ runSpend $ publicationContext {publicationThreadDatum = activeDatum}
        , testCase "rejects unexpected minting" $
            pfails $ runSpend $ publicationContext {publicationUnexpectedMint = True}
        ]
    , testGroup
        "ConsumeCarrier"
        [ testCase "binds a carrier to the thread redeemer that names it" $
            psucceeds $ runContext validConsumeContext
        , testCase "rejects a thread redeemer that names another carrier" $
            pfails $ runContext $ consumeScriptContext 1
        , testCase "rejects a thread redeemer with no previous carrier" $
            pfails $ runContext consumeWithoutPreviousContext
        ]
    , testGroup
        "Coordinate"
        [ testCase "binds a bond input to OpenChallenge" $
            psucceeds $ runContext $ coordinateContext openChallengeRedeemer
        , testCase "rejects an undeclared availability input" $
            pfails $ runContext $ coordinateContext $ openChallengeRedeemerAt 1
        , testCase "rejects MintBondFromAttestation as a spend coordinator" $
            pfails $ runContext $ coordinateContext mintBondRedeemer
        ]
    , testGroup
        "MintBondFromAttestation"
        [ testCase "accepts the exact attestation and queue transition" $
            psucceeds $ runMintBond validMintBond
        , testCase "accepts a wallet fee input and change output" $
            psucceeds $ runMintBond validMintBond {mintBondWithWalletChange = True}
        , testCase "rejects a substituted deployment identity" $
            pfails $ runMintBond validMintBond {mintBondDeploymentIdentity = BS.replicate 28 0x99}
        , testCase "rejects aliased bond and state-queue outputs" $
            pfails $
              runMintBond $
                validMintBond
                  { mintBondBondOutputIndex = 0
                  , mintBondStateQueueOutputIndex = 0
                  }
        ]
    , testGroup
        "CloseChallenge"
        [ testCase "accepts exact distinct terminal outputs" $
            psucceeds $ runCloseChallenge validCloseChallenge
        , testCase "rejects aliased DA and challenger refunds" $
            pfails $
              runCloseChallenge $
                validCloseChallenge
                  { closeDaRefundOutputIndex = 1
                  , closeChallengerRefundOutputIndex = 1
                  }
        ]
    , testGroup
        "TimeoutChallenge"
        [ testCase "accepts exact challenger slash and unused bond refund" $
            psucceeds $ runTimeoutChallenge validTimeoutChallenge
        , testCase "rejects aliased challenger outputs" $
            pfails $
              runTimeoutChallenge $
                validTimeoutChallenge
                  { timeoutDaSlashOutputIndex = 0
                  , timeoutChallengerRefundOutputIndex = 0
                  }
        , testCase "rejects an excessive fee" $
            pfails $ runTimeoutChallenge validTimeoutChallenge {timeoutFee = 1_200_001}
        , testCase "rejects premature validity" $
            pfails $ runTimeoutChallenge validTimeoutChallenge {timeoutLowerBound = responseDeadline - 1}
        , testCase "rejects the wrong state-queue challenge identity" $
            pfails $
              runTimeoutChallenge $
                validTimeoutChallenge {timeoutStateQueueChallengeName = BS.replicate 32 0x99}
        , testCase "rejects aliased bond and terminal input indices" $
            pfails $ runTimeoutChallenge validTimeoutChallenge {timeoutTerminalInputIndex = 0}
        , testCase "rejects a third challenger output" $
            pfails $ runTimeoutChallenge validTimeoutChallenge {timeoutExtraChallengerOutput = True}
        ]
    , testGroup
        "SettleTranche"
        [ testCase "fixture commitment is canonical" $
            psucceeds fixtureCommitmentIsCanonical
        , testCase "fixture terminal hashes match the Plutarch implementation" $
            psucceeds fixtureTerminalHashesMatch
        , testCase "settles an exact published tranche with its carrier" $
            psucceeds $ runSettlement publishedSettlement
        , testCase "settles an exact timed-out active tranche without a carrier" $
            psucceeds $ runSettlement timedOutSettlement
        , testCase "rejects a substituted publication carrier out-ref" $
            pfails $ runSettlement publishedSettlement {settlementSubstituteCarrier = True}
        , testCase "rejects a premature timeout" $
            pfails $ runSettlement timedOutSettlement {settlementLowerBound = responseDeadline - 1}
        , testCase "rejects a settlement fee above the authenticated cap" $
            pfails $ runSettlement publishedSettlement {settlementFee = 500_001}
        , testCase "rejects aliased terminal and tranche inputs" $
            pfails $ runSettlement publishedSettlement {settlementTrancheInputIndex = 0}
        , testCase "rejects an extra input that could charge a second settlement fee" $
            pfails $ runSettlement publishedSettlement {settlementExtraInput = True}
        ]
    , testGroup
        "OpenChallenge outputs"
        [ testCase "accepts the exact initial tranche sequence" $
            psucceeds $ runInitialTrancheOutput activeDatum
        , testCase "rejects a substituted initial tranche datum" $
            pfails $ runInitialTrancheOutput receiptDatum
        , testCase "accepts the exact initial terminal accumulator" $
            psucceeds $ runInitialTerminalOutput initialTerminalDatum
        , testCase "rejects a substituted initial terminal datum" $
            pfails $ runInitialTerminalOutput terminalDatum
        ]
    , testGroup
        "OpenChallenge"
        [ testCase "accepts exact distinct inputs, outputs, and challenger signer" $
            psucceeds $ runOpenChallenge validOpenChallenge
        , testCase "rejects a missing challenger signature" $
            pfails $ runOpenChallenge validOpenChallenge {openSigner = bondOwnerBytes}
        , testCase "rejects aliased bond and state-queue outputs" $
            pfails $
              runOpenChallenge $
                validOpenChallenge
                  { openBondOutputIndex = 0
                  , openStateQueueOutputIndex = 0
                  }
        ]
    ]

fixtureCommitmentIsCanonical :: forall s. Term s PUnit
fixtureCommitmentIsCanonical =
  pif
    (pcommitmentIsCanonicalV1 decodedCommitment canonicalParameters)
    (pconstant ())
    perror

fixtureTerminalHashesMatch :: forall s. Term s PUnit
fixtureTerminalHashesMatch =
  let challengeName = pconstant challengeAssetName
      start = pterminalAccumulatorStartV1 decodedCommitment challengeName
      published = pcon $ PPublishedTranche $ pdata $ pconstant nextAccumulator
      timedOut = pcon $ PTimedOutTranche (pdata 0) (pdata $ pconstant startAccumulator)
   in pif
        ( start #== pconstant terminalStartAccumulator
            #&& pfoldTerminalAccumulatorV1 start 0 published #== pconstant publishedTerminalFold
            #&& pfoldTerminalAccumulatorV1 start 0 timedOut #== pconstant timedOutTerminalFold
        )
        (pconstant ())
        perror

decodedCommitment :: forall s. Term s PCommitmentV1
decodedCommitment =
  pfromData (punsafeCoerce (pconstant @PData commitmentData) :: Term s (PAsData PCommitmentV1))

runInitialTrancheOutput :: PD.Data -> forall s. Term s PUnit
runInitialTrancheOutput candidateDatum =
  pmatch (pconstant $ initialOutputsContext candidateDatum initialTerminalDatum) $ \PV3.PScriptContext {pscriptContext'txInfo} ->
    pmatch pscriptContext'txInfo $ \PV3.PTxInfo {ptxInfo'outputs} ->
      pif
        ( pvalidateInitialTrancheOutputsV1
            (pfromData ptxInfo'outputs)
            (pconstant availabilityPolicy)
            decodedCommitment
            (pconstant challengeAssetName)
            (pdata $ pconstant $ PubKeyHash $ toBuiltin challengerBytes)
            (pconstant responseDeadline)
            0
            0
            (pcons # pdata decodedDescriptor # pnil)
            (pcons # pconstant initialTrancheLovelace # pnil)
        )
        (pconstant ())
        perror

runInitialTerminalOutput :: PD.Data -> forall s. Term s PUnit
runInitialTerminalOutput candidateDatum =
  pmatch (pconstant $ initialOutputsContext activeDatum candidateDatum) $ \PV3.PScriptContext {pscriptContext'txInfo} ->
    pmatch pscriptContext'txInfo $ \PV3.PTxInfo {ptxInfo'outputs} ->
      pif
        ( pvalidateInitialTerminalAccumulatorOutputV1
            (pfromData $ pelemAt # 1 # pfromData ptxInfo'outputs)
            (pconstant availabilityPolicy)
            decodedCommitment
            (pconstant challengeAssetName)
            (pdata $ pconstant $ PubKeyHash $ toBuiltin challengerBytes)
            (pconstant responseDeadline)
            (pconstant terminalInitialLovelace)
        )
        (pconstant ())
        perror

decodedDescriptor :: forall s. Term s PTrancheDescriptorV1
decodedDescriptor =
  pfromData (punsafeCoerce (pconstant @PData descriptorData) :: Term s (PAsData PTrancheDescriptorV1))

initialOutputsContext :: PD.Data -> PD.Data -> ScriptContext
initialOutputsContext trancheDatum terminalAccumulatorDatum =
  buildScriptContext $
    withOutput
      ( withTxOutAddress scriptAddress
          <> withTxOutValue
            ( mkAdaValue (fromInteger initialTrancheLovelace)
                <> singleton availabilityPolicy trancheAssetName 1
            )
          <> withTxOutInlineDatum (dataToBuiltinData trancheDatum)
      )
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue
              ( mkAdaValue (fromInteger terminalInitialLovelace)
                  <> singleton availabilityPolicy terminalAssetName 1
              )
            <> withTxOutInlineDatum (dataToBuiltinData terminalAccumulatorDatum)
        )

data MintBondContext = MintBondContext
  { mintBondDeploymentIdentity :: BS.ByteString
  , mintBondBondOutputIndex :: Integer
  , mintBondStateQueueOutputIndex :: Integer
  , mintBondWithWalletChange :: Bool
  }

validMintBond :: MintBondContext
validMintBond =
  MintBondContext
    { mintBondDeploymentIdentity = deploymentIdentity
    , mintBondBondOutputIndex = 0
    , mintBondStateQueueOutputIndex = 1
    , mintBondWithWalletChange = False
    }

runMintBond :: MintBondContext -> forall s. Term s PUnit
runMintBond fixture =
  availabilityChallengeValidator
    # pdata (pconstant hubOraclePolicy)
    # pdata canonicalParameters
    # pconstant (mintBondContext fixture)

mintBondContext :: MintBondContext -> ScriptContext
mintBondContext fixture =
  addInput attestationInput $
    addInput stateQueueInput $
      buildScriptContext $
        withMintingScript availabilityMint (dataToBuiltinData availabilityRedeemer)
          <> withMint daBurn (dataToBuiltinData daRedeemer)
          <> withRedeemer (dataToBuiltinData availabilityRedeemer)
          <> withReferenceInput
            ( withOutRef hubRef
                <> withAddress hubAddress
                <> withValue
                  (mkAdaValue 2_000_000 <> singleton hubOraclePolicy (TokenName "MIDGARD_HUB_ORACLE") 1)
                <> withInlineDatum (dataToBuiltinData hubDatumData)
            )
          <> withOutput
            ( withTxOutAddress scriptAddress
                <> withTxOutValue
                  (mkAdaValue daBondLovelace <> singleton availabilityPolicy mintBondAssetName 1)
                <> withTxOutInlineDatum (dataToBuiltinData $ mintBondDatum fixture)
            )
          <> withOutput
            ( withTxOutAddress stateQueueAddress
                <> withTxOutValue stateQueueValue
                <> withTxOutInlineDatum
                  (dataToBuiltinData $ stateQueueDatum $ PD.Constr 1 [tokenNameData mintBondAssetName])
            )
          <> walletChange fixture
          <> withFee (if mintBondWithWalletChange fixture then 100 else 0)
          <> withValidRange (interval (POSIXTime 0) $ POSIXTime 1_000)
  where
    availabilityRedeemer =
      PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 1
        , PD.I $ mintBondBondOutputIndex fixture
        , PD.I 1
        , PD.I $ mintBondStateQueueOutputIndex fixture
        ]
    daRedeemer =
      PD.Constr
        1
        [ PD.I 0
        , PD.I 0
        , PD.I 1
        , PD.I 1
        , PD.I 0
        , PD.I 1
        ]
    availabilityMint = singleton availabilityPolicy mintBondAssetName 1
    daBurn = singleton daAttestationPolicy daAttestationAssetName (-1)
    attestationInput =
      mkInput
        ( withOutRef mintBondAttestationRef
            <> withAddress daAttestationAddress
            <> withValue
              (mkAdaValue daBondLovelace <> singleton daAttestationPolicy daAttestationAssetName 1)
            <> withInlineDatum (dataToBuiltinData $ mintBondAttestationDatum fixture)
        )
    stateQueueInput =
      mkInput
        ( withOutRef mintBondStateQueueRef
            <> withAddress stateQueueAddress
            <> withValue stateQueueValue
            <> withInlineDatum (dataToBuiltinData $ stateQueueDatum $ PD.Constr 0 [])
        )

walletChange :: MintBondContext -> ScriptContextBuilder
walletChange fixture
  | not (mintBondWithWalletChange fixture) = mempty
  | otherwise =
      withInput
        ( withOutRef mintBondWalletRef
            <> withAddress challengerAddress
            <> withValue (mkAdaValue 2_000_100)
        )
        <> withOutput
          ( withTxOutAddress challengerAddress
              <> withTxOutValue (mkAdaValue 2_000_000)
          )

mintBondAttestationDatum :: MintBondContext -> PD.Data
mintBondAttestationDatum fixture =
  PD.Constr
    0
    [ PD.B headerHash
    , commitmentDataFor (mintBondDeploymentIdentity fixture)
    , PD.I 2
    , PD.B committeeSignersHash
    , builtinDataToData (toBuiltinData challengerAddress)
    , PD.B (BS.cons 0xc0 $ BS.replicate 31 0)
    , PD.I 2
    ]

mintBondDatum :: MintBondContext -> PD.Data
mintBondDatum fixture =
  PD.Constr
    0
    [ commitmentDataFor (mintBondDeploymentIdentity fixture)
    , tokenNameData mintBondAssetName
    , PD.B committeeSignersHash
    , PD.B (BS.cons 0xc0 $ BS.replicate 31 0)
    ]

data CloseChallengeContext = CloseChallengeContext
  { closeDaRefundOutputIndex :: Integer
  , closeChallengerRefundOutputIndex :: Integer
  }

validCloseChallenge :: CloseChallengeContext
validCloseChallenge =
  CloseChallengeContext
    { closeDaRefundOutputIndex = 1
    , closeChallengerRefundOutputIndex = 2
    }

runCloseChallenge :: CloseChallengeContext -> forall s. Term s PUnit
runCloseChallenge fixture =
  availabilityChallengeValidator
    # pdata (pconstant hubOraclePolicy)
    # pdata canonicalParameters
    # pconstant (closeChallengeContext fixture)

closeChallengeContext :: CloseChallengeContext -> ScriptContext
closeChallengeContext fixture =
  addInput bondInput $
    addInput stateQueueInput $
      addInput terminalInput $
        buildScriptContext $
          withMintingScript closeBurn (dataToBuiltinData redeemer)
            <> withRedeemer (dataToBuiltinData redeemer)
            <> withReferenceInput
              ( withOutRef hubRef
                  <> withAddress hubAddress
                  <> withValue
                    (mkAdaValue 2_000_000 <> singleton hubOraclePolicy (TokenName "MIDGARD_HUB_ORACLE") 1)
                  <> withInlineDatum (dataToBuiltinData hubDatumData)
              )
            <> withOutput
              ( withTxOutAddress stateQueueAddress
                  <> withTxOutValue stateQueueValue
                  <> withTxOutInlineDatum
                    ( dataToBuiltinData $
                        stateQueueDatum $
                          PD.Constr 3 [PD.B publishedCommitment]
                    )
              )
            <> withOutput
              ( withTxOutAddress bondOwnerAddress
                  <> withTxOutValue (mkAdaValue daBondLovelace)
              )
            <> withOutput
              ( withTxOutAddress challengerAddress
                  <> withTxOutValue (mkAdaValue $ terminalRemainingLovelace - fromInteger closeFee)
              )
            <> withFee closeFee
            <> withValidRange (interval (POSIXTime 0) $ POSIXTime 1_000)
  where
    redeemer =
      PD.Constr
        3
        [ PD.I 0
        , PD.I 0
        , PD.I 2
        , PD.I 1
        , PD.I 0
        , PD.I $ closeDaRefundOutputIndex fixture
        , PD.I $ closeChallengerRefundOutputIndex fixture
        ]
    closeBurn =
      singleton availabilityPolicy daBondAssetName (-1)
        <> singleton availabilityPolicy challengeAssetName (-1)
        <> singleton availabilityPolicy terminalAssetName (-1)
    bondInput =
      mkInput
        ( withOutRef closeBondRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue daBondLovelace
                  <> singleton availabilityPolicy daBondAssetName 1
                  <> singleton availabilityPolicy challengeAssetName 1
              )
            <> withInlineDatum (dataToBuiltinData challengedBondDatum)
        )
    stateQueueInput =
      mkInput
        ( withOutRef closeStateQueueRef
            <> withAddress stateQueueAddress
            <> withValue stateQueueValue
            <> withInlineDatum
              ( dataToBuiltinData $
                  stateQueueDatum $
                    PD.Constr 2 [tokenNameData daBondAssetName, tokenNameData challengeAssetName]
              )
        )
    terminalInput =
      mkInput
        ( withOutRef closeTerminalRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue terminalRemainingLovelace
                  <> singleton availabilityPolicy terminalAssetName 1
              )
            <> withInlineDatum (dataToBuiltinData closeTerminalDatum)
        )

closeTerminalDatum :: PD.Data
closeTerminalDatum =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , PD.I 1
    , PD.B publishedTerminalFold
    , PD.Constr 0 []
    , PD.I responseDeadline
    , PD.B challengerBytes
    , PD.I $ fromIntegral terminalRemainingLovelace
    ]

data TimeoutChallengeContext = TimeoutChallengeContext
  { timeoutDaSlashOutputIndex :: Integer
  , timeoutChallengerRefundOutputIndex :: Integer
  , timeoutFee :: Integer
  , timeoutLowerBound :: Integer
  , timeoutStateQueueChallengeName :: BS.ByteString
  , timeoutTerminalInputIndex :: Integer
  , timeoutExtraChallengerOutput :: Bool
  }

validTimeoutChallenge :: TimeoutChallengeContext
validTimeoutChallenge =
  TimeoutChallengeContext
    { timeoutDaSlashOutputIndex = 0
    , timeoutChallengerRefundOutputIndex = 1
    , timeoutFee = closeFee
    , timeoutLowerBound = responseDeadline
    , timeoutStateQueueChallengeName = challengeAssetNameBytes
    , timeoutTerminalInputIndex = 1
    , timeoutExtraChallengerOutput = False
    }

runTimeoutChallenge :: TimeoutChallengeContext -> forall s. Term s PUnit
runTimeoutChallenge fixture =
  availabilityChallengeValidator
    # pdata (pconstant hubOraclePolicy)
    # pdata canonicalParameters
    # pconstant (timeoutChallengeContext fixture)

timeoutChallengeContext :: TimeoutChallengeContext -> ScriptContext
timeoutChallengeContext fixture =
  addInput bondInput $
    addInput terminalInput $
      buildScriptContext $
        withMintingScript timeoutBurn (dataToBuiltinData redeemer)
          <> withMint stateQueueBurn (dataToBuiltinData stateQueueRedeemer)
          <> withRedeemer (dataToBuiltinData redeemer)
          <> withReferenceInput
            ( withOutRef hubRef
                <> withAddress hubAddress
                <> withValue
                  (mkAdaValue 2_000_000 <> singleton hubOraclePolicy (TokenName "MIDGARD_HUB_ORACLE") 1)
                <> withInlineDatum (dataToBuiltinData hubDatumData)
            )
          <> withOutput
            ( withTxOutAddress challengerAddress
                <> withTxOutValue (mkAdaValue daBondLovelace)
            )
          <> withOutput
            ( withTxOutAddress challengerAddress
                <> withTxOutValue
                  (mkAdaValue $ terminalRemainingLovelace - fromInteger (timeoutFee fixture))
            )
          <> extraChallengerOutput fixture
          <> withFee (timeoutFee fixture)
          <> withValidRange
            ( interval
                (POSIXTime $ timeoutLowerBound fixture)
                (POSIXTime $ timeoutLowerBound fixture + 1_000)
            )
  where
    redeemer =
      PD.Constr
        4
        [ PD.I 0
        , PD.I 0
        , PD.I $ timeoutTerminalInputIndex fixture
        , PD.I 1
        , PD.I $ timeoutDaSlashOutputIndex fixture
        , PD.I $ timeoutChallengerRefundOutputIndex fixture
        ]
    stateQueueRedeemer =
      PD.Constr
        5
        [ PD.B headerHash
        , PD.B $ timeoutStateQueueChallengeName fixture
        , PD.Constr
            0
            [ PD.I 0
            , builtinDataToData $ toBuiltinData closeBondRef
            , PD.I 0
            ]
        ]
    timeoutBurn =
      singleton availabilityPolicy daBondAssetName (-1)
        <> singleton availabilityPolicy challengeAssetName (-1)
        <> singleton availabilityPolicy terminalAssetName (-1)
    stateQueueBurn =
      singleton stateQueuePolicy (TokenName $ toBuiltin $ "MBLC" <> headerHash) (-1)
    bondInput =
      mkInput
        ( withOutRef closeBondRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue daBondLovelace
                  <> singleton availabilityPolicy daBondAssetName 1
                  <> singleton availabilityPolicy challengeAssetName 1
              )
            <> withInlineDatum (dataToBuiltinData challengedBondDatum)
        )
    terminalInput =
      mkInput
        ( withOutRef closeTerminalRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue terminalRemainingLovelace
                  <> singleton availabilityPolicy terminalAssetName 1
              )
            <> withInlineDatum (dataToBuiltinData timeoutTerminalDatum)
        )

extraChallengerOutput :: TimeoutChallengeContext -> ScriptContextBuilder
extraChallengerOutput fixture
  | timeoutExtraChallengerOutput fixture =
      withOutput
        ( withTxOutAddress challengerAddress
            <> withTxOutValue (mkAdaValue 1)
        )
  | otherwise = mempty

timeoutTerminalDatum :: PD.Data
timeoutTerminalDatum =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , PD.I 1
    , PD.B timedOutTerminalFold
    , PD.Constr 1 []
    , PD.I responseDeadline
    , PD.B challengerBytes
    , PD.I $ fromIntegral terminalRemainingLovelace
    ]

data OpenChallengeContext = OpenChallengeContext
  { openBondOutputIndex :: Integer
  , openStateQueueOutputIndex :: Integer
  , openSigner :: BS.ByteString
  }

validOpenChallenge :: OpenChallengeContext
validOpenChallenge =
  OpenChallengeContext
    { openBondOutputIndex = 0
    , openStateQueueOutputIndex = 1
    , openSigner = challengerBytes
    }

runOpenChallenge :: OpenChallengeContext -> forall s. Term s PUnit
runOpenChallenge fixture =
  availabilityChallengeValidator
    # pdata (pconstant hubOraclePolicy)
    # pdata canonicalParameters
    # pconstant (openChallengeContext fixture)

openChallengeContext :: OpenChallengeContext -> ScriptContext
openChallengeContext fixture =
  buildScriptContext $
    withMintingScript openMint (dataToBuiltinData redeemer)
      <> withRedeemer (dataToBuiltinData redeemer)
      <> withReferenceInput
        ( withOutRef hubRef
            <> withAddress hubAddress
            <> withValue
              (mkAdaValue 2_000_000 <> singleton hubOraclePolicy (TokenName "MIDGARD_HUB_ORACLE") 1)
            <> withInlineDatum (dataToBuiltinData hubDatumData)
        )
      <> withScriptInput
        (dataToBuiltinData $ PD.Constr 2 [PD.I 0])
        ( withOutRef bondRef
            <> withAddress scriptAddress
            <> withValue
              (mkAdaValue daBondLovelace <> singleton availabilityPolicy daBondAssetName 1)
            <> withInlineDatum (dataToBuiltinData availableBondDatum)
        )
      <> withScriptInput
        (dataToBuiltinData $ PD.Constr 2 [PD.I 0])
        ( withOutRef openStateQueueRef
            <> withAddress stateQueueAddress
            <> withValue stateQueueValue
            <> withInlineDatum (dataToBuiltinData $ stateQueueDatum $ PD.Constr 1 [tokenNameData daBondAssetName])
        )
      <> withInput
        ( withOutRef challengerRef
            <> withAddress challengerAddress
            <> withValue (mkAdaValue $ daBondLovelace + fromInteger openFee)
        )
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue
              ( mkAdaValue daBondLovelace
                  <> singleton availabilityPolicy daBondAssetName 1
                  <> singleton availabilityPolicy challengeAssetName 1
              )
            <> withTxOutInlineDatum (dataToBuiltinData challengedBondDatum)
        )
      <> withOutput
        ( withTxOutAddress stateQueueAddress
            <> withTxOutValue stateQueueValue
            <> withTxOutInlineDatum
              ( dataToBuiltinData $
                  stateQueueDatum $
                    PD.Constr 2 [tokenNameData daBondAssetName, tokenNameData challengeAssetName]
              )
        )
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue
              ( mkAdaValue (fromInteger initialTrancheLovelace)
                  <> singleton availabilityPolicy trancheAssetName 1
              )
            <> withTxOutInlineDatum (dataToBuiltinData activeDatum)
        )
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue
              ( mkAdaValue (fromInteger terminalInitialLovelace)
                  <> singleton availabilityPolicy terminalAssetName 1
              )
            <> withTxOutInlineDatum (dataToBuiltinData initialTerminalDatum)
        )
      <> withSigner (PubKeyHash $ toBuiltin $ openSigner fixture)
      <> withFee openFee
      <> withValidRange (interval (POSIXTime 0) $ POSIXTime 1_000)
  where
    redeemer =
      PD.Constr
        1
        [ PD.I 0
        , PD.I 0
        , PD.I $ openBondOutputIndex fixture
        , PD.I 2
        , PD.I 1
        , PD.I $ openStateQueueOutputIndex fixture
        , PD.I 2
        , PD.I 3
        , PD.B challengerBytes
        ]
    openMint =
      singleton availabilityPolicy challengeAssetName 1
        <> singleton availabilityPolicy terminalAssetName 1
        <> singleton availabilityPolicy trancheAssetName 1

runSpend :: PublicationContext -> forall s. Term s PUnit
runSpend fixture =
  runContext $ publicationScriptContext fixture

runContext :: ScriptContext -> forall s. Term s PUnit
runContext ctx =
  availabilityChallengeSpendValidator
    # pdata canonicalParameters
    # pconstant ctx

runSettlement :: SettlementContext -> forall s. Term s PUnit
runSettlement fixture =
  availabilityChallengeValidator
    # pdata (pconstant hubOraclePolicy)
    # pdata canonicalParameters
    # pconstant (settlementScriptContext fixture)

data SettlementContext = SettlementContext
  { settlementPublished :: Bool
  , settlementFee :: Integer
  , settlementLowerBound :: Integer
  , settlementTrancheInputIndex :: Integer
  , settlementSubstituteCarrier :: Bool
  , settlementExtraInput :: Bool
  }

publishedSettlement, timedOutSettlement :: SettlementContext
publishedSettlement =
  SettlementContext
    { settlementPublished = True
    , settlementFee = 500_000
    , settlementLowerBound = 0
    , settlementTrancheInputIndex = 1
    , settlementSubstituteCarrier = False
    , settlementExtraInput = False
    }
timedOutSettlement =
  SettlementContext
    { settlementPublished = False
    , settlementFee = 500_000
    , settlementLowerBound = responseDeadline
    , settlementTrancheInputIndex = 1
    , settlementSubstituteCarrier = False
    , settlementExtraInput = False
    }

settlementScriptContext :: SettlementContext -> ScriptContext
settlementScriptContext fixture =
  buildScriptContext $
    withMintingScript
      (singleton availabilityPolicy trancheAssetName (-1))
      (dataToBuiltinData $ settleRedeemer fixture)
      <> withRedeemer (dataToBuiltinData $ settleRedeemer fixture)
      <> withReferenceInput
        ( withOutRef bondRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue daBondLovelace
                  <> singleton availabilityPolicy daBondAssetName 1
                  <> singleton availabilityPolicy challengeAssetName 1
              )
            <> withInlineDatum (dataToBuiltinData challengedBondDatum)
        )
      <> withScriptInput
        (dataToBuiltinData $ PD.Constr 2 [PD.I 0])
        ( withOutRef terminalRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue terminalRemainingLovelace
                  <> singleton availabilityPolicy terminalAssetName 1
              )
            <> withInlineDatum (dataToBuiltinData terminalDatum)
        )
      <> withScriptInput
        (dataToBuiltinData $ PD.Constr 2 [PD.I 0])
        ( withOutRef trancheRef
            <> withAddress scriptAddress
            <> withValue
              ( mkAdaValue inputLovelace
                  <> singleton availabilityPolicy trancheAssetName 1
              )
            <> withInlineDatum
              (dataToBuiltinData $ if settlementPublished fixture then receiptDatum else activeDatum)
        )
      <> carrierInput fixture
      <> extraSettlementInput fixture
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue
              ( mkAdaValue (fromInteger $ settlementOutputLovelace fixture)
                  <> singleton availabilityPolicy terminalAssetName 1
              )
            <> withTxOutInlineDatum (dataToBuiltinData $ settlementOutputDatum fixture)
        )
      <> withFee (settlementFee fixture)
      <> withValidRange
        ( interval
            (POSIXTime $ settlementLowerBound fixture)
            (POSIXTime $ max responseDeadline (settlementLowerBound fixture + 1))
        )

carrierInput :: SettlementContext -> ScriptContextBuilder
carrierInput fixture
  | not (settlementPublished fixture) = mempty
  | otherwise =
      withScriptInput
        (dataToBuiltinData $ PD.Constr 1 [PD.I 1, PD.I 0])
        ( withOutRef
            (if settlementSubstituteCarrier fixture then substitutedCarrierRef else settlementCarrierRef)
            <> withAddress scriptAddress
            <> withValue (mkAdaValue carrierLovelace)
            <> withInlineDatum (dataToBuiltinData publicationDatum)
        )

extraSettlementInput :: SettlementContext -> ScriptContextBuilder
extraSettlementInput fixture
  | not (settlementExtraInput fixture) = mempty
  | otherwise =
      withScriptInput
        (dataToBuiltinData $ PD.Constr 2 [PD.I 0])
        ( withOutRef extraRef
            <> withAddress scriptAddress
            <> withValue (mkAdaValue 2_000_000)
            <> withInlineDatum (dataToBuiltinData terminalDatum)
        )

settleRedeemer :: SettlementContext -> PD.Data
settleRedeemer fixture =
  PD.Constr
    2
    [ PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I $ settlementTrancheInputIndex fixture
    , if settlementPublished fixture then PD.Constr 0 [PD.I 2] else PD.Constr 1 []
    ]

settlementOutputLovelace :: SettlementContext -> Integer
settlementOutputLovelace fixture =
  fromIntegral terminalRemainingLovelace
    + fromIntegral inputLovelace
    + (if settlementPublished fixture then fromIntegral carrierLovelace else 0)
    - settlementFee fixture

settlementOutputDatum :: SettlementContext -> PD.Data
settlementOutputDatum fixture =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , PD.I 1
    , PD.B $ if settlementPublished fixture then publishedTerminalFold else timedOutTerminalFold
    , PD.Constr (if settlementPublished fixture then 0 else 1) []
    , PD.I responseDeadline
    , PD.B challengerBytes
    , PD.I $ settlementOutputLovelace fixture
    ]

availableBondDatum, challengedBondDatum, commitmentData, terminalDatum, initialTerminalDatum :: PD.Data
availableBondDatum =
  PD.Constr
    0
    [ commitmentData
    , tokenNameData daBondAssetName
    , PD.B committeeSignersHash
    , PD.B ""
    ]

challengedBondDatum =
  PD.Constr
    1
    [ commitmentData
    , tokenNameData daBondAssetName
    , PD.B committeeSignersHash
    , PD.B ""
    , tokenNameData challengeAssetName
    , PD.B challengerBytes
    , PD.I 0
    , PD.I responseDeadline
    ]

commitmentData =
  commitmentDataFor deploymentIdentity

commitmentDataFor :: BS.ByteString -> PD.Data
commitmentDataFor commitmentDeploymentIdentity =
  PD.Constr
    0
    [ PD.I 1
    , PD.B commitmentDeploymentIdentity
    , PD.B headerHash
    , PD.I 1
    , responseGeometryData
    , PD.List [descriptorData]
    , PD.B bondOwnerBytes
    ]

terminalDatum =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , PD.I 0
    , PD.B terminalStartAccumulator
    , PD.Constr 0 []
    , PD.I responseDeadline
    , PD.B challengerBytes
    , PD.I $ fromIntegral terminalRemainingLovelace
    ]

initialTerminalDatum =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , PD.I 0
    , PD.B terminalStartAccumulator
    , PD.Constr 0 []
    , PD.I responseDeadline
    , PD.B challengerBytes
    , PD.I terminalInitialLovelace
    ]

responseGeometryData :: PD.Data
responseGeometryData =
  PD.Constr
    0
    [ PD.I maximumChunkBytes
    , PD.I trancheBytes
    , PD.I 16
    ]

hubDatumData :: PD.Data
hubDatumData =
  PD.Constr
    0
    ( map (PD.B . fromBuiltin . unCurrencySymbol) hubPolicies
        <> replicate 13 (builtinDataToData $ toBuiltinData miscellaneousAddress)
        <> [PD.B $ fromBuiltin $ unCurrencySymbol miscellaneousPolicy]
    )
  where
    hubPolicies =
      replicate 4 miscellaneousPolicy
        <> [stateQueuePolicy]
        <> replicate 7 miscellaneousPolicy

stateQueueDatum :: PD.Data -> PD.Data
stateQueueDatum status =
  PD.Constr
    0
    [ PD.Constr 1 [PD.Constr 0 [headerData, status]]
    , PD.Constr 1 []
    ]

headerData :: PD.Data
headerData =
  PD.Constr
    0
    ( replicate 9 (PD.B emptyRoot)
        <> replicate 7 (PD.I 0)
        <> [ PD.I 0
           , PD.I 1
           , PD.I 0
           , PD.I 0
           , PD.I 0
           , PD.I 0
           , PD.B $ fromBuiltin $ unCurrencySymbol miscellaneousPolicy
           , PD.B bondOwnerBytes
           , PD.I 1
           ]
    )

terminalStartAccumulator, publishedTerminalFold, timedOutTerminalFold, publishedCommitment :: BS.ByteString
terminalStartAccumulator =
  hashData
    "MidgardDaAvailabilityTerminalStartV1"
    (PD.Constr 0 [PD.I 1, PD.B deploymentIdentity, PD.B headerHash, tokenNameData challengeAssetName])
publishedTerminalFold =
  hashData
    "MidgardDaAvailabilityTerminalStepV1"
    (PD.Constr 0 [PD.I 1, PD.B terminalStartAccumulator, PD.I 0, PD.Constr 0 [PD.B nextAccumulator]])
timedOutTerminalFold =
  hashData
    "MidgardDaAvailabilityTerminalStepV1"
    (PD.Constr 0 [PD.I 1, PD.B terminalStartAccumulator, PD.I 0, PD.Constr 1 [PD.I 0, PD.B startAccumulator]])
publishedCommitment = hashData "MidgardDaAvailabilityPublishedV1" commitmentData

tokenNameData :: TokenName -> PD.Data
tokenNameData (TokenName bytes) = PD.B $ fromBuiltin bytes

data PublicationContext = PublicationContext
  { publicationFee :: Integer
  , publicationUpperBound :: Integer
  , publicationCarrierOutputIndex :: Integer
  , publicationCarrierLovelace :: Integer
  , publicationThreadDatum :: PD.Data
  , publicationUnexpectedMint :: Bool
  }

validPublicationContext :: PublicationContext
validPublicationContext = publicationContext

publicationContext :: PublicationContext
publicationContext =
  PublicationContext
    { publicationFee = maximumPublicationFee
    , publicationUpperBound = 1_000
    , publicationCarrierOutputIndex = 1
    , publicationCarrierLovelace = fromIntegral carrierLovelace
    , publicationThreadDatum = receiptDatum
    , publicationUnexpectedMint = False
    }

publicationScriptContext :: PublicationContext -> ScriptContext
publicationScriptContext fixture =
  buildScriptContext $
    withSpendingScript
      (dataToBuiltinData $ advanceRedeemer 0 (publicationCarrierOutputIndex fixture))
      ( withOutRef ownRef
          <> withAddress scriptAddress
          <> withValue (mkAdaValue inputLovelace <> singleton availabilityPolicy trancheAssetName 1)
          <> withInlineDatum (dataToBuiltinData activeDatum)
      )
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue (mkAdaValue threadLovelace <> singleton availabilityPolicy trancheAssetName 1)
            <> withTxOutInlineDatum (dataToBuiltinData $ publicationThreadDatum fixture)
        )
      <> withOutput
        ( withTxOutAddress scriptAddress
            <> withTxOutValue (mkAdaValue $ fromInteger $ publicationCarrierLovelace fixture)
            <> withTxOutInlineDatum (dataToBuiltinData publicationDatum)
        )
      <> withFee (publicationFee fixture)
      <> withValidRange (interval (POSIXTime 0) $ POSIXTime $ publicationUpperBound fixture)
      <> if publicationUnexpectedMint fixture
        then withMint (singleton foreignPolicy (TokenName "unexpected") 1) (dataToBuiltinData $ PD.Constr 0 [])
        else mempty

validConsumeContext :: ScriptContext
validConsumeContext = consumeScriptContext 0

consumeScriptContext :: Integer -> ScriptContext
consumeScriptContext previousCarrierInputIndex =
  buildScriptContext $
    withSpendingScript
      (dataToBuiltinData $ PD.Constr 1 [PD.I 1, PD.I 1])
      ( withOutRef carrierRef
          <> withAddress scriptAddress
          <> withValue (mkAdaValue carrierLovelace)
          <> withInlineDatum (dataToBuiltinData publicationDatum)
      )
      <> withScriptInput
        (dataToBuiltinData $ advanceRedeemerWithPrevious 0 1 previousCarrierInputIndex)
        ( withOutRef threadRef
            <> withAddress scriptAddress
            <> withValue (mkAdaValue inputLovelace <> singleton availabilityPolicy trancheAssetName 1)
            <> withInlineDatum (dataToBuiltinData activeDatum)
        )

consumeWithoutPreviousContext :: ScriptContext
consumeWithoutPreviousContext =
  buildScriptContext $
    withSpendingScript
      (dataToBuiltinData $ PD.Constr 1 [PD.I 1, PD.I 1])
      ( withOutRef carrierRef
          <> withAddress scriptAddress
          <> withValue (mkAdaValue carrierLovelace)
          <> withInlineDatum (dataToBuiltinData publicationDatum)
      )
      <> withScriptInput
        (dataToBuiltinData $ advanceRedeemer 0 1)
        ( withOutRef threadRef
            <> withAddress scriptAddress
            <> withValue (mkAdaValue inputLovelace <> singleton availabilityPolicy trancheAssetName 1)
            <> withInlineDatum (dataToBuiltinData activeDatum)
        )

coordinateContext :: PD.Data -> ScriptContext
coordinateContext mintRedeemer =
  buildScriptContext $
    withSpendingScript
      (dataToBuiltinData $ PD.Constr 2 [PD.I 1])
      ( withOutRef ownRef
          <> withAddress scriptAddress
          <> withValue (mkAdaValue inputLovelace <> singleton availabilityPolicy (TokenName "bond") 1)
          <> withInlineDatum (dataToBuiltinData $ PD.Constr 0 [])
      )
      <> withMint (singleton availabilityPolicy (TokenName "challenge") 1) (dataToBuiltinData mintRedeemer)

canonicalParameters :: forall s. Term s PParametersV1
canonicalParameters =
  pcon $
    PParametersV1
      ( pdata $
          pcon $
            PResponseGeometryV1
              (pdata $ pconstant maximumChunkBytes)
              (pdata $ pconstant trancheBytes)
              (pdata 16)
      )
      (pdata 10_000_000_000)
      (pdata 10_000_000_000)
      (pdata 500_000)
      (pdata $ pconstant maximumPublicationFee)
      (pdata 500_000)
      (pdata 1_000_000)
      (pdata 1_200_000)

advanceRedeemer :: Integer -> Integer -> PD.Data
advanceRedeemer threadOutputIndex carrierOutputIndex =
  PD.Constr
    0
    [ PD.I threadOutputIndex
    , PD.I carrierOutputIndex
    , PD.Constr 1 []
    ]

advanceRedeemerWithPrevious :: Integer -> Integer -> Integer -> PD.Data
advanceRedeemerWithPrevious threadOutputIndex carrierOutputIndex previousCarrierInputIndex =
  PD.Constr
    0
    [ PD.I threadOutputIndex
    , PD.I carrierOutputIndex
    , PD.Constr 0 [PD.I previousCarrierInputIndex]
    ]

openChallengeRedeemer, mintBondRedeemer :: PD.Data
openChallengeRedeemer = openChallengeRedeemerAt 0
openChallengeRedeemerAt :: Integer -> PD.Data
openChallengeRedeemerAt bondInputIndex =
  PD.Constr
    1
    [ PD.I 0
    , PD.I bondInputIndex
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.B challengerBytes
    ]
mintBondRedeemer = PD.Constr 0 (replicate 6 $ PD.I 0)

activeDatum, receiptDatum, publicationDatum, descriptorData :: PD.Data
activeDatum =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , descriptorData
    , PD.I 0
    , PD.B startAccumulator
    , PD.Constr 1 []
    , PD.I responseDeadline
    , PD.B challengerBytes
    ]

receiptDatum =
  PD.Constr
    1
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , descriptorData
    , PD.B nextAccumulator
    , PD.I 1
    , PD.B challengerBytes
    ]

publicationDatum =
  PD.Constr
    0
    [ PD.B deploymentIdentity
    , PD.B headerHash
    , PD.B challengeAssetNameBytes
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 1
    , PD.B chunkHash
    , PD.List [PD.Constr 0 [PD.I 0, PD.B chunkLeaf]]
    , PD.List []
    , PD.B startAccumulator
    , PD.B nextAccumulator
    , PD.B chunk
    ]

descriptorData =
  PD.Constr
    0
    [ PD.I 0
    , PD.I 0
    , PD.I 1
    , PD.I 1
    , PD.B chunkCommitment
    , PD.B nextAccumulator
    ]

startAccumulator, nextAccumulator, chunkHash, chunkLeaf, chunkCommitment :: BS.ByteString
startAccumulator =
  hashData
    "MidgardDaAvailabilityTrancheStartV1"
    (PD.Constr 0 [PD.I 1, PD.B deploymentIdentity, PD.B headerHash, PD.I 0, PD.I 0, PD.I 1])

nextAccumulator =
  hashData
    "MidgardDaAvailabilityTrancheStepV1"
    ( PD.Constr
        0
        [ PD.I 1
        , PD.B deploymentIdentity
        , PD.B headerHash
        , PD.I 0
        , PD.I 0
        , PD.I 1
        , PD.B chunkHash
        , PD.B startAccumulator
        ]
    )

chunkHash = hashBytes chunk
chunkLeaf =
  hashData
    "MidgardDaAvailabilityChunkLeafV1"
    (PD.Constr 0 [PD.I 1, PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.B chunkHash])
chunkCommitment =
  hashBytes $
    "MidgardValidationMerkleFrontierV1"
      <> "\x01"
      <> "\x81\x82\x00\x58\x20"
      <> chunkLeaf

hashData :: BS.ByteString -> PD.Data -> BS.ByteString
hashData domain value =
  hashBytes $ domain <> fromBuiltin (serialiseData $ dataToBuiltinData value)

hashBytes :: BS.ByteString -> BS.ByteString
hashBytes = fromBuiltin . blake2b_256 . toBuiltin

hashData224 :: PD.Data -> BS.ByteString
hashData224 = fromBuiltin . blake2b_224 . serialiseData . dataToBuiltinData

deploymentIdentity, headerHash, challengeAssetNameBytes, challengerBytes, bondOwnerBytes, chunk :: BS.ByteString
deploymentIdentity = BS.replicate 28 0x11
headerHash = hashData224 headerData
challengeAssetNameBytes = "DACH" <> hashData224 (builtinDataToData $ toBuiltinData bondRef)
challengerBytes = BS.replicate 28 0x66
bondOwnerBytes = BS.replicate 28 0x55
chunk = "\x01"

availabilityPolicy, daAttestationPolicy, foreignPolicy, hubOraclePolicy, stateQueuePolicy, miscellaneousPolicy :: CurrencySymbol
availabilityPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x33
daAttestationPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x12
foreignPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x44
hubOraclePolicy = CurrencySymbol $ toBuiltin deploymentIdentity
stateQueuePolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x22
miscellaneousPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x44

scriptAddress, daAttestationAddress, hubAddress, stateQueueAddress, miscellaneousAddress, challengerAddress, bondOwnerAddress :: Address
scriptAddress = scriptHashAddress $ ScriptHash $ unCurrencySymbol availabilityPolicy
daAttestationAddress = scriptHashAddress $ ScriptHash $ unCurrencySymbol daAttestationPolicy
hubAddress = scriptHashAddress $ ScriptHash $ unCurrencySymbol hubOraclePolicy
stateQueueAddress = scriptHashAddress $ ScriptHash $ unCurrencySymbol stateQueuePolicy
miscellaneousAddress = scriptHashAddress $ ScriptHash $ unCurrencySymbol miscellaneousPolicy
challengerAddress = pubKeyHashAddress $ PubKeyHash $ toBuiltin challengerBytes
bondOwnerAddress = pubKeyHashAddress $ PubKeyHash $ toBuiltin bondOwnerBytes

challengeAssetName, daBondAssetName, terminalAssetName, trancheAssetName :: TokenName
challengeAssetName = TokenName $ toBuiltin challengeAssetNameBytes
daBondAssetName = TokenName "DAB"
terminalAssetName = TokenName $ toBuiltin $ "DACT" <> BS.drop 4 challengeAssetNameBytes
trancheAssetName = TokenName $ toBuiltin $ "DT" <> BS.drop 4 challengeAssetNameBytes <> "\x00\x00"

mintBondAssetName, daAttestationAssetName :: TokenName
mintBondAssetName = TokenName $ toBuiltin $ "DABN" <> hashData224 (builtinDataToData $ toBuiltinData mintBondAttestationRef)
daAttestationAssetName = TokenName $ toBuiltin $ "DAAT" <> headerHash

ownRef :: TxOutRef
ownRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0xaa) 0

threadRef, carrierRef :: TxOutRef
threadRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0xbb) 0
carrierRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0xbb) 1

bondRef, terminalRef, trancheRef, settlementCarrierRef, substitutedCarrierRef, extraRef :: TxOutRef
bondRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x40) 0
terminalRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x30) 0
trancheRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x20) 2
settlementCarrierRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x20) 1
substitutedCarrierRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x19) 1
extraRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x10) 0

hubRef, openStateQueueRef, challengerRef :: TxOutRef
hubRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x50) 0
openStateQueueRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x30) 0
challengerRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x20) 0

mintBondAttestationRef, mintBondStateQueueRef, mintBondWalletRef :: TxOutRef
mintBondAttestationRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x20) 0
mintBondStateQueueRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x30) 0
mintBondWalletRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x60) 0

closeBondRef, closeStateQueueRef, closeTerminalRef :: TxOutRef
closeBondRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x40) 0
closeStateQueueRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x50) 0
closeTerminalRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x60) 0

inputLovelace, threadLovelace, carrierLovelace :: Int
inputLovelace = 10_000_000
threadLovelace = 8_000_000
carrierLovelace = 1_500_000

daBondLovelace, terminalRemainingLovelace :: Int
daBondLovelace = 10_000_000_000
terminalRemainingLovelace = 10_000_000_000

committeeSignersHash :: BS.ByteString
committeeSignersHash = BS.replicate 32 0x77

emptyRoot :: BS.ByteString
emptyRoot = BS.pack [0x0e, 0x57, 0x51, 0xc0, 0x26, 0xe5, 0x43, 0xb2, 0xe8, 0xab, 0x2e, 0xb0, 0x60, 0x99, 0xda, 0xa1, 0xd1, 0xe5, 0xdf, 0x47, 0x78, 0xf7, 0x78, 0x7f, 0xaa, 0xb4, 0x5c, 0xdf, 0x12, 0xfe, 0x3a, 0x8c]

stateQueueValue :: Value
stateQueueValue =
  mkAdaValue 2_000_000
    <> singleton stateQueuePolicy (TokenName $ toBuiltin $ "MBLC" <> headerHash) 1

openFee, maximumPublicationFee, responseDeadline, maximumChunkBytes, trancheBytes :: Integer
openFee = 100
maximumPublicationFee = 500_000
responseDeadline = 3_600_000
maximumChunkBytes = 14_020
trancheBytes = 4 * 1024 * 1024

closeFee :: Integer
closeFee = 1_000_000

initialTrancheLovelace, terminalInitialLovelace :: Integer
initialTrancheLovelace = 9_998_800_000
terminalInitialLovelace = 1_200_000
