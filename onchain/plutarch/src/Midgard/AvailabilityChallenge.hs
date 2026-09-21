{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.AvailabilityChallenge
Description : Plutarch port of @lib/midgard/availability-challenge.ak@.

These definitions preserve the Aiken constructor and field order, the canonical
commitment checks, the exact challenger-bond split, every domain-separated
accumulator, and the publication transition used by the validator.
-}
module Midgard.AvailabilityChallenge (
  pcommitmentVersionV1,
  psmallPayloadMaxBytesV1,
  pfullPayloadMaxBytesV1,
  psmallResponseWindowMsV1,
  pfullResponseWindowMsV1,
  pmaxResponseChunkSafetyBytesV1,
  pmaxTrancheCountSafetyV1,
  PResponseGeometryV1 (..),
  PParametersV1 (..),
  PTrancheDescriptorV1 (..),
  PCommitmentV1 (..),
  PStateQueueStatusV1 (..),
  PMintRedeemerV1 (..),
  PYieldRedeemer (..),
  PSpendRedeemerV1 (..),
  PBondDatumV1 (..),
  PTrancheDatumV1 (..),
  PPublicationDatumV1 (..),
  PTrancheTerminalStatusV1 (..),
  PTerminalAccumulatorDatumV1 (..),
  pdaBondAssetNamePrefixV1,
  pchallengeAssetNamePrefixV1,
  ptrancheAssetNamePrefixV1,
  pterminalAccumulatorAssetNamePrefixV1,
  pdaBondAssetNameV1,
  pchallengeAssetNameV1,
  ptrancheAssetNameV1,
  pterminalAccumulatorAssetNameV1,
  presponseWindowMsV1,
  presponseDeadlineV1,
  presponseGeometryIsCanonicalV1,
  pparametersAreCanonicalV1,
  pcommitmentIsCanonicalV1,
  pterminalAccumulatorReserveLovelaceV1,
  ptrancheInitialLovelacesV1,
  ptrancheInitialLovelaceV1,
  pterminalAccumulatorInitialLovelaceV1,
  pterminalAccumulatorStartV1,
  pfoldTerminalAccumulatorV1,
  pattestationMessageV1,
  ppublishedTerminalCommitmentV1,
  ptrancheStartAccumulatorV1,
  ptrancheStepAccumulatorV1,
  pchunkLeafHashV1,
  ppublicationAdvancesActiveTrancheV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantLast)
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PPubKeyHash, PTokenName (..), PTxOutRef)
import Plutarch.Prelude

import Midgard.LedgerState (PFrontierPeak, PHeaderHash)
import Midgard.ValidationMerkle qualified as ValidationMerkle

pcommitmentVersionV1 :: forall s. Term s PInteger
pcommitmentVersionV1 = 1

psmallPayloadMaxBytesV1 :: forall s. Term s PInteger
psmallPayloadMaxBytesV1 = 64 * 1024

pfullPayloadMaxBytesV1 :: forall s. Term s PInteger
pfullPayloadMaxBytesV1 = 64 * 1024 * 1024

psmallResponseWindowMsV1 :: forall s. Term s PInteger
psmallResponseWindowMsV1 = 60 * 60 * 1000

pfullResponseWindowMsV1 :: forall s. Term s PInteger
pfullResponseWindowMsV1 = 48 * 60 * 60 * 1000

pmaxResponseChunkSafetyBytesV1 :: forall s. Term s PInteger
pmaxResponseChunkSafetyBytesV1 = 15_148

pmaxTrancheCountSafetyV1 :: forall s. Term s PInteger
pmaxTrancheCountSafetyV1 = 64

data PResponseGeometryV1 (s :: S) = PResponseGeometryV1
  { presponseGeometry'chunkByteLength :: Term s (PAsData PInteger)
  , presponseGeometry'trancheByteLength :: Term s (PAsData PInteger)
  , presponseGeometry'maxTrancheCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResponseGeometryV1)

data PParametersV1 (s :: S) = PParametersV1
  { pparameters'responseGeometry :: Term s (PAsData PResponseGeometryV1)
  , pparameters'daBondLovelace :: Term s (PAsData PInteger)
  , pparameters'challengerBondLovelace :: Term s (PAsData PInteger)
  , pparameters'maxOpenFeeLovelace :: Term s (PAsData PInteger)
  , pparameters'maxPublicationFeeLovelace :: Term s (PAsData PInteger)
  , pparameters'maxSettlementFeeLovelace :: Term s (PAsData PInteger)
  , pparameters'maxCloseFeeLovelace :: Term s (PAsData PInteger)
  , pparameters'maxTimeoutFeeLovelace :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PParametersV1)

data PTrancheDescriptorV1 (s :: S) = PTrancheDescriptorV1
  { ptrancheDescriptor'trancheIndex :: Term s (PAsData PInteger)
  , ptrancheDescriptor'startOffset :: Term s (PAsData PInteger)
  , ptrancheDescriptor'byteLength :: Term s (PAsData PInteger)
  , ptrancheDescriptor'chunkCount :: Term s (PAsData PInteger)
  , ptrancheDescriptor'chunkCommitment :: Term s (PAsData PByteString)
  , ptrancheDescriptor'terminalAccumulator :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTrancheDescriptorV1)

data PCommitmentV1 (s :: S) = PCommitmentV1
  { pcommitment'version :: Term s (PAsData PInteger)
  , pcommitment'deploymentIdentity :: Term s (PAsData PByteString)
  , pcommitment'headerHash :: Term s (PAsData PHeaderHash)
  , pcommitment'payloadByteLength :: Term s (PAsData PInteger)
  , pcommitment'responseGeometry :: Term s (PAsData PResponseGeometryV1)
  , pcommitment'trancheDescriptors :: Term s (PAsData (PBuiltinList (PAsData PTrancheDescriptorV1)))
  , pcommitment'bondOwner :: Term s (PAsData PPubKeyHash)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommitmentV1)

data PStateQueueStatusV1 (s :: S)
  = PUnattested
  | PAttested (Term s (PAsData PTokenName))
  | PChallenged (Term s (PAsData PTokenName)) (Term s (PAsData PTokenName))
  | PPublished (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStateQueueStatusV1)

data PMintRedeemerV1 (s :: S)
  = PMintBondFromAttestation
      (Term s (PAsData PInteger)) -- authenticated yield reference input
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  | POpenChallenge
      (Term s (PAsData PInteger)) -- authenticated yield reference input
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PPubKeyHash))
  | PSettleTranche
      (Term s (PAsData PInteger)) -- authenticated yield reference input
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PMaybeData PInteger)))
  | PCloseChallenge
      (Term s (PAsData PInteger)) -- authenticated yield reference input
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  | PTimeoutChallenge
      (Term s (PAsData PInteger)) -- authenticated yield reference input
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemerV1)

data PYieldRedeemer (s :: S) = PYield
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PYieldRedeemer)

data PSpendRedeemerV1 (s :: S)
  = PAdvanceTranche
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PMaybeData PInteger)))
  | PConsumeCarrier (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PCoordinate (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemerV1)

data PBondDatumV1 (s :: S)
  = PAvailable
      (Term s (PAsData PCommitmentV1))
      (Term s (PAsData PTokenName))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  | PChallengedBond
      { pbond'commitment :: Term s (PAsData PCommitmentV1)
      , pbond'daBondAssetName :: Term s (PAsData PTokenName)
      , pbond'committeeSignersHash :: Term s (PAsData PByteString)
      , pbond'attestedSigners :: Term s (PAsData PByteString)
      , pbond'challengeAssetName :: Term s (PAsData PTokenName)
      , pbond'challenger :: Term s (PAsData PPubKeyHash)
      , pbond'openedAt :: Term s (PAsData PInteger)
      , pbond'responseDeadline :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBondDatumV1)

data PTrancheDatumV1 (s :: S)
  = PActiveTranche
      (Term s (PAsData PByteString))
      (Term s (PAsData PHeaderHash))
      (Term s (PAsData PTokenName))
      (Term s (PAsData PTrancheDescriptorV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PMaybeData PInteger)))
      (Term s (PAsData PInteger))
      (Term s (PAsData PPubKeyHash))
  | PReceipt
      (Term s (PAsData PByteString))
      (Term s (PAsData PHeaderHash))
      (Term s (PAsData PTokenName))
      (Term s (PAsData PTrancheDescriptorV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PPubKeyHash))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTrancheDatumV1)

data PPublicationDatumV1 (s :: S) = PPublicationDatumV1
  { ppublication'deploymentIdentity :: Term s (PAsData PByteString)
  , ppublication'headerHash :: Term s (PAsData PHeaderHash)
  , ppublication'challengeAssetName :: Term s (PAsData PTokenName)
  , ppublication'trancheIndex :: Term s (PAsData PInteger)
  , ppublication'chunkIndex :: Term s (PAsData PInteger)
  , ppublication'chunkOffset :: Term s (PAsData PInteger)
  , ppublication'chunkByteLength :: Term s (PAsData PInteger)
  , ppublication'chunkHash :: Term s (PAsData PByteString)
  , ppublication'chunkFrontier :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , ppublication'chunkSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , ppublication'previousAccumulator :: Term s (PAsData PByteString)
  , ppublication'nextAccumulator :: Term s (PAsData PByteString)
  , ppublication'chunk :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPublicationDatumV1)

data PTrancheTerminalStatusV1 (s :: S)
  = PPublishedTranche (Term s (PAsData PByteString))
  | PTimedOutTranche (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTrancheTerminalStatusV1)

data PTerminalAccumulatorDatumV1 (s :: S) = PTerminalAccumulatorDatumV1
  { pterminal'deploymentIdentity :: Term s (PAsData PByteString)
  , pterminal'headerHash :: Term s (PAsData PHeaderHash)
  , pterminal'challengeAssetName :: Term s (PAsData PTokenName)
  , pterminal'nextTrancheIndex :: Term s (PAsData PInteger)
  , pterminal'foldedTerminalAccumulator :: Term s (PAsData PByteString)
  , pterminal'hasTimedOutTranche :: Term s (PAsData PBool)
  , pterminal'responseDeadline :: Term s (PAsData PInteger)
  , pterminal'challenger :: Term s (PAsData PPubKeyHash)
  , pterminal'remainingChallengerLovelace :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTerminalAccumulatorDatumV1)

data PTrancheStartV1 (s :: S) = PTrancheStartV1
  { ptrancheStart'version :: Term s (PAsData PInteger)
  , ptrancheStart'deploymentIdentity :: Term s (PAsData PByteString)
  , ptrancheStart'headerHash :: Term s (PAsData PHeaderHash)
  , ptrancheStart'trancheIndex :: Term s (PAsData PInteger)
  , ptrancheStart'startOffset :: Term s (PAsData PInteger)
  , ptrancheStart'byteLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PTrancheStartV1)

data PTrancheStepV1 (s :: S) = PTrancheStepV1
  { ptrancheStep'version :: Term s (PAsData PInteger)
  , ptrancheStep'deploymentIdentity :: Term s (PAsData PByteString)
  , ptrancheStep'headerHash :: Term s (PAsData PHeaderHash)
  , ptrancheStep'trancheIndex :: Term s (PAsData PInteger)
  , ptrancheStep'chunkOffset :: Term s (PAsData PInteger)
  , ptrancheStep'chunkByteLength :: Term s (PAsData PInteger)
  , ptrancheStep'chunkHash :: Term s (PAsData PByteString)
  , ptrancheStep'previousAccumulator :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PTrancheStepV1)

data PTerminalAccumulatorStartV1 (s :: S) = PTerminalAccumulatorStartV1
  { pterminalStart'version :: Term s (PAsData PInteger)
  , pterminalStart'deploymentIdentity :: Term s (PAsData PByteString)
  , pterminalStart'headerHash :: Term s (PAsData PHeaderHash)
  , pterminalStart'challengeAssetName :: Term s (PAsData PTokenName)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PTerminalAccumulatorStartV1)

data PTerminalAccumulatorStepV1 (s :: S) = PTerminalAccumulatorStepV1
  { pterminalStep'version :: Term s (PAsData PInteger)
  , pterminalStep'previousAccumulator :: Term s (PAsData PByteString)
  , pterminalStep'trancheIndex :: Term s (PAsData PInteger)
  , pterminalStep'status :: Term s (PAsData PTrancheTerminalStatusV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PTerminalAccumulatorStepV1)

data PChunkLeafV1 (s :: S) = PChunkLeafV1
  { pchunkLeaf'version :: Term s (PAsData PInteger)
  , pchunkLeaf'trancheIndex :: Term s (PAsData PInteger)
  , pchunkLeaf'chunkIndex :: Term s (PAsData PInteger)
  , pchunkLeaf'chunkOffset :: Term s (PAsData PInteger)
  , pchunkLeaf'chunkByteLength :: Term s (PAsData PInteger)
  , pchunkLeaf'chunkHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PChunkLeafV1)

pdaBondAssetNamePrefixV1 :: forall s. Term s (PAsData PTokenName)
pdaBondAssetNamePrefixV1 = pdata (pcon (PTokenName (pconstant "DABN")))

pchallengeAssetNamePrefixV1 :: forall s. Term s (PAsData PTokenName)
pchallengeAssetNamePrefixV1 = pdata (pcon (PTokenName (pconstant "DACH")))

ptrancheAssetNamePrefixV1 :: forall s. Term s (PAsData PTokenName)
ptrancheAssetNamePrefixV1 = pdata (pcon (PTokenName (pconstant "DT")))

pterminalAccumulatorAssetNamePrefixV1 :: forall s. Term s (PAsData PTokenName)
pterminalAccumulatorAssetNamePrefixV1 = pdata (pcon (PTokenName (pconstant "DACT")))

pserialiseAndHash28 :: forall s a. (PIsData a) => Term s a -> Term s PByteString
pserialiseAndHash28 value = pblake2b_224 # (pserialiseData # pforgetData (pdata value))

-- | Aiken @da_bond_asset_name_v1@.
pdaBondAssetNameV1 :: forall s. Term s PTxOutRef -> Term s PTokenName
pdaBondAssetNameV1 attestationInputOutref =
  pcon $ PTokenName $ pto (pfromData pdaBondAssetNamePrefixV1) <> pserialiseAndHash28 attestationInputOutref

-- | Aiken @challenge_asset_name_v1@.
pchallengeAssetNameV1 :: forall s. Term s PTxOutRef -> Term s PTokenName
pchallengeAssetNameV1 bondInputOutref =
  pcon $ PTokenName $ pto (pfromData pchallengeAssetNamePrefixV1) <> pserialiseAndHash28 bondInputOutref

-- | Aiken @tranche_asset_name_v1@.
ptrancheAssetNameV1 :: forall s. Term s PTokenName -> Term s PInteger -> Term s PTokenName
ptrancheAssetNameV1 challengeAssetName trancheIndex =
  plet (pto challengeAssetName) $ \challengeNameBytes ->
    pif
      ( pand'List
          [ plengthBS # challengeNameBytes #== 32
          , 0 #<= trancheIndex
          , trancheIndex #< pmaxTrancheCountSafetyV1
          ]
      )
      ( pcon $
          PTokenName $
            pto (pfromData ptrancheAssetNamePrefixV1)
              <> (psliceBS # 4 # 28 # challengeNameBytes)
              <> (pintegerToByteString # pmostSignificantLast # 2 # trancheIndex)
      )
      perror

-- | Aiken @terminal_accumulator_asset_name_v1@.
pterminalAccumulatorAssetNameV1 :: forall s. Term s PTokenName -> Term s PTokenName
pterminalAccumulatorAssetNameV1 challengeAssetName =
  plet (pto challengeAssetName) $ \challengeNameBytes ->
    pif
      (plengthBS # challengeNameBytes #== 32)
      ( pcon $
          PTokenName $
            pto (pfromData pterminalAccumulatorAssetNamePrefixV1)
              <> (psliceBS # 4 # 28 # challengeNameBytes)
      )
      perror

pceilDiv :: forall s. Term s PInteger -> Term s PInteger -> Term s PInteger
pceilDiv numerator denominator = pdiv # (numerator + denominator - 1) # denominator

-- | Aiken @response_window_ms_v1@.
presponseWindowMsV1 :: forall s. Term s PInteger -> Term s (PMaybe PInteger)
presponseWindowMsV1 payloadByteLength =
  pif
    (0 #< payloadByteLength #&& payloadByteLength #<= psmallPayloadMaxBytesV1)
    (pcon $ PJust psmallResponseWindowMsV1)
    ( pif
        (psmallPayloadMaxBytesV1 #< payloadByteLength #&& payloadByteLength #<= pfullPayloadMaxBytesV1)
        (pcon $ PJust pfullResponseWindowMsV1)
        (pcon PNothing)
    )

-- | Aiken @response_deadline_v1@.
presponseDeadlineV1 :: forall s. Term s PInteger -> Term s PInteger -> Term s (PMaybe PInteger)
presponseDeadlineV1 payloadByteLength openedAt =
  pif
    (openedAt #< 0)
    (pcon PNothing)
    ( pmatch (presponseWindowMsV1 payloadByteLength) $ \case
        PNothing -> pcon PNothing
        PJust window -> pcon $ PJust $ openedAt + window
    )

-- | Aiken @response_geometry_is_canonical_v1@.
presponseGeometryIsCanonicalV1 :: forall s. Term s (PResponseGeometryV1 :--> PBool)
presponseGeometryIsCanonicalV1 = phoistAcyclic $
  plam $ \geometry ->
    pmatch geometry $ \PResponseGeometryV1 {..} ->
      let chunkByteLength = pfromData presponseGeometry'chunkByteLength
          trancheByteLength = pfromData presponseGeometry'trancheByteLength
          maxTrancheCount = pfromData presponseGeometry'maxTrancheCount
       in pif
            (0 #< chunkByteLength #&& 0 #< trancheByteLength)
            ( pand'List
                [ chunkByteLength #<= pmaxResponseChunkSafetyBytesV1
                , psmallPayloadMaxBytesV1 #<= trancheByteLength
                , trancheByteLength #<= pfullPayloadMaxBytesV1
                , 0 #< maxTrancheCount
                , maxTrancheCount #<= pmaxTrancheCountSafetyV1
                , pceilDiv pfullPayloadMaxBytesV1 trancheByteLength #<= maxTrancheCount
                ]
            )
            (pconstant False)

-- | Aiken @parameters_are_canonical_v1@.
pparametersAreCanonicalV1 :: forall s. Term s (PParametersV1 :--> PBool)
pparametersAreCanonicalV1 = phoistAcyclic $
  plam $ \parameters ->
    pmatch parameters $ \PParametersV1 {..} ->
      plet (pfromData pparameters'responseGeometry) $ \geometry ->
        pif
          (presponseGeometryIsCanonicalV1 # geometry)
          ( pmatch geometry $ \PResponseGeometryV1 {presponseGeometry'chunkByteLength, presponseGeometry'trancheByteLength, presponseGeometry'maxTrancheCount} ->
              let chunkLength = pfromData presponseGeometry'chunkByteLength
                  trancheLength = pfromData presponseGeometry'trancheByteLength
                  fullTranches = pdiv # pfullPayloadMaxBytesV1 # trancheLength
                  remainder = pmod # pfullPayloadMaxBytesV1 # trancheLength
                  publicationsPerFullTranche = pceilDiv trancheLength chunkLength
                  maximumPublications =
                    fullTranches * publicationsPerFullTranche
                      + pif (remainder #== 0) 0 (pceilDiv remainder chunkLength)
                  daBond = pfromData pparameters'daBondLovelace
                  challengerBond = pfromData pparameters'challengerBondLovelace
                  publicationFee = pfromData pparameters'maxPublicationFeeLovelace
                  settlementFee = pfromData pparameters'maxSettlementFeeLovelace
                  closeFee = pfromData pparameters'maxCloseFeeLovelace
                  timeoutFee = pfromData pparameters'maxTimeoutFeeLovelace
                  maxTranches = pfromData presponseGeometry'maxTrancheCount
               in pand'List
                    [ 0 #< daBond
                    , challengerBond #== daBond
                    , 0 #< pfromData pparameters'maxOpenFeeLovelace
                    , 0 #< publicationFee
                    , 0 #< settlementFee
                    , 0 #< closeFee
                    , 0 #< timeoutFee
                    , maximumPublications * publicationFee + maxTranches * settlementFee + closeFee #< challengerBond
                    , maximumPublications * publicationFee + maxTranches * settlementFee + timeoutFee #< challengerBond
                    ]
          )
          (pconstant False)

pdescriptorsAreCanonicalV1 ::
  forall s.
  Term
    s
    ( PBuiltinList (PAsData PTrancheDescriptorV1)
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pdescriptorsAreCanonicalV1 = pfix $ \self -> plam $ \descriptors payloadLength trancheLength chunkLength expectedIndex expectedOffset ->
  pmatch descriptors $ \case
    PNil ->
      expectedOffset #== payloadLength
        #&& expectedIndex #== pceilDiv payloadLength trancheLength
    PCons descriptorData rest ->
      pmatch (pfromData descriptorData) $ \PTrancheDescriptorV1 {..} ->
        let remaining = payloadLength - expectedOffset
            expectedLength = pif (remaining #< trancheLength) remaining trancheLength
         in pand'List
              [ 0 #< expectedLength
              , pfromData ptrancheDescriptor'trancheIndex #== expectedIndex
              , pfromData ptrancheDescriptor'startOffset #== expectedOffset
              , pfromData ptrancheDescriptor'byteLength #== expectedLength
              , pfromData ptrancheDescriptor'chunkCount #== pceilDiv expectedLength chunkLength
              , plengthBS # pfromData ptrancheDescriptor'chunkCommitment #== 32
              , plengthBS # pfromData ptrancheDescriptor'terminalAccumulator #== 32
              , self
                  # rest
                  # payloadLength
                  # trancheLength
                  # chunkLength
                  # (expectedIndex + 1)
                  # (expectedOffset + expectedLength)
              ]

-- | Aiken @commitment_is_canonical_v1@.
pcommitmentIsCanonicalV1 ::
  forall s. Term s PCommitmentV1 -> Term s PParametersV1 -> Term s PBool
pcommitmentIsCanonicalV1 commitment authenticatedParameters =
  pmatch authenticatedParameters $ \PParametersV1 {pparameters'responseGeometry = authenticatedGeometryData} ->
    pmatch commitment $ \PCommitmentV1 {..} ->
      plet (pfromData pcommitment'responseGeometry) $ \geometry ->
        plet (pfromData pcommitment'trancheDescriptors) $ \descriptors ->
          pmatch geometry $ \PResponseGeometryV1 {..} ->
            let payloadLength = pfromData pcommitment'payloadByteLength
                trancheLength = pfromData presponseGeometry'trancheByteLength
                chunkLength = pfromData presponseGeometry'chunkByteLength
             in pif
                  (presponseWindowMsV1 payloadLength #== pcon PNothing)
                  (pconstant False)
                  ( pand'List
                      [ pfromData pcommitment'version #== pcommitmentVersionV1
                      , plengthBS # pfromData pcommitment'deploymentIdentity #== 28
                      , plengthBS # pfromData pcommitment'headerHash #== 28
                      , plengthBS # pto (pfromData pcommitment'bondOwner) #== 28
                      , pparametersAreCanonicalV1 # authenticatedParameters
                      , pcommitment'responseGeometry #== authenticatedGeometryData
                      , plength # descriptors #<= pfromData presponseGeometry'maxTrancheCount
                      , pdescriptorsAreCanonicalV1
                          # descriptors
                          # payloadLength
                          # trancheLength
                          # chunkLength
                          # 0
                          # 0
                      ]
                  )

pdescriptorPublicationCountV1 :: forall s. Term s PTrancheDescriptorV1 -> Term s PInteger -> Term s PInteger
pdescriptorPublicationCountV1 descriptor chunkByteLength =
  pmatch descriptor $ \PTrancheDescriptorV1 {ptrancheDescriptor'byteLength} ->
    pceilDiv (pfromData ptrancheDescriptor'byteLength) chunkByteLength

ptotalPublicationReserveV1 ::
  forall s.
  Term s (PBuiltinList (PAsData PTrancheDescriptorV1) :--> PInteger :--> PInteger :--> PInteger)
ptotalPublicationReserveV1 = pfix $ \self -> plam $ \descriptors chunkByteLength maximumPublicationFee ->
  pmatch descriptors $ \case
    PNil -> 0
    PCons descriptorData rest ->
      pdescriptorPublicationCountV1 (pfromData descriptorData) chunkByteLength * maximumPublicationFee
        + self # rest # chunkByteLength # maximumPublicationFee

-- | Aiken @terminal_accumulator_reserve_lovelace_v1@.
pterminalAccumulatorReserveLovelaceV1 :: forall s. Term s PParametersV1 -> Term s PInteger
pterminalAccumulatorReserveLovelaceV1 parameters =
  pmatch parameters $ \PParametersV1 {pparameters'maxCloseFeeLovelace, pparameters'maxTimeoutFeeLovelace} ->
    let closeFee = pfromData pparameters'maxCloseFeeLovelace
        timeoutFee = pfromData pparameters'maxTimeoutFeeLovelace
     in pif (closeFee #> timeoutFee) closeFee timeoutFee

pinitialTrancheLovelacesV1 ::
  forall s.
  Term
    s
    ( PBuiltinList (PAsData PTrancheDescriptorV1)
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList PInteger
    )
pinitialTrancheLovelacesV1 = pfix $ \self -> plam $ \descriptors chunkByteLength maximumPublicationFee maximumSettlementFee base remainder trancheIndex ->
  pmatch descriptors $ \case
    PNil -> pnil
    PCons descriptorData rest ->
      pcons
        # ( pdescriptorPublicationCountV1 (pfromData descriptorData) chunkByteLength * maximumPublicationFee
              + maximumSettlementFee
              + base
              + pif (trancheIndex #< remainder) 1 0
          )
        # (self # rest # chunkByteLength # maximumPublicationFee # maximumSettlementFee # base # remainder # (trancheIndex + 1))

-- | Aiken @tranche_initial_lovelaces_v1@.
ptrancheInitialLovelacesV1 ::
  forall s.
  Term s PCommitmentV1 -> Term s PParametersV1 -> Term s (PMaybe (PBuiltinList PInteger))
ptrancheInitialLovelacesV1 commitment parameters =
  pmatch commitment $ \PCommitmentV1 {pcommitment'trancheDescriptors} ->
    plet (pfromData pcommitment'trancheDescriptors) $ \descriptors ->
      plet (plength # descriptors) $ \descriptorCount ->
        pif
          (pnot # pcommitmentIsCanonicalV1 commitment parameters #|| descriptorCount #<= 0)
          (pcon PNothing)
          ( pmatch parameters $ \PParametersV1 {..} ->
              let chunkByteLength =
                    pmatch (pfromData pparameters'responseGeometry) $ \PResponseGeometryV1 {presponseGeometry'chunkByteLength} ->
                      pfromData presponseGeometry'chunkByteLength
                  maximumPublicationFee = pfromData pparameters'maxPublicationFeeLovelace
                  maximumSettlementFee = pfromData pparameters'maxSettlementFeeLovelace
                  terminalReserve = pterminalAccumulatorReserveLovelaceV1 parameters
                  totalPublicationReserve = ptotalPublicationReserveV1 # descriptors # chunkByteLength # maximumPublicationFee
                  totalSettlementReserve = descriptorCount * maximumSettlementFee
                  distributable = pfromData pparameters'challengerBondLovelace - totalPublicationReserve - totalSettlementReserve - terminalReserve
                  base = pdiv # distributable # descriptorCount
                  remainder = pmod # distributable # descriptorCount
               in pcon $
                    PJust $
                      pinitialTrancheLovelacesV1
                        # descriptors
                        # chunkByteLength
                        # maximumPublicationFee
                        # maximumSettlementFee
                        # base
                        # remainder
                        # 0
          )

plistAtMaybe ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PInteger :--> PBuiltinList a :--> PMaybe a)
plistAtMaybe = phoistAcyclic $ pfix $ \self -> plam $ \index values ->
  pif
    (index #< 0)
    (pcon PNothing)
    ( pelimList
        (\value rest -> pif (index #== 0) (pcon $ PJust value) (self # (index - 1) # rest))
        (pcon PNothing)
        values
    )

-- | Aiken @tranche_initial_lovelace_v1@.
ptrancheInitialLovelaceV1 ::
  forall s.
  Term s PCommitmentV1 -> Term s PParametersV1 -> Term s PInteger -> Term s (PMaybe PInteger)
ptrancheInitialLovelaceV1 commitment parameters trancheIndex =
  pmatch (ptrancheInitialLovelacesV1 commitment parameters) $ \case
    PNothing -> pcon PNothing
    PJust lovelaces -> plistAtMaybe # trancheIndex # lovelaces

-- | Aiken @terminal_accumulator_initial_lovelace_v1@.
pterminalAccumulatorInitialLovelaceV1 ::
  forall s.
  Term s PCommitmentV1 -> Term s PParametersV1 -> Term s (PMaybe PInteger)
pterminalAccumulatorInitialLovelaceV1 commitment parameters =
  pif
    (pcommitmentIsCanonicalV1 commitment parameters)
    (pcon $ PJust $ pterminalAccumulatorReserveLovelaceV1 parameters)
    (pcon PNothing)

phashDomainAndData :: forall s. Term s PByteString -> Term s PData -> Term s PByteString
phashDomainAndData domain value = pblake2b_256 # (domain <> (pserialiseData # value))

-- | Aiken @terminal_accumulator_start_v1@.
pterminalAccumulatorStartV1 ::
  forall s. Term s PCommitmentV1 -> Term s PTokenName -> Term s PByteString
pterminalAccumulatorStartV1 commitment challengeAssetName =
  pmatch commitment $ \PCommitmentV1 {..} ->
    phashDomainAndData
      (pconstant "MidgardDaAvailabilityTerminalStartV1")
      ( pforgetData $
          pdata $
            pcon $
              PTerminalAccumulatorStartV1
                (pdata pcommitmentVersionV1)
                pcommitment'deploymentIdentity
                pcommitment'headerHash
                (pdata challengeAssetName)
      )

-- | Aiken @fold_terminal_accumulator_v1@.
pfoldTerminalAccumulatorV1 ::
  forall s.
  Term s PByteString -> Term s PInteger -> Term s PTrancheTerminalStatusV1 -> Term s PByteString
pfoldTerminalAccumulatorV1 previousAccumulator trancheIndex status =
  phashDomainAndData
    (pconstant "MidgardDaAvailabilityTerminalStepV1")
    ( pforgetData $
        pdata $
          pcon $
            PTerminalAccumulatorStepV1
              (pdata pcommitmentVersionV1)
              (pdata previousAccumulator)
              (pdata trancheIndex)
              (pdata status)
    )

-- | Aiken @attestation_message_v1@.
pattestationMessageV1 :: forall s. Term s PCommitmentV1 -> Term s PByteString
pattestationMessageV1 commitment =
  phashDomainAndData (pconstant "MidgardDaAvailabilityAttestationV1") (pforgetData $ pdata commitment)

-- | Aiken @published_terminal_commitment_v1@.
ppublishedTerminalCommitmentV1 :: forall s. Term s PCommitmentV1 -> Term s PByteString
ppublishedTerminalCommitmentV1 commitment =
  phashDomainAndData (pconstant "MidgardDaAvailabilityPublishedV1") (pforgetData $ pdata commitment)

-- | Aiken @tranche_start_accumulator_v1@.
ptrancheStartAccumulatorV1 ::
  forall s.
  Term s PByteString -> Term s PHeaderHash -> Term s PTrancheDescriptorV1 -> Term s PByteString
ptrancheStartAccumulatorV1 deploymentIdentity headerHash descriptor =
  pmatch descriptor $ \PTrancheDescriptorV1 {..} ->
    phashDomainAndData
      (pconstant "MidgardDaAvailabilityTrancheStartV1")
      ( pforgetData $
          pdata $
            pcon $
              PTrancheStartV1
                (pdata pcommitmentVersionV1)
                (pdata deploymentIdentity)
                (pdata headerHash)
                ptrancheDescriptor'trancheIndex
                ptrancheDescriptor'startOffset
                ptrancheDescriptor'byteLength
      )

-- | Aiken @tranche_step_accumulator_v1@.
ptrancheStepAccumulatorV1 ::
  forall s.
  Term s PByteString -> Term s PHeaderHash -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PByteString
ptrancheStepAccumulatorV1 deploymentIdentity headerHash trancheIndex chunkOffset chunk previousAccumulator =
  phashDomainAndData
    (pconstant "MidgardDaAvailabilityTrancheStepV1")
    ( pforgetData $
        pdata $
          pcon $
            PTrancheStepV1
              (pdata pcommitmentVersionV1)
              (pdata deploymentIdentity)
              (pdata headerHash)
              (pdata trancheIndex)
              (pdata chunkOffset)
              (pdata $ plengthBS # chunk)
              (pdata $ pblake2b_256 # chunk)
              (pdata previousAccumulator)
    )

-- | Aiken @chunk_leaf_hash_v1@.
pchunkLeafHashV1 ::
  forall s.
  Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PByteString
pchunkLeafHashV1 trancheIndex chunkIndex chunkOffset chunkByteLength chunkHash =
  phashDomainAndData
    (pconstant "MidgardDaAvailabilityChunkLeafV1")
    ( pforgetData $
        pdata $
          pcon $
            PChunkLeafV1
              (pdata pcommitmentVersionV1)
              (pdata trancheIndex)
              (pdata chunkIndex)
              (pdata chunkOffset)
              (pdata chunkByteLength)
              (pdata chunkHash)
    )

ppublicationChunkIsCommittedV1 ::
  forall s. Term s PTrancheDescriptorV1 -> Term s PPublicationDatumV1 -> Term s PBool
ppublicationChunkIsCommittedV1 descriptor publication =
  pmatch descriptor $ \PTrancheDescriptorV1 {..} ->
    pmatch publication $ \PPublicationDatumV1 {..} ->
      let chunkCount = pfromData ptrancheDescriptor'chunkCount
          chunkIndex = pfromData ppublication'chunkIndex
          frontier = pfromData ppublication'chunkFrontier
          leafHash =
            pchunkLeafHashV1
              (pfromData ptrancheDescriptor'trancheIndex)
              chunkIndex
              (pfromData ppublication'chunkOffset)
              (pfromData ppublication'chunkByteLength)
              (pfromData ppublication'chunkHash)
       in chunkCount #> 0
            #&& chunkIndex #>= 0
            #&& chunkIndex #< chunkCount
            #&& ValidationMerkle.pfrontierIsWellFormed # chunkCount # frontier
            #&& ValidationMerkle.pfrontierCommitment # chunkCount # frontier
              #== pfromData ptrancheDescriptor'chunkCommitment
            #&& ValidationMerkle.pverifyMembership
              # chunkCount
              # frontier
              # chunkIndex
              # leafHash
              # pfromData ppublication'chunkSiblings

-- | Aiken @publication_advances_active_tranche_v1@.
ppublicationAdvancesActiveTrancheV1 ::
  forall s.
  Term s PTrancheDatumV1 -> Term s PPublicationDatumV1 -> Term s PInteger -> Term s PInteger -> Term s (PMaybe PTrancheDatumV1)
ppublicationAdvancesActiveTrancheV1 active publication chunkByteLength carrierOutputIndex =
  pmatch active $ \case
    PReceipt _ _ _ _ _ _ _ -> pcon PNothing
    PActiveTranche deploymentIdentityData headerHashData challengeAssetNameData descriptorData nextOffsetData accumulatorData _ responseDeadlineData challengerData ->
      plet (pfromData descriptorData) $ \descriptor ->
        pmatch descriptor $ \PTrancheDescriptorV1 {..} ->
          plet (pfromData nextOffsetData) $ \nextOffset ->
            plet (pfromData accumulatorData) $ \accumulator ->
              plet (pfromData deploymentIdentityData) $ \deploymentIdentity ->
                plet (pfromData headerHashData) $ \headerHash ->
                  plet (pfromData challengeAssetNameData) $ \challengeAssetName ->
                    plet (pfromData ptrancheDescriptor'startOffset + pfromData ptrancheDescriptor'byteLength) $ \endOffset ->
                      plet (endOffset - nextOffset) $ \remaining ->
                        plet (pif (remaining #< chunkByteLength) remaining chunkByteLength) $ \expectedChunkLength ->
                          pmatch publication $ \PPublicationDatumV1 {..} ->
                            plet
                              ( ptrancheStepAccumulatorV1
                                  deploymentIdentity
                                  headerHash
                                  (pfromData ptrancheDescriptor'trancheIndex)
                                  nextOffset
                                  (pfromData ppublication'chunk)
                                  accumulator
                              )
                              $ \nextAccumulator ->
                                plet
                                  (pdiv # (nextOffset - pfromData ptrancheDescriptor'startOffset) # chunkByteLength)
                                  $ \expectedChunkIndex ->
                                    pif
                                      ( pand'List
                                          [ expectedChunkLength #> 0
                                          , pfromData ppublication'deploymentIdentity #== deploymentIdentity
                                          , pfromData ppublication'headerHash #== headerHash
                                          , pfromData ppublication'challengeAssetName #== challengeAssetName
                                          , pfromData ppublication'trancheIndex #== pfromData ptrancheDescriptor'trancheIndex
                                          , pfromData ppublication'chunkIndex #== expectedChunkIndex
                                          , pfromData ppublication'chunkOffset #== nextOffset
                                          , pfromData ppublication'chunkByteLength #== expectedChunkLength
                                          , plengthBS # pfromData ppublication'chunk #== expectedChunkLength
                                          , pfromData ppublication'chunkHash #== pblake2b_256 # pfromData ppublication'chunk
                                          , pfromData ppublication'previousAccumulator #== accumulator
                                          , pfromData ppublication'nextAccumulator #== nextAccumulator
                                          , ppublicationChunkIsCommittedV1 descriptor publication
                                          , carrierOutputIndex #>= 0
                                          ]
                                      )
                                      ( plet (nextOffset + expectedChunkLength) $ \advancedOffset ->
                                          pif
                                            (advancedOffset #== endOffset)
                                            ( pif
                                                (nextAccumulator #== pfromData ptrancheDescriptor'terminalAccumulator)
                                                ( pcon $
                                                    PJust $
                                                      pcon $
                                                        PReceipt
                                                          deploymentIdentityData
                                                          headerHashData
                                                          challengeAssetNameData
                                                          descriptorData
                                                          (pdata nextAccumulator)
                                                          (pdata carrierOutputIndex)
                                                          challengerData
                                                )
                                                (pcon PNothing)
                                            )
                                            ( pcon $
                                                PJust $
                                                  pcon $
                                                    PActiveTranche
                                                      deploymentIdentityData
                                                      headerHashData
                                                      challengeAssetNameData
                                                      descriptorData
                                                      (pdata advancedOffset)
                                                      (pdata nextAccumulator)
                                                      (pdata $ pcon $ PDJust $ pdata carrierOutputIndex)
                                                      responseDeadlineData
                                                      challengerData
                                            )
                                      )
                                      (pcon PNothing)
