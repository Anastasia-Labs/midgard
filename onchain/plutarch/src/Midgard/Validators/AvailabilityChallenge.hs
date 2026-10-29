{- |
Module      : Midgard.Validators.AvailabilityChallenge
Description : Plutarch port of @validators/availability-challenge.ak@.

The spending side is kept separately callable while the five minting branches
are ported in bounded slices. Both handlers are ultimately dispatched by one
multi-purpose validator and therefore share a script hash on-chain.
-}
module Midgard.Validators.AvailabilityChallenge (
  availabilityChallengeMintValidator,
  availabilityChallengeSpendValidator,
  availabilityChallengeValidator,
  availabilityChallengeTimeoutYieldValidator,
  availabilityChallengeCloseYieldValidator,
  availabilityChallengeSettleYieldValidator,
  availabilityChallengeOpenYieldValidator,
  availabilityChallengeBondYieldValidator,
  pvalidateAdvanceTrancheV1,
  pvalidateConsumeCarrierV1,
  pvalidateCoordinateSpendV1,
  pvalidateInitialTerminalAccumulatorOutputV1,
  pvalidateInitialTrancheOutputsV1,
  pvalidateCloseChallengeV1,
  pvalidateMintBondFromAttestationV1,
  pvalidateOpenChallengeV1,
  ptimeoutCarrierLovelaceV1,
  pvalidateSettleTrancheV1,
  pvalidateTimeoutChallengeV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  PMintValue,
  POutputDatum (..),
  PPubKeyHash,
  PRedeemer,
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef (..),
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.StateQueueYield qualified as Yield
import Midgard.AvailabilityChallenge
import Midgard.Common.Utils (
  pgetInclusiveBoundsOfAShortValidityRange,
  pgetInclusiveLowerBoundOfInterval,
  pgetRedeemerAt,
  phasSigned,
 )
import Midgard.DaAttestation qualified as Da
import Midgard.DaAttestation.Signatures qualified as Da
import Midgard.HubOracle qualified as Hub
import Midgard.StateQueue qualified as StateQueue

data PSettlementStatusV1 (s :: S) = PSettlementStatusV1
  { psettlementStatus'status :: Term s PTrancheTerminalStatusV1
  , psettlementStatus'timedOut :: Term s PBool
  , psettlementStatus'carrierLovelace :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PSettlementStatusV1)

punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer redeemer = punsafeCoerce (pto redeemer)

punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer redeemer = punsafeCoerce (pto (pfromData redeemer))

pinputAt ::
  forall s. Term s (PBuiltinList (PAsData PTxInInfo)) -> Term s PInteger -> Term s PTxInInfo
pinputAt inputs index = pfromData $ pelemAt # index # inputs

poutputAt ::
  forall s. Term s (PBuiltinList (PAsData PTxOut)) -> Term s PInteger -> Term s PTxOut
poutputAt outputs index = pfromData $ pelemAt # index # outputs

pinlineDatum :: forall (a :: S -> Type) s. PIsData a => Term s PTxOut -> Term s a
pinlineDatum output =
  pmatch output $ \PTxOut {ptxOut'datum} ->
    pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} ->
        pfromData $ punsafeCoerce @(PAsData a) $ pto poutputDatum'outputDatum
      _ -> perror

pscriptAddress :: forall s. Term s PCurrencySymbol -> Term s PAddress
pscriptAddress policyId =
  pcon $
    PAddress
      (pcon $ PScriptCredential $ punsafeCoerce @(PAsData PScriptHash) $ pdata policyId)
      (pcon PDNothing)

pscriptHash :: forall s. Term s PAddress -> Term s PCurrencySymbol
pscriptHash address =
  pmatch address $ \PAddress {paddress'credential} ->
    pmatch paddress'credential $ \case
      PScriptCredential hash -> punsafeCoerce @PCurrencySymbol $ pfromData hash
      PPubKeyCredential _ -> perror

plovelaceOf :: forall s. Term s (PAsData Value.PLedgerValue) -> Term s PInteger
plovelaceOf value =
  Value.pvalueOf
    # pto (pfromData value)
    # Value.padaSymbol
    # Value.padaToken

pvalueHasOnlyAdaAndAsset ::
  forall s.
  Term s (PAsData Value.PLedgerValue) ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s PBool
pvalueHasOnlyAdaAndAsset value policyId assetName quantity =
  pto (pfromData value)
    #== ( Value.psingletonSortedValue
            # Value.padaSymbol
            # Value.padaToken
            # plovelaceOf value
            <> (Value.psingletonSortedValue # policyId # assetName # quantity)
        )

pvalueIsAdaOnly :: forall s. Term s (PAsData Value.PLedgerValue) -> Term s PBool
pvalueIsAdaOnly value =
  pto (pfromData value)
    #== Value.psingletonSortedValue
      # Value.padaSymbol
      # Value.padaToken
      # plovelaceOf value

pnoReferenceScript :: forall s. Term s PTxOut -> Term s PBool
pnoReferenceScript output =
  pmatch output $ \PTxOut {ptxOut'referenceScript} ->
    pmatch ptxOut'referenceScript $ \case
      PDNothing -> pconstant True
      PDJust _ -> pconstant False

pexactScriptOutput ::
  forall s.
  Term s PTxOut -> Term s PAddress -> Term s PBool -> Term s PBool
pexactScriptOutput output expectedAddress valueIsExact =
  pmatch output $ \PTxOut {ptxOut'address} ->
    pand'List
      [ ptxOut'address #== expectedAddress
      , valueIsExact
      , pnoReferenceScript output
      ]

pfindOwnInput ::
  forall s. Term s (PBuiltinList (PAsData PTxInInfo)) -> Term s PTxOutRef -> Term s PTxInInfo
pfindOwnInput inputs ownRef =
  pmatch
    ( pfind
        # plam
          ( \input ->
              pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef} ->
                ptxInInfo'outRef #== ownRef
          )
        # inputs
    )
    $ \case
      PJust input -> pfromData input
      PNothing -> perror

pmintIsZero :: forall s. Term s (PAsData PMintValue) -> Term s PBool
pmintIsZero mint = pnull # pto (pto (pto (pto (pfromData mint))))

pmintPairsExactly ::
  forall s.
  Term s (PAsData PMintValue) ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s PBool
pmintPairsExactly mint policyId assetName quantity =
  pmatch (AssocMap.plookup # policyId # pto (pto (pfromData mint))) $ \case
    PNothing -> pconstant False
    PJust tokens ->
      let entries = pto (pto tokens)
       in pif
            (pnull # entries)
            (pconstant False)
            ( pmatch (phead # entries) $ \(PBuiltinPair mintedName mintedQuantity) ->
                pnull # (ptail # entries)
                  #&& mintedName #== pdata assetName
                  #&& pfromData mintedQuantity #== quantity
            )

pvalidateDaApplyBindingV1 ::
  forall s.
  Term s PTxInfo ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pvalidateDaApplyBindingV1 tx daPolicyId daMintRedeemerIndex daAttestationInputIndex stateQueueInputIndex stateQueueOutputIndex = P.do
  PTxInfo {ptxInfo'redeemers} <- pmatch tx
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  daRedeemer <-
    plet $
      pfromData $
        punsafeCoerceRedeemer @Da.PMintRedeemer $
          pgetRedeemerAt
            # redeemers
            # pdata (pcon $ PMinting $ pdata daPolicyId)
            # daMintRedeemerIndex
  pmatch daRedeemer $ \case
    Da.PApplyToStateQueue
      { Da.papply'daAttestationInputIndex
      , Da.papply'stateQueueInputIndex
      , Da.papply'stateQueueOutputIndex
      } ->
        pand'List
          [ pfromData papply'daAttestationInputIndex #== daAttestationInputIndex
          , pfromData papply'stateQueueInputIndex #== stateQueueInputIndex
          , pfromData papply'stateQueueOutputIndex #== stateQueueOutputIndex
          ]
    _ -> perror

pvalidateStateQueueStatusTransitionV1 ::
  forall s.
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PStateQueueStatusV1 ->
  Term s PStateQueueStatusV1 ->
  Term s PBool
pvalidateStateQueueStatusTransitionV1 inputs outputs stateQueuePolicyId stateQueueInputIndex stateQueueOutputIndex headerHash previousStatus nextStatus = P.do
  stateQueueInput <- plet $ pinputAt inputs stateQueueInputIndex
  PTxInInfo {ptxInInfo'resolved = stateQueueInputOutput} <- pmatch stateQueueInput
  stateQueueOutput <- plet $ poutputAt outputs stateQueueOutputIndex
  let inputValue = pmatch stateQueueInputOutput $ \PTxOut {ptxOut'value} -> ptxOut'value
      outputValue = pmatch stateQueueOutput $ \PTxOut {ptxOut'value} -> ptxOut'value
  pto (pfromData inputValue) #== pto (pfromData outputValue)
    #&& StateQueue.pvalidateDaAvailabilityStatusTransition
      inputs
      outputs
      stateQueuePolicyId
      stateQueueInputIndex
      stateQueueOutputIndex
      headerHash
      previousStatus
      nextStatus

-- | Aiken @validate_mint_bond_from_attestation@.
pvalidateMintBondFromAttestationV1 ::
  forall s.
  Term s PCurrencySymbol ->
  Term s PParametersV1 ->
  Term s PCurrencySymbol ->
  Term s PTxInfo ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pvalidateMintBondFromAttestationV1 hubOraclePolicyId parameters ownPolicyId tx hubRefInputIndex daAttestationInputIndex daAttestationMintRedeemerIndex bondOutputIndex stateQueueInputIndex stateQueueOutputIndex = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'mint} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  outputs <- plet $ pfromData ptxInfo'outputs
  hubDatum <-
    plet $
      Hub.pgetDatum
        # referenceInputs
        # punsafeCoerce @(PAsData PScriptHash) (pdata hubOraclePolicyId)
        # hubRefInputIndex
  stateQueuePolicyId <-
    plet $ pmatch hubDatum $ \Hub.PHubOracleDatum {Hub.phubOracle'stateQueue} -> phubOracle'stateQueue
  attestationInput <- plet $ pinputAt inputs daAttestationInputIndex
  PTxInInfo {ptxInInfo'outRef = attestationInputRef, ptxInInfo'resolved = attestationOutput} <- pmatch attestationInput
  PTxOut {ptxOut'address = attestationAddress, ptxOut'value = attestationValue} <- pmatch attestationOutput
  daPolicyId <- plet $ pscriptHash attestationAddress
  attestationDatum <- plet $ pinlineDatum @Da.PDaAttestationDatum attestationOutput
  Da.PDaAttestationDatum
    { Da.pdaAttestation'headerHash
    , Da.pdaAttestation'availabilityCommitment
    , Da.pdaAttestation'committeeSignersHash
    , Da.pdaAttestation'attestedSigners
    } <-
    pmatch attestationDatum
  commitment <- plet $ pfromData pdaAttestation'availabilityCommitment
  daAssetName <- plet $ Da.pattestationAssetName # pfromData pdaAttestation'headerHash
  bondAssetName <- plet $ pdaBondAssetNameV1 attestationInputRef
  bondOutput <- plet $ poutputAt outputs bondOutputIndex
  expectedBondDatum <-
    plet $
      pcon $
        PAvailable
          pdaAttestation'availabilityCommitment
          (pdata bondAssetName)
          pdaAttestation'committeeSignersHash
          pdaAttestation'attestedSigners
  let daBondLovelace = pmatch parameters $ \PParametersV1 {pparameters'daBondLovelace} -> pfromData pparameters'daBondLovelace
      expectedAttestationValue =
        Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # daBondLovelace
          <> (Value.psingletonSortedValue # daPolicyId # pfromData daAssetName # 1)
      expectedBondValue =
        Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # daBondLovelace
          <> (Value.psingletonSortedValue # ownPolicyId # bondAssetName # 1)
  pand'List
    [ pnot # (daAttestationInputIndex #== stateQueueInputIndex)
    , pnot # (bondOutputIndex #== stateQueueOutputIndex)
    , pparametersAreCanonicalV1 # parameters
    , pcommitmentIsCanonicalV1 commitment parameters
    , pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity} ->
        pfromData pcommitment'deploymentIdentity #== pto hubOraclePolicyId
    , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
        pcommitment'headerHash #== pdaAttestation'headerHash
    , pto (pfromData attestationValue) #== expectedAttestationValue
    , pmintPairsExactly ptxInfo'mint ownPolicyId bondAssetName 1
    , pmatch bondOutput $ \PTxOut {ptxOut'value} ->
        pexactScriptOutput bondOutput (pscriptAddress ownPolicyId) (pto (pfromData ptxOut'value) #== expectedBondValue)
    , pinlineDatum @PBondDatumV1 bondOutput #== expectedBondDatum
    , pvalidateDaApplyBindingV1
        tx
        daPolicyId
        daAttestationMintRedeemerIndex
        daAttestationInputIndex
        stateQueueInputIndex
        stateQueueOutputIndex
    , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
        pvalidateStateQueueStatusTransitionV1
          inputs
          outputs
          stateQueuePolicyId
          stateQueueInputIndex
          stateQueueOutputIndex
          (pfromData pcommitment'headerHash)
          (pcon PUnattested)
          (pcon $ PAttested $ pdata bondAssetName)
    ]

pdescriptorAt ::
  forall s.
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTrancheDescriptorV1)) ->
  Term s (PMaybe PTrancheDescriptorV1)
pdescriptorAt index descriptors =
  pif
    (index #< 0)
    (pcon PNothing)
    ( (pfix $ \self -> plam $ \remaining values ->
          pelimList
            (\value rest -> pif (remaining #== 0) (pcon $ PJust $ pfromData value) (self # (remaining - 1) # rest))
            (pcon PNothing)
            values
      )
        # index
        # descriptors
    )

-- | Aiken @timeout_carrier_lovelace@.
ptimeoutCarrierLovelaceV1 ::
  forall s.
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PCurrencySymbol ->
  Term s PCommitmentV1 ->
  Term s PTokenName ->
  Term s PTrancheDescriptorV1 ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PMaybeData PInteger) ->
  Term s (PMaybe PInteger)
ptimeoutCarrierLovelaceV1 inputs ownPolicyId commitment challengeAssetName descriptor expectedAccumulator expectedNextOffset carrierInputIndex =
  pmatch descriptor $ \PTrancheDescriptorV1 {ptrancheDescriptor'trancheIndex, ptrancheDescriptor'startOffset} ->
    pmatch carrierInputIndex $ \case
      PDNothing ->
        pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity, pcommitment'headerHash} ->
          pif
            ( expectedNextOffset #== pfromData ptrancheDescriptor'startOffset
                #&& expectedAccumulator
                  #== ptrancheStartAccumulatorV1
                    (pfromData pcommitment'deploymentIdentity)
                    (pfromData pcommitment'headerHash)
                    descriptor
            )
            (pcon $ PJust 0)
            (pcon PNothing)
      PDJust carrierInputIndexData -> P.do
        carrierInput <- plet $ pinputAt inputs (pfromData carrierInputIndexData)
        PTxInInfo {ptxInInfo'resolved = carrierOutput} <- pmatch carrierInput
        publication <- plet $ pinlineDatum @PPublicationDatumV1 carrierOutput
        PTxOut {ptxOut'address, ptxOut'value} <- pmatch carrierOutput
        pif
          ( pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity, pcommitment'headerHash} ->
              pmatch publication $ \PPublicationDatumV1 {..} ->
                pand'List
                  [ ptxOut'address #== pscriptAddress ownPolicyId
                  , pnoReferenceScript carrierOutput
                  , pvalueIsAdaOnly ptxOut'value
                  , pfromData ppublication'deploymentIdentity #== pfromData pcommitment'deploymentIdentity
                  , pfromData ppublication'headerHash #== pfromData pcommitment'headerHash
                  , pfromData ppublication'challengeAssetName #== challengeAssetName
                  , pfromData ppublication'trancheIndex #== pfromData ptrancheDescriptor'trancheIndex
                  , pfromData ppublication'nextAccumulator #== expectedAccumulator
                  , pfromData ppublication'chunkOffset + pfromData ppublication'chunkByteLength #== expectedNextOffset
                  ]
          )
          (pcon $ PJust $ plovelaceOf ptxOut'value)
          (pcon PNothing)

pvalidateSettlementStatusV1 ::
  forall s.
  Term s PTxInfo ->
  Term s PCurrencySymbol ->
  Term s PCommitmentV1 ->
  Term s PTokenName ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s PTrancheDescriptorV1 ->
  Term s PTxOutRef ->
  Term s PTrancheDatumV1 ->
  Term s (PMaybeData PInteger) ->
  Term s (PMaybe PSettlementStatusV1)
pvalidateSettlementStatusV1 tx ownPolicyId commitment challengeAssetName challenger responseDeadline descriptor threadInputReference threadDatum carrierInputIndex =
  pmatch tx $ \PTxInfo {ptxInfo'inputs, ptxInfo'validRange} ->
    plet (pfromData ptxInfo'inputs) $ \inputs ->
      pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity, pcommitment'headerHash} ->
        pmatch descriptor $ \PTrancheDescriptorV1 {ptrancheDescriptor'startOffset, ptrancheDescriptor'byteLength, ptrancheDescriptor'terminalAccumulator} ->
          pmatch threadDatum $ \case
            PReceipt deploymentIdentity headerHash threadChallengeAssetName threadDescriptor terminalAccumulator terminalCarrierOutputIndex threadChallenger ->
              pmatch carrierInputIndex $ \case
                PDNothing -> pcon PNothing
                PDJust carrierIndexData -> P.do
                  carrierInput <- plet $ pinputAt inputs (pfromData carrierIndexData)
                  carrierLovelace <-
                    plet $
                      pmatch
                        ( ptimeoutCarrierLovelaceV1
                            inputs
                            ownPolicyId
                            commitment
                            challengeAssetName
                            descriptor
                            (pfromData terminalAccumulator)
                            (pfromData ptrancheDescriptor'startOffset + pfromData ptrancheDescriptor'byteLength)
                            carrierInputIndex
                        )
                        $ \case
                          PJust amount -> amount
                          PNothing -> perror
                  PTxInInfo {ptxInInfo'outRef = carrierRef} <- pmatch carrierInput
                  refsMatch <-
                    plet $
                      pmatch carrierRef $ \PTxOutRef {ptxOutRef'id = carrierTxId, ptxOutRef'idx = carrierOutputIndex} ->
                        pmatch threadInputReference $ \PTxOutRef {ptxOutRef'id = threadTxId} ->
                          carrierTxId #== threadTxId
                            #&& pfromData carrierOutputIndex #== pfromData terminalCarrierOutputIndex
                  pif
                    ( pand'List
                        [ pfromData deploymentIdentity #== pfromData pcommitment'deploymentIdentity
                        , pfromData headerHash #== pfromData pcommitment'headerHash
                        , pfromData threadChallengeAssetName #== challengeAssetName
                        , pfromData threadDescriptor #== descriptor
                        , pfromData terminalAccumulator #== pfromData ptrancheDescriptor'terminalAccumulator
                        , refsMatch
                        , threadChallenger #== challenger
                        ]
                    )
                    ( pcon $
                        PJust $
                          pcon $
                            PSettlementStatusV1
                              (pcon $ PPublishedTranche terminalAccumulator)
                              (pconstant False)
                              carrierLovelace
                    )
                    (pcon PNothing)
            PActiveTranche deploymentIdentity headerHash threadChallengeAssetName threadDescriptor nextOffset accumulator latestCarrierOutputIndex threadResponseDeadline threadChallenger -> P.do
              carrierReferenceIsValid <-
                plet $
                  pmatch (pfromData latestCarrierOutputIndex) $ \case
                    PDNothing ->
                      pmatch carrierInputIndex $ \case PDNothing -> pconstant True; PDJust _ -> pconstant False
                    PDJust expectedOutputIndex ->
                      pmatch carrierInputIndex $ \case
                        PDNothing -> perror
                        PDJust carrierIndexData ->
                          pmatch (pinputAt inputs $ pfromData carrierIndexData) $ \PTxInInfo {ptxInInfo'outRef} ->
                            pmatch ptxInInfo'outRef $ \PTxOutRef {ptxOutRef'id = carrierTxId, ptxOutRef'idx = carrierOutputIndex} ->
                              pmatch threadInputReference $ \PTxOutRef {ptxOutRef'id = threadTxId} ->
                                carrierTxId #== threadTxId
                                  #&& carrierOutputIndex #== expectedOutputIndex
              carrierLovelace <-
                plet $
                  pmatch
                    ( ptimeoutCarrierLovelaceV1
                        inputs
                        ownPolicyId
                        commitment
                        challengeAssetName
                        descriptor
                        (pfromData accumulator)
                        (pfromData nextOffset)
                        carrierInputIndex
                    )
                    $ \case
                      PJust amount -> amount
                      PNothing -> perror
              pif
                ( pand'List
                    [ pgetInclusiveLowerBoundOfInterval # ptxInfo'validRange #>= responseDeadline
                    , pfromData deploymentIdentity #== pfromData pcommitment'deploymentIdentity
                    , pfromData headerHash #== pfromData pcommitment'headerHash
                    , pfromData threadChallengeAssetName #== challengeAssetName
                    , pfromData threadDescriptor #== descriptor
                    , pfromData threadResponseDeadline #== responseDeadline
                    , threadChallenger #== challenger
                    , carrierReferenceIsValid
                    ]
                )
                ( pcon $
                    PJust $
                      pcon $
                        PSettlementStatusV1
                          (pcon $ PTimedOutTranche nextOffset accumulator)
                          (pconstant True)
                          carrierLovelace
                )
                (pcon PNothing)

-- | Aiken @validate_settle_tranche@.
pvalidateSettleTrancheV1 ::
  forall s.
  Term s PCurrencySymbol ->
  Term s PParametersV1 ->
  Term s PCurrencySymbol ->
  Term s PTxInfo ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybeData PInteger) ->
  Term s PBool
pvalidateSettleTrancheV1 hubOraclePolicyId parameters ownPolicyId tx bondRefInputIndex terminalInputIndex terminalOutputIndex trancheInputIndex carrierInputIndex = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'fee, ptxInfo'mint} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  outputs <- plet $ pfromData ptxInfo'outputs
  bondRefInput <- plet $ pinputAt referenceInputs bondRefInputIndex
  PTxInInfo {ptxInInfo'resolved = bondRefOutput} <- pmatch bondRefInput
  bondDatum <- plet $ pinlineDatum @PBondDatumV1 bondRefOutput
  PTxOut {ptxOut'address = bondAddress, ptxOut'value = bondValue} <- pmatch bondRefOutput
  terminalInput <- plet $ pinputAt inputs terminalInputIndex
  PTxInInfo {ptxInInfo'resolved = terminalInputOutput} <- pmatch terminalInput
  terminalDatum <- plet $ pinlineDatum @PTerminalAccumulatorDatumV1 terminalInputOutput
  trancheInput <- plet $ pinputAt inputs trancheInputIndex
  PTxInInfo {ptxInInfo'outRef = trancheInputRef, ptxInInfo'resolved = trancheInputOutput} <- pmatch trancheInput
  trancheDatum <- plet $ pinlineDatum @PTrancheDatumV1 trancheInputOutput
  output <- plet $ poutputAt outputs terminalOutputIndex
  pmatch bondDatum $ \case
    PAvailable _ _ _ _ -> perror
    PChallengedBond {..} -> P.do
      commitment <- plet $ pfromData pbond'commitment
      challengeAssetName <- plet $ pfromData pbond'challengeAssetName
      terminalAssetName <- plet $ pterminalAccumulatorAssetNameV1 challengeAssetName
      pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity, pcommitment'headerHash, pcommitment'trancheDescriptors} ->
        pmatch terminalDatum $ \PTerminalAccumulatorDatumV1 {..} -> P.do
          descriptorIndex <- plet $ pfromData pterminal'nextTrancheIndex
          descriptor <-
            plet $
              pmatch (pdescriptorAt descriptorIndex $ pfromData pcommitment'trancheDescriptors) $ \case
                PJust value -> value
                PNothing -> perror
          PTrancheDescriptorV1 {ptrancheDescriptor'trancheIndex} <- pmatch descriptor
          settlement <-
            plet $
              pmatch
                ( pvalidateSettlementStatusV1
                    tx
                    ownPolicyId
                    commitment
                    challengeAssetName
                    pbond'challenger
                    (pfromData pbond'responseDeadline)
                    descriptor
                    trancheInputRef
                    trancheDatum
                    carrierInputIndex
                )
                $ \case
                  PJust value -> value
                  PNothing -> perror
          PSettlementStatusV1 status timedOut carrierLovelace <- pmatch settlement
          trancheAssetName <- plet $ ptrancheAssetNameV1 challengeAssetName descriptorIndex
          PTxOut {ptxOut'address = terminalInputAddress, ptxOut'value = terminalInputValue} <- pmatch terminalInputOutput
          PTxOut {ptxOut'address = trancheInputAddress, ptxOut'value = trancheInputValue} <- pmatch trancheInputOutput
          let fee = pto (pfromData ptxInfo'fee)
              remainingLovelace = pfromData pterminal'remainingChallengerLovelace
              outputLovelace = remainingLovelace + plovelaceOf trancheInputValue + carrierLovelace - fee
              expectedAddress = pscriptAddress ownPolicyId
              expectedTerminalValue =
                Value.psingletonSortedValue
                  # Value.padaSymbol
                  # Value.padaToken
                  # outputLovelace
                  <> (Value.psingletonSortedValue # ownPolicyId # terminalAssetName # 1)
              expectedBondValue =
                Value.psingletonSortedValue
                  # Value.padaSymbol
                  # Value.padaToken
                  # (pmatch parameters $ \PParametersV1 {pparameters'daBondLovelace} -> pfromData pparameters'daBondLovelace)
                  <> (Value.psingletonSortedValue # ownPolicyId # pfromData pbond'daBondAssetName # 1)
                  <> (Value.psingletonSortedValue # ownPolicyId # challengeAssetName # 1)
              expectedInputTerminalValue =
                Value.psingletonSortedValue
                  # Value.padaSymbol
                  # Value.padaToken
                  # remainingLovelace
                  <> (Value.psingletonSortedValue # ownPolicyId # terminalAssetName # 1)
              expectedOutputDatum =
                pcon $
                  PTerminalAccumulatorDatumV1
                    pterminal'deploymentIdentity
                    pterminal'headerHash
                    pterminal'challengeAssetName
                    (pdata $ descriptorIndex + 1)
                    (pdata $ pfoldTerminalAccumulatorV1 (pfromData pterminal'foldedTerminalAccumulator) descriptorIndex status)
                    (pdata $ pfromData pterminal'hasTimedOutTranche #|| timedOut)
                    pterminal'responseDeadline
                    pterminal'challenger
                    (pdata outputLovelace)
              carrierDoesNotAlias =
                pmatch carrierInputIndex $ \case
                  PDNothing -> pconstant True
                  PDJust index ->
                    pnot # (pfromData index #== terminalInputIndex)
                      #&& pnot # (pfromData index #== trancheInputIndex)
              expectedInputCount =
                pmatch carrierInputIndex $ \case
                  PDNothing -> 2
                  PDJust _ -> 3
              maximumSettlementFee =
                pmatch parameters $ \PParametersV1 {pparameters'maxSettlementFeeLovelace} ->
                  pfromData pparameters'maxSettlementFeeLovelace
              outputValue = pmatch output $ \PTxOut {ptxOut'value} -> ptxOut'value
          pand'List
            [ pcommitmentIsCanonicalV1 commitment parameters
            , pfromData pcommitment'deploymentIdentity #== pto hubOraclePolicyId
            , pfromData ptrancheDescriptor'trancheIndex #== descriptorIndex
            , descriptorIndex #>= 0
            , descriptorIndex #< plength # pfromData pcommitment'trancheDescriptors
            , pnot # (terminalInputIndex #== trancheInputIndex)
            , carrierDoesNotAlias
            , plength # inputs #== expectedInputCount
            , plength # outputs #== 1
            , fee #> 0
            , fee #<= maximumSettlementFee
            , outputLovelace #> 0
            , bondAddress #== expectedAddress
            , pto (pfromData bondValue) #== expectedBondValue
            , terminalInputAddress #== expectedAddress
            , pnoReferenceScript terminalInputOutput
            , pto (pfromData terminalInputValue) #== expectedInputTerminalValue
            , pfromData pterminal'deploymentIdentity #== pfromData pcommitment'deploymentIdentity
            , pfromData pterminal'headerHash #== pfromData pcommitment'headerHash
            , pfromData pterminal'challengeAssetName #== challengeAssetName
            , pfromData pterminal'responseDeadline #== pfromData pbond'responseDeadline
            , pterminal'challenger #== pbond'challenger
            , plengthBS # pfromData pterminal'foldedTerminalAccumulator #== 32
            , trancheInputAddress #== expectedAddress
            , pnoReferenceScript trancheInputOutput
            , pvalueHasOnlyAdaAndAsset trancheInputValue ownPolicyId trancheAssetName 1
            , pmintPairsExactly ptxInfo'mint ownPolicyId trancheAssetName (-1)
            , pexactScriptOutput output expectedAddress (pto (pfromData outputValue) #== expectedTerminalValue)
            , pinlineDatum @PTerminalAccumulatorDatumV1 output #== expectedOutputDatum
            ]

-- | Aiken @validate_initial_tranche_outputs@.
pvalidateInitialTrancheOutputsV1 ::
  forall s.
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PCurrencySymbol ->
  Term s PCommitmentV1 ->
  Term s PTokenName ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTrancheDescriptorV1)) ->
  Term s (PBuiltinList PInteger) ->
  Term s PBool
pvalidateInitialTrancheOutputsV1 outputs ownPolicyId commitment challengeAssetName challenger responseDeadline firstOutputIndex initialDescriptorIndex descriptors initialLovelaces =
  ( pfix $ \self -> plam $ \descriptorIndex remainingDescriptors remainingLovelaces ->
      pmatch remainingDescriptors $ \case
        PNil -> pnull # remainingLovelaces
        PCons descriptorData restDescriptors ->
          pmatch remainingLovelaces $ \case
            PNil -> pconstant False
            PCons initialLovelace restLovelaces -> P.do
              descriptor <- plet $ pfromData descriptorData
              output <- plet $ poutputAt outputs (firstOutputIndex + descriptorIndex)
              trancheAssetName <- plet $ ptrancheAssetNameV1 challengeAssetName descriptorIndex
              expectedDatum <-
                plet $
                  pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity, pcommitment'headerHash} ->
                    pmatch descriptor $ \PTrancheDescriptorV1 {ptrancheDescriptor'startOffset} ->
                      pcon $
                        PActiveTranche
                          pcommitment'deploymentIdentity
                          pcommitment'headerHash
                          (pdata challengeAssetName)
                          descriptorData
                          ptrancheDescriptor'startOffset
                          (pdata $ ptrancheStartAccumulatorV1 (pfromData pcommitment'deploymentIdentity) (pfromData pcommitment'headerHash) descriptor)
                          (pdata $ pcon PDNothing)
                          (pdata responseDeadline)
                          challenger
              PTrancheDescriptorV1 {ptrancheDescriptor'trancheIndex} <- pmatch descriptor
              let outputValue = pmatch output $ \PTxOut {ptxOut'value} -> ptxOut'value
              pand'List
                [ pfromData ptrancheDescriptor'trancheIndex #== descriptorIndex
                , pexactScriptOutput output (pscriptAddress ownPolicyId) (pvalueHasOnlyAdaAndAsset outputValue ownPolicyId trancheAssetName 1)
                , plovelaceOf outputValue #== initialLovelace
                , pinlineDatum @PTrancheDatumV1 output #== expectedDatum
                , self # (descriptorIndex + 1) # restDescriptors # restLovelaces
                ]
  )
    # initialDescriptorIndex
    # descriptors
    # initialLovelaces

-- | Aiken @validate_initial_terminal_accumulator_output@.
pvalidateInitialTerminalAccumulatorOutputV1 ::
  forall s.
  Term s PTxOut ->
  Term s PCurrencySymbol ->
  Term s PCommitmentV1 ->
  Term s PTokenName ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pvalidateInitialTerminalAccumulatorOutputV1 output ownPolicyId commitment challengeAssetName challenger responseDeadline initialLovelace =
  pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity, pcommitment'headerHash} ->
    let terminalAssetName = pterminalAccumulatorAssetNameV1 challengeAssetName
        expectedDatum =
          pcon $
            PTerminalAccumulatorDatumV1
              pcommitment'deploymentIdentity
              pcommitment'headerHash
              (pdata challengeAssetName)
              (pdata 0)
              (pdata $ pterminalAccumulatorStartV1 commitment challengeAssetName)
              (pdata $ pconstant False)
              (pdata responseDeadline)
              challenger
              (pdata initialLovelace)
        outputValue = pmatch output $ \PTxOut {ptxOut'value} -> ptxOut'value
     in pexactScriptOutput
          output
          (pscriptAddress ownPolicyId)
          (pvalueHasOnlyAdaAndAsset outputValue ownPolicyId terminalAssetName 1)
          #&& plovelaceOf outputValue #== initialLovelace
          #&& pinlineDatum @PTerminalAccumulatorDatumV1 output #== expectedDatum

pexpectedTrancheMintPairs ::
  forall s.
  Term s (PBuiltinList (PAsData PTrancheDescriptorV1)) ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pexpectedTrancheMintPairs descriptors challengeAssetName initialIndex quantity =
  ( pfix $ \self -> plam $ \remaining index ->
      pmatch remaining $ \case
        PNil -> pnil
        PCons _ rest ->
          pcons
            # (ppairDataBuiltin # pdata (ptrancheAssetNameV1 challengeAssetName index) # pdata quantity)
            # (self # rest # (index + 1))
  )
    # descriptors
    # initialIndex

pmintPairsEqual ::
  forall s.
  Term s (PAsData PMintValue) ->
  Term s PCurrencySymbol ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
  Term s PBool
pmintPairsEqual mint policyId expected =
  pmatch (AssocMap.plookup # policyId # pto (pto (pfromData mint))) $ \case
    PNothing -> pnull # expected
    PJust tokens -> pto (pto tokens) #== expected

pexactEnterpriseAdaOutput ::
  forall s.
  Term s PTxOut ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s PBool
pexactEnterpriseAdaOutput output owner lovelace =
  pmatch output $ \PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum} ->
    pand'List
      [ ptxOut'address #== pcon (PAddress (pcon $ PPubKeyCredential owner) (pcon PDNothing))
      , pto (pfromData ptxOut'value)
          #== Value.psingletonSortedValue
            # Value.padaSymbol
            # Value.padaToken
            # lovelace
      , pmatch ptxOut'datum $ \case PNoOutputDatum -> pconstant True; _ -> pconstant False
      , pnoReferenceScript output
      ]

-- | Aiken @validate_close_challenge@.
pvalidateCloseChallengeV1 ::
  forall s.
  Term s PCurrencySymbol ->
  Term s PParametersV1 ->
  Term s PCurrencySymbol ->
  Term s PTxInfo ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pvalidateCloseChallengeV1 hubOraclePolicyId parameters ownPolicyId tx hubRefInputIndex bondInputIndex terminalInputIndex stateQueueInputIndex stateQueueOutputIndex daRefundOutputIndex challengerRefundOutputIndex = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'fee, ptxInfo'mint} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  outputs <- plet $ pfromData ptxInfo'outputs
  hubDatum <-
    plet $
      Hub.pgetDatum
        # referenceInputs
        # punsafeCoerce @(PAsData PScriptHash) (pdata hubOraclePolicyId)
        # hubRefInputIndex
  stateQueuePolicyId <-
    plet $ pmatch hubDatum $ \Hub.PHubOracleDatum {Hub.phubOracle'stateQueue} -> phubOracle'stateQueue
  bondInput <- plet $ pinputAt inputs bondInputIndex
  PTxInInfo {ptxInInfo'resolved = bondOutput} <- pmatch bondInput
  bondDatum <- plet $ pinlineDatum @PBondDatumV1 bondOutput
  terminalInput <- plet $ pinputAt inputs terminalInputIndex
  PTxInInfo {ptxInInfo'resolved = terminalOutput} <- pmatch terminalInput
  terminalDatum <- plet $ pinlineDatum @PTerminalAccumulatorDatumV1 terminalOutput
  pmatch bondDatum $ \case
    PAvailable {} -> perror
    PChallengedBond
      { pbond'commitment
      , pbond'daBondAssetName
      , pbond'challengeAssetName
      , pbond'challenger
      } -> P.do
        commitment <- plet $ pfromData pbond'commitment
        challengeAssetName <- plet $ pfromData pbond'challengeAssetName
        terminalAssetName <- plet $ pterminalAccumulatorAssetNameV1 challengeAssetName
        PTerminalAccumulatorDatumV1
          { pterminal'deploymentIdentity
          , pterminal'headerHash
          , pterminal'challengeAssetName
          , pterminal'nextTrancheIndex
          , pterminal'hasTimedOutTranche
          , pterminal'challenger
          , pterminal'remainingChallengerLovelace
          } <-
          pmatch terminalDatum
        let fee = pto (pfromData ptxInfo'fee)
            daBondLovelace = pmatch parameters $ \PParametersV1 {pparameters'daBondLovelace} -> pfromData pparameters'daBondLovelace
            maxCloseFee = pmatch parameters $ \PParametersV1 {pparameters'maxCloseFeeLovelace} -> pfromData pparameters'maxCloseFeeLovelace
            expectedBondValue =
              Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # daBondLovelace
                <> (Value.psingletonSortedValue # ownPolicyId # pfromData pbond'daBondAssetName # 1)
                <> (Value.psingletonSortedValue # ownPolicyId # challengeAssetName # 1)
            expectedTerminalValue =
              Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # pfromData pterminal'remainingChallengerLovelace
                <> (Value.psingletonSortedValue # ownPolicyId # terminalAssetName # 1)
            expectedBurnPairs =
              pcons
                # (ppairDataBuiltin # pbond'daBondAssetName # pdata (-1))
                # ( pcons
                      # (ppairDataBuiltin # pdata challengeAssetName # pdata (-1))
                      # (pcons # (ppairDataBuiltin # pdata terminalAssetName # pdata (-1)) # pnil)
                  )
        pand'List
          [ pcommitmentIsCanonicalV1 commitment parameters
          , pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity} ->
              pfromData pcommitment'deploymentIdentity #== pto hubOraclePolicyId
          , fee #> 0
          , fee #<= maxCloseFee
          , plength # inputs #== 3
          , plength # outputs #== 3
          , pnot # (bondInputIndex #== stateQueueInputIndex)
          , pnot # (bondInputIndex #== terminalInputIndex)
          , pnot # (stateQueueInputIndex #== terminalInputIndex)
          , pnot # (daRefundOutputIndex #== challengerRefundOutputIndex)
          , pnot # (daRefundOutputIndex #== stateQueueOutputIndex)
          , pnot # (challengerRefundOutputIndex #== stateQueueOutputIndex)
          , pmatch bondOutput $ \PTxOut {ptxOut'address, ptxOut'value} ->
              ptxOut'address #== pscriptAddress ownPolicyId
                #&& pto (pfromData ptxOut'value) #== expectedBondValue
          , pmatch terminalOutput $ \PTxOut {ptxOut'address, ptxOut'value} ->
              ptxOut'address #== pscriptAddress ownPolicyId
                #&& pto (pfromData ptxOut'value) #== expectedTerminalValue
                #&& pnoReferenceScript terminalOutput
          , pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity} ->
              pterminal'deploymentIdentity #== pcommitment'deploymentIdentity
          , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
              pterminal'headerHash #== pcommitment'headerHash
          , pterminal'challengeAssetName #== pbond'challengeAssetName
          , pmatch commitment $ \PCommitmentV1 {pcommitment'trancheDescriptors} ->
              pfromData pterminal'nextTrancheIndex #== plength # pfromData pcommitment'trancheDescriptors
          , pfromData pterminal'hasTimedOutTranche #== pconstant False
          , pterminal'challenger #== pbond'challenger
          , pmintPairsEqual ptxInfo'mint ownPolicyId expectedBurnPairs
          , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
              pvalidateStateQueueStatusTransitionV1
                inputs
                outputs
                stateQueuePolicyId
                stateQueueInputIndex
                stateQueueOutputIndex
                (pfromData pcommitment'headerHash)
                (pcon $ PChallenged pbond'daBondAssetName pbond'challengeAssetName)
                (pcon $ PPublished $ pdata $ ppublishedTerminalCommitmentV1 commitment)
          , pmatch commitment $ \PCommitmentV1 {pcommitment'bondOwner} ->
              pexactEnterpriseAdaOutput (poutputAt outputs daRefundOutputIndex) pcommitment'bondOwner daBondLovelace
          , pexactEnterpriseAdaOutput
              (poutputAt outputs challengerRefundOutputIndex)
              pbond'challenger
              (pfromData pterminal'remainingChallengerLovelace - fee)
          , pfromData pterminal'remainingChallengerLovelace #> fee
          ]

pvalidateStateQueueTimeoutBindingV1 ::
  forall s.
  Term s PTxInfo ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PTokenName ->
  Term s PBool
pvalidateStateQueueTimeoutBindingV1 tx stateQueuePolicyId stateQueueMintRedeemerIndex headerHash challengeAssetName = P.do
  PTxInfo {ptxInfo'redeemers} <- pmatch tx
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  stateQueueRedeemer <-
    plet $
      pfromData $
        punsafeCoerceRedeemer @StateQueue.PMintRedeemer $
          pgetRedeemerAt
            # redeemers
            # pdata (pcon $ PMinting stateQueuePolicyId)
            # stateQueueMintRedeemerIndex
  pmatch stateQueueRedeemer $ \case
    StateQueue.PRemoveUnavailableBlockAfterTimeout
      { StateQueue.psqRemoveUnavailable'unavailableHeaderHash
      , StateQueue.psqRemoveUnavailable'challengeAssetName
      } ->
        pfromData psqRemoveUnavailable'unavailableHeaderHash #== headerHash
          #&& pfromData psqRemoveUnavailable'challengeAssetName #== challengeAssetName
    _ -> perror

-- | Aiken @validate_timeout_challenge@.
pvalidateTimeoutChallengeV1 ::
  forall s.
  Term s PCurrencySymbol ->
  Term s PParametersV1 ->
  Term s PCurrencySymbol ->
  Term s PTxInfo ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pvalidateTimeoutChallengeV1 hubOraclePolicyId parameters ownPolicyId tx hubRefInputIndex bondInputIndex terminalInputIndex stateQueueMintRedeemerIndex daSlashOutputIndex challengerRefundOutputIndex = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'fee, ptxInfo'mint, ptxInfo'validRange} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  outputs <- plet $ pfromData ptxInfo'outputs
  hubDatum <-
    plet $
      Hub.pgetDatum
        # referenceInputs
        # punsafeCoerce @(PAsData PScriptHash) (pdata hubOraclePolicyId)
        # hubRefInputIndex
  stateQueuePolicyId <-
    plet $ pmatch hubDatum $ \Hub.PHubOracleDatum {Hub.phubOracle'stateQueue} -> phubOracle'stateQueue
  bondInput <- plet $ pinputAt inputs bondInputIndex
  PTxInInfo {ptxInInfo'resolved = bondOutput} <- pmatch bondInput
  bondDatum <- plet $ pinlineDatum @PBondDatumV1 bondOutput
  terminalInput <- plet $ pinputAt inputs terminalInputIndex
  PTxInInfo {ptxInInfo'resolved = terminalOutput} <- pmatch terminalInput
  terminalDatum <- plet $ pinlineDatum @PTerminalAccumulatorDatumV1 terminalOutput
  pmatch bondDatum $ \case
    PAvailable {} -> perror
    PChallengedBond
      { pbond'commitment
      , pbond'daBondAssetName
      , pbond'challengeAssetName
      , pbond'challenger
      , pbond'responseDeadline
      } -> P.do
        commitment <- plet $ pfromData pbond'commitment
        challengeAssetName <- plet $ pfromData pbond'challengeAssetName
        terminalAssetName <- plet $ pterminalAccumulatorAssetNameV1 challengeAssetName
        PTerminalAccumulatorDatumV1
          { pterminal'deploymentIdentity
          , pterminal'headerHash
          , pterminal'challengeAssetName
          , pterminal'nextTrancheIndex
          , pterminal'hasTimedOutTranche
          , pterminal'responseDeadline
          , pterminal'challenger
          , pterminal'remainingChallengerLovelace
          } <-
          pmatch terminalDatum
        let fee = pto (pfromData ptxInfo'fee)
            daBondLovelace = pmatch parameters $ \PParametersV1 {pparameters'daBondLovelace} -> pfromData pparameters'daBondLovelace
            maxTimeoutFee = pmatch parameters $ \PParametersV1 {pparameters'maxTimeoutFeeLovelace} -> pfromData pparameters'maxTimeoutFeeLovelace
            expectedBondValue =
              Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # daBondLovelace
                <> (Value.psingletonSortedValue # ownPolicyId # pfromData pbond'daBondAssetName # 1)
                <> (Value.psingletonSortedValue # ownPolicyId # challengeAssetName # 1)
            expectedTerminalValue =
              Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # pfromData pterminal'remainingChallengerLovelace
                <> (Value.psingletonSortedValue # ownPolicyId # terminalAssetName # 1)
            expectedBurnPairs =
              pcons
                # (ppairDataBuiltin # pbond'daBondAssetName # pdata (-1))
                # ( pcons
                      # (ppairDataBuiltin # pdata challengeAssetName # pdata (-1))
                      # (pcons # (ppairDataBuiltin # pdata terminalAssetName # pdata (-1)) # pnil)
                  )
            challengerAddress = pcon $ PAddress (pcon $ PPubKeyCredential pbond'challenger) (pcon PDNothing)
            challengerOutputCount =
              plength
                # ( pfilter
                      # plam
                        ( \output ->
                            pmatch (pfromData output) $ \PTxOut {ptxOut'address} ->
                              ptxOut'address #== challengerAddress
                        )
                      # outputs
                  )
        pand'List
          [ pcommitmentIsCanonicalV1 commitment parameters
          , pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity} ->
              pfromData pcommitment'deploymentIdentity #== pto hubOraclePolicyId
          , plengthBS # pto challengeAssetName #== 32
          , fee #> 0
          , fee #<= maxTimeoutFee
          , pgetInclusiveLowerBoundOfInterval # ptxInfo'validRange #>= pfromData pbond'responseDeadline
          , pnot # (bondInputIndex #== terminalInputIndex)
          , daSlashOutputIndex #>= 0
          , challengerRefundOutputIndex #>= 0
          , pnot # (daSlashOutputIndex #== challengerRefundOutputIndex)
          , pmatch bondOutput $ \PTxOut {ptxOut'address, ptxOut'value} ->
              ptxOut'address #== pscriptAddress ownPolicyId
                #&& pto (pfromData ptxOut'value) #== expectedBondValue
          , pmatch terminalOutput $ \PTxOut {ptxOut'address, ptxOut'value} ->
              ptxOut'address #== pscriptAddress ownPolicyId
                #&& pto (pfromData ptxOut'value) #== expectedTerminalValue
                #&& pnoReferenceScript terminalOutput
          , pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity} ->
              pterminal'deploymentIdentity #== pcommitment'deploymentIdentity
          , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
              pterminal'headerHash #== pcommitment'headerHash
          , pterminal'challengeAssetName #== pbond'challengeAssetName
          , pmatch commitment $ \PCommitmentV1 {pcommitment'trancheDescriptors} ->
              pfromData pterminal'nextTrancheIndex #== plength # pfromData pcommitment'trancheDescriptors
          , pfromData pterminal'hasTimedOutTranche
          , pterminal'responseDeadline #== pbond'responseDeadline
          , pterminal'challenger #== pbond'challenger
          , pmintPairsEqual ptxInfo'mint ownPolicyId expectedBurnPairs
          , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
              pvalidateStateQueueTimeoutBindingV1
                tx
                stateQueuePolicyId
                stateQueueMintRedeemerIndex
                (pfromData pcommitment'headerHash)
                challengeAssetName
          , pexactEnterpriseAdaOutput (poutputAt outputs daSlashOutputIndex) pbond'challenger daBondLovelace
          , pexactEnterpriseAdaOutput
              (poutputAt outputs challengerRefundOutputIndex)
              pbond'challenger
              (pfromData pterminal'remainingChallengerLovelace - fee)
          , pfromData pterminal'remainingChallengerLovelace #> fee
          , challengerOutputCount #== 2
          ]

-- | Aiken @validate_open_challenge@.
pvalidateOpenChallengeV1 ::
  forall s.
  Term s PCurrencySymbol ->
  Term s PParametersV1 ->
  Term s PCurrencySymbol ->
  Term s PTxInfo ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PPubKeyHash) ->
  Term s PBool
pvalidateOpenChallengeV1 hubOraclePolicyId parameters ownPolicyId tx hubRefInputIndex bondInputIndex bondOutputIndex challengerInputIndex stateQueueInputIndex stateQueueOutputIndex firstTrancheOutputIndex terminalOutputIndex challenger = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'fee, ptxInfo'mint, ptxInfo'validRange, ptxInfo'signatories} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  outputs <- plet $ pfromData ptxInfo'outputs
  hubDatum <-
    plet $
      Hub.pgetDatum
        # referenceInputs
        # punsafeCoerce @(PAsData PScriptHash) (pdata hubOraclePolicyId)
        # hubRefInputIndex
  stateQueuePolicy <-
    plet $ pmatch hubDatum $ \Hub.PHubOracleDatum {Hub.phubOracle'stateQueue} -> phubOracle'stateQueue
  bondInput <- plet $ pinputAt inputs bondInputIndex
  PTxInInfo {ptxInInfo'outRef = bondInputRef, ptxInInfo'resolved = bondInputOutput} <- pmatch bondInput
  bondDatum <- plet $ pinlineDatum @PBondDatumV1 bondInputOutput
  challengerInput <- plet $ pinputAt inputs challengerInputIndex
  PTxInInfo {ptxInInfo'resolved = challengerInputOutput} <- pmatch challengerInput
  bondOutput <- plet $ poutputAt outputs bondOutputIndex
  let (inclusiveLowerBound, _inclusiveUpperBound) = pgetInclusiveBoundsOfAShortValidityRange ptxInfo'validRange
      fee = pto (pfromData ptxInfo'fee)
      expectedAddress = pscriptAddress ownPolicyId
  pmatch bondDatum $ \case
    PChallengedBond {} -> perror
    PAvailable commitmentData daBondAssetName committeeSignersHash attestedSigners -> P.do
      commitment <- plet $ pfromData commitmentData
      challengeAssetName <- plet $ pchallengeAssetNameV1 bondInputRef
      responseDeadline <-
        plet $
          pmatch
            ( pmatch commitment $ \PCommitmentV1 {pcommitment'payloadByteLength} ->
                presponseDeadlineV1 (pfromData pcommitment'payloadByteLength) inclusiveLowerBound
            )
            $ \case
              PJust deadline -> deadline
              PNothing -> perror
      descriptors <-
        plet $ pmatch commitment $ \PCommitmentV1 {pcommitment'trancheDescriptors} -> pfromData pcommitment'trancheDescriptors
      initialLovelaces <-
        plet $
          pmatch (ptrancheInitialLovelacesV1 commitment parameters) $ \case
            PJust values -> values
            PNothing -> perror
      terminalLovelace <- plet $ pterminalAccumulatorReserveLovelaceV1 parameters
      terminalAssetName <- plet $ pterminalAccumulatorAssetNameV1 challengeAssetName
      expectedBondDatum <-
        plet $
          pcon $
            PChallengedBond
              commitmentData
              daBondAssetName
              committeeSignersHash
              attestedSigners
              (pdata challengeAssetName)
              challenger
              (pdata inclusiveLowerBound)
              (pdata responseDeadline)
      descriptorCount <- plet $ plength # descriptors
      let maximumOpenFee = pmatch parameters $ \PParametersV1 {pparameters'maxOpenFeeLovelace} -> pfromData pparameters'maxOpenFeeLovelace
          daBondLovelace = pmatch parameters $ \PParametersV1 {pparameters'daBondLovelace} -> pfromData pparameters'daBondLovelace
          challengerBondLovelace = pmatch parameters $ \PParametersV1 {pparameters'challengerBondLovelace} -> pfromData pparameters'challengerBondLovelace
          bondInputValue = pmatch bondInputOutput $ \PTxOut {ptxOut'value} -> ptxOut'value
          bondOutputValue = pmatch bondOutput $ \PTxOut {ptxOut'value} -> ptxOut'value
          expectedBondInputValue =
            Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # daBondLovelace
              <> (Value.psingletonSortedValue # ownPolicyId # pfromData daBondAssetName # 1)
          expectedBondOutputValue =
            expectedBondInputValue
              <> (Value.psingletonSortedValue # ownPolicyId # challengeAssetName # 1)
          expectedMintPairs =
            pcons
              # (ppairDataBuiltin # pdata challengeAssetName # pdata 1)
              # ( pcons
                    # (ppairDataBuiltin # pdata terminalAssetName # pdata 1)
                    # pexpectedTrancheMintPairs descriptors challengeAssetName 0 1
                )
          indexOutside index = index #< firstTrancheOutputIndex #|| index #>= firstTrancheOutputIndex + descriptorCount
      pand'List
        [ plength # inputs #== 3
        , plength # outputs #== 3 + descriptorCount
        , fee #> 0
        , fee #<= maximumOpenFee
        , pnot # (bondInputIndex #== challengerInputIndex)
        , pnot # (bondInputIndex #== stateQueueInputIndex)
        , pnot # (challengerInputIndex #== stateQueueInputIndex)
        , pnot # (bondOutputIndex #== stateQueueOutputIndex)
        , indexOutside bondOutputIndex
        , indexOutside stateQueueOutputIndex
        , pnot # (terminalOutputIndex #== bondOutputIndex)
        , pnot # (terminalOutputIndex #== stateQueueOutputIndex)
        , indexOutside terminalOutputIndex
        , firstTrancheOutputIndex + descriptorCount #<= plength # outputs
        , pmatch commitment $ \PCommitmentV1 {pcommitment'deploymentIdentity} -> pfromData pcommitment'deploymentIdentity #== pto hubOraclePolicyId
        , phasSigned # challenger # pfromData ptxInfo'signatories
        , pmatch bondInputOutput $ \PTxOut {ptxOut'address} -> ptxOut'address #== expectedAddress
        , pto (pfromData bondInputValue) #== expectedBondInputValue
        , pexactEnterpriseAdaOutput challengerInputOutput challenger (challengerBondLovelace + fee)
        , pmintPairsEqual ptxInfo'mint ownPolicyId expectedMintPairs
        , pexactScriptOutput bondOutput expectedAddress (pto (pfromData bondOutputValue) #== expectedBondOutputValue)
        , pinlineDatum @PBondDatumV1 bondOutput #== expectedBondDatum
        , pmatch commitment $ \PCommitmentV1 {pcommitment'headerHash} ->
            StateQueue.pvalidateDaAvailabilityStatusTransition
              inputs
              outputs
              stateQueuePolicy
              stateQueueInputIndex
              stateQueueOutputIndex
              (pfromData pcommitment'headerHash)
              (pcon $ PAttested daBondAssetName)
              (pcon $ PChallenged daBondAssetName $ pdata challengeAssetName)
        , pvalidateInitialTrancheOutputsV1
            outputs
            ownPolicyId
            commitment
            challengeAssetName
            challenger
            responseDeadline
            firstTrancheOutputIndex
            0
            descriptors
            initialLovelaces
        , pvalidateInitialTerminalAccumulatorOutputV1
            (poutputAt outputs terminalOutputIndex)
            ownPolicyId
            commitment
            challengeAssetName
            challenger
            responseDeadline
            terminalLovelace
        ]

{- | Aiken @validate_advance_tranche@.

The thread and its latest publication carrier form a same-transaction linked
list. A new publication replaces the thread and carrier while preserving the
combined lovelace minus the bounded transaction fee.
-}
pvalidateAdvanceTrancheV1 ::
  forall s.
  Term s PParametersV1 ->
  Term s PCurrencySymbol ->
  Term s PTrancheDatumV1 ->
  Term s PTxInInfo ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybeData PInteger) ->
  Term s PTxInfo ->
  Term s PBool
pvalidateAdvanceTrancheV1 parameters ownPolicyId currentDatum ownInput threadOutputIndex carrierOutputIndex previousCarrierInputIndex tx =
  pmatch currentDatum $ \case
    PReceipt _ _ _ _ _ _ _ -> perror
    PActiveTranche deploymentIdentityData headerHashData challengeAssetNameData descriptorData nextOffsetData accumulatorData latestCarrierOutputIndexData responseDeadlineData _ -> P.do
      descriptor <- plet $ pfromData descriptorData
      PTrancheDescriptorV1 {ptrancheDescriptor'trancheIndex, ptrancheDescriptor'startOffset} <- pmatch descriptor
      challengeAssetName <- plet $ pfromData challengeAssetNameData
      trancheAssetName <-
        plet $ ptrancheAssetNameV1 challengeAssetName (pfromData ptrancheDescriptor'trancheIndex)
      PTxInInfo {ptxInInfo'outRef = ownRef, ptxInInfo'resolved = ownResolved} <- pmatch ownInput
      PTxOut {ptxOut'value = ownValue} <- pmatch ownResolved
      _ <- plet $ pif (pvalueHasOnlyAdaAndAsset ownValue ownPolicyId trancheAssetName 1) (pconstant @PBool True) perror
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'fee, ptxInfo'mint, ptxInfo'validRange} <- pmatch tx
      inputs <- plet $ pfromData ptxInfo'inputs
      outputs <- plet $ pfromData ptxInfo'outputs
      threadOutput <- plet $ poutputAt outputs threadOutputIndex
      carrierOutput <- plet $ poutputAt outputs carrierOutputIndex
      publication <- plet $ pinlineDatum @PPublicationDatumV1 carrierOutput
      responseGeometry <-
        plet $ pmatch parameters $ \PParametersV1 {pparameters'responseGeometry} -> pfromData pparameters'responseGeometry
      chunkByteLength <-
        plet $ pmatch responseGeometry $ \PResponseGeometryV1 {presponseGeometry'chunkByteLength} -> pfromData presponseGeometry'chunkByteLength
      expectedNextDatum <-
        plet $
          pmatch
            (ppublicationAdvancesActiveTrancheV1 currentDatum publication chunkByteLength carrierOutputIndex)
            $ \case
              PJust next -> next
              PNothing -> perror
      nextDatum <- plet $ pinlineDatum @PTrancheDatumV1 threadOutput
      previousCarrierLovelace <-
        plet $
          pmatch previousCarrierInputIndex $ \case
            PDNothing ->
              pif
                ( pmatch (pfromData latestCarrierOutputIndexData) (\case PDNothing -> pconstant True; PDJust _ -> pconstant False)
                    #&& pfromData nextOffsetData #== pfromData ptrancheDescriptor'startOffset
                    #&& pfromData accumulatorData
                      #== ptrancheStartAccumulatorV1
                        (pfromData deploymentIdentityData)
                        (pfromData headerHashData)
                        descriptor
                )
                0
                perror
            PDJust previousCarrierInputIndexData -> P.do
              previousCarrier <- plet $ pinputAt inputs (pfromData previousCarrierInputIndexData)
              PTxInInfo {ptxInInfo'outRef = previousRef, ptxInInfo'resolved = previousResolved} <- pmatch previousCarrier
              previousPublication <- plet $ pinlineDatum @PPublicationDatumV1 previousResolved
              expectedPreviousOutputIndex <-
                plet $
                  pmatch (pfromData latestCarrierOutputIndexData) $ \case
                    PDJust outputIndex -> pfromData outputIndex
                    PDNothing -> perror
              PTxOut {ptxOut'address = previousAddress, ptxOut'value = previousValue} <- pmatch previousResolved
              refsAreLinked <-
                plet $
                  pmatch previousRef $ \PTxOutRef {ptxOutRef'id = previousTxId, ptxOutRef'idx = previousIndex} ->
                    pmatch ownRef $ \PTxOutRef {ptxOutRef'id = ownTxId} ->
                      pand'List
                        [ pnot # (previousRef #== ownRef)
                        , previousTxId #== ownTxId
                        , pfromData previousIndex #== expectedPreviousOutputIndex
                        ]
              publicationIsLinked <-
                plet $
                  pmatch previousPublication $ \PPublicationDatumV1 {..} ->
                    pand'List
                      [ pfromData ppublication'deploymentIdentity #== pfromData deploymentIdentityData
                      , pfromData ppublication'headerHash #== pfromData headerHashData
                      , pfromData ppublication'challengeAssetName #== challengeAssetName
                      , pfromData ppublication'trancheIndex #== pfromData ptrancheDescriptor'trancheIndex
                      , pfromData ppublication'nextAccumulator #== pfromData accumulatorData
                      , pfromData ppublication'chunkOffset + pfromData ppublication'chunkByteLength #== pfromData nextOffsetData
                      ]
              pif
                ( pand'List
                    [ refsAreLinked
                    , previousAddress #== pscriptAddress ownPolicyId
                    , pvalueIsAdaOnly previousValue
                    , pnoReferenceScript previousResolved
                    , publicationIsLinked
                    ]
                )
                (plovelaceOf previousValue)
                perror
      let (_inclusiveLowerBound, inclusiveUpperBound) =
            pgetInclusiveBoundsOfAShortValidityRange ptxInfo'validRange
          fee = pto (pfromData ptxInfo'fee)
          maximumPublicationFee =
            pmatch parameters $ \PParametersV1 {pparameters'maxPublicationFeeLovelace} ->
              pfromData pparameters'maxPublicationFeeLovelace
          expectedAddress = pscriptAddress ownPolicyId
          threadValue = pmatch threadOutput $ \PTxOut {ptxOut'value} -> ptxOut'value
          carrierValue = pmatch carrierOutput $ \PTxOut {ptxOut'value} -> ptxOut'value
      pand'List
        [ pmintIsZero ptxInfo'mint
        , fee #> 0
        , fee #<= maximumPublicationFee
        , inclusiveUpperBound #<= pfromData responseDeadlineData
        , nextDatum #== expectedNextDatum
        , pexactScriptOutput threadOutput expectedAddress (pvalueHasOnlyAdaAndAsset threadValue ownPolicyId trancheAssetName 1)
        , pexactScriptOutput carrierOutput expectedAddress (pvalueIsAdaOnly carrierValue)
        , plovelaceOf threadValue + plovelaceOf carrierValue + fee
            #== plovelaceOf ownValue + previousCarrierLovelace
        , plength # inputs #== pmatch previousCarrierInputIndex (\case PDNothing -> 1; PDJust _ -> 2)
        , plength # outputs #== 2
        , pnot # (threadOutputIndex #== carrierOutputIndex)
        ]

-- | Aiken @validate_consume_carrier@.
pvalidateConsumeCarrierV1 ::
  forall s.
  Term s PTxOutRef -> Term s PInteger -> Term s PInteger -> Term s PTxInfo -> Term s PBool
pvalidateConsumeCarrierV1 ownRef threadInputIndex threadSpendRedeemerIndex tx = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'redeemers} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  threadInput <- plet $ pinputAt inputs threadInputIndex
  PTxInInfo {ptxInInfo'outRef = threadRef} <- pmatch threadInput
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  threadRedeemer <-
    plet $
      pfromData $
        punsafeCoerceRedeemer @PSpendRedeemerV1 $
          pgetRedeemerAt # redeemers # pdata (pcon $ PSpending threadRef) # threadSpendRedeemerIndex
  previousCarrierInputIndex <-
    plet $
      pmatch threadRedeemer $ \case
        PAdvanceTranche _ _ previousIndex ->
          pmatch (pfromData previousIndex) $ \case
            PDJust inputIndex -> pfromData inputIndex
            PDNothing -> perror
        _ -> perror
  PTxInInfo {ptxInInfo'outRef = previousCarrierRef} <- pmatch $ pinputAt inputs previousCarrierInputIndex
  previousCarrierRef #== ownRef

-- | Aiken @validate_coordinate_spend@.
pvalidateCoordinateSpendV1 ::
  forall s.
  Term s PCurrencySymbol -> Term s PTxOutRef -> Term s PInteger -> Term s PTxInfo -> Term s PBool
pvalidateCoordinateSpendV1 ownPolicyId ownRef mintRedeemerIndex tx = P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'redeemers} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  mintRedeemer <-
    plet $
      pfromData $
        punsafeCoerceRedeemer @PMintRedeemerV1 $
          pgetRedeemerAt # redeemers # pdata (pcon $ PMinting $ pdata ownPolicyId) # mintRedeemerIndex
  let refAt index = pmatch (pinputAt inputs index) $ \PTxInInfo {ptxInInfo'outRef} -> ptxInInfo'outRef
  pmatch mintRedeemer $ \case
    PMintBondFromAttestation _ _ _ _ _ _ _ -> perror
    POpenChallenge _ _ bondInputIndex _ _ _ _ _ _ _ -> refAt (pfromData bondInputIndex) #== ownRef
    PSettleTranche _ _ terminalInputIndex _ trancheInputIndex carrierInputIndex ->
      refAt (pfromData terminalInputIndex) #== ownRef
        #|| refAt (pfromData trancheInputIndex) #== ownRef
        #|| pmatch (pfromData carrierInputIndex) (\case PDNothing -> pconstant False; PDJust index -> refAt (pfromData index) #== ownRef)
    PCloseChallenge _ _ bondInputIndex terminalInputIndex _ _ _ _ ->
      refAt (pfromData bondInputIndex) #== ownRef #|| refAt (pfromData terminalInputIndex) #== ownRef
    PTimeoutChallenge _ _ bondInputIndex terminalInputIndex _ _ _ ->
      refAt (pfromData bondInputIndex) #== ownRef #|| refAt (pfromData terminalInputIndex) #== ownRef

-- | Spending handler for @validators/availability-challenge.ak@.
availabilityChallengeSpendValidator ::
  forall s. Term s (PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeSpendValidator = plam $ \parametersData ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  ownRef <- plet $ pmatch pscriptContext'scriptInfo $ \case PSpendingScript outRef _ -> outRef; _ -> perror
  ownDatumData <- plet $ pmatch pscriptContext'scriptInfo $ \case PSpendingScript _ datum -> datum; _ -> perror
  tx <- plet pscriptContext'txInfo
  PTxInfo {ptxInfo'inputs} <- pmatch tx
  inputs <- plet $ pfromData ptxInfo'inputs
  ownInput <- plet $ pfindOwnInput inputs ownRef
  ownPolicyId <-
    plet $ pmatch ownInput $ \PTxInInfo {ptxInInfo'resolved} ->
      pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'address} -> pscriptHash ptxOut'address
  redeemer <- plet $ pfromData $ punsafeCoerceOwnRedeemer @PSpendRedeemerV1 pscriptContext'redeemer
  parameters <- plet $ pfromData parametersData
  result <-
    plet $ pmatch redeemer $ \case
      PAdvanceTranche threadOutputIndex carrierOutputIndex previousCarrierInputIndex ->
        pmatch ownDatumData $ \case
          PDNothing -> perror
          PDJust datum ->
            pvalidateAdvanceTrancheV1
              parameters
              ownPolicyId
              (pfromData $ punsafeCoerce @(PAsData PTrancheDatumV1) $ pto $ pfromData datum)
              ownInput
              (pfromData threadOutputIndex)
              (pfromData carrierOutputIndex)
              (pfromData previousCarrierInputIndex)
              tx
      PConsumeCarrier threadInputIndex threadSpendRedeemerIndex ->
        pvalidateConsumeCarrierV1 ownRef (pfromData threadInputIndex) (pfromData threadSpendRedeemerIndex) tx
      PCoordinate mintRedeemerIndex ->
        pvalidateCoordinateSpendV1 ownPolicyId ownRef (pfromData mintRedeemerIndex) tx
  pif result (pconstant ()) perror

-- | Minting authenticates the arm-specific role NFT and zero withdrawal.
availabilityChallengeMintValidator :: forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeMintValidator = plam $ \_hubOracle referenceScriptAuthPolicyId _parameters ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PMintingScript _ -> P.do
      redeemer <- plet $ pfromData $ punsafeCoerceOwnRedeemer @PMintRedeemerV1 pscriptContext'redeemer
      let requireYield role index = plet
            (Yield.prequireAuthenticatedZeroYield # pscriptContext'txInfo # pfromData referenceScriptAuthPolicyId # role # pfromData index)
            (const $ pconstant ())
      pmatch redeemer $ \case
        PMintBondFromAttestation index _ _ _ _ _ _ -> requireYield (pcon (PTokenName $ pconstant "AvailabilityChallengeBondYield")) index
        POpenChallenge index _ _ _ _ _ _ _ _ _ -> requireYield (pcon (PTokenName $ pconstant "AvailabilityChallengeOpenYield")) index
        PSettleTranche index _ _ _ _ _ -> requireYield (pcon (PTokenName $ pconstant "AvailabilityChallengeSettleYield")) index
        PCloseChallenge index _ _ _ _ _ _ _ -> requireYield (pcon (PTokenName $ pconstant "AvailabilityChallengeCloseYield")) index
        PTimeoutChallenge index _ _ _ _ _ _ -> requireYield (pcon (PTokenName $ pconstant "AvailabilityChallengeExpiryYield")) index
    _ -> perror

-- | Aiken @availability_challenge_yields.bond.withdraw@.
availabilityChallengeBondYieldValidator :: forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeBondYieldValidator = plam $ \ownPolicyId hubOraclePolicyIdData parametersData ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  redeemer <- plet $ pfromData $ punsafeCoerceRedeemer @PMintRedeemerV1 $
    Yield.pgetYieldedMintRedeemer # ctx # pfromData ownPolicyId
  result <- plet $ pmatch redeemer $ \case
    PMintBondFromAttestation _ hubRefInputIndex daAttestationInputIndex daAttestationMintRedeemerIndex bondOutputIndex stateQueueInputIndex stateQueueOutputIndex ->
            pvalidateMintBondFromAttestationV1
              (pfromData hubOraclePolicyIdData)
              (pfromData parametersData)
              (pfromData ownPolicyId)
              pscriptContext'txInfo
              (pfromData hubRefInputIndex)
              (pfromData daAttestationInputIndex)
              (pfromData daAttestationMintRedeemerIndex)
              (pfromData bondOutputIndex)
              (pfromData stateQueueInputIndex)
              (pfromData stateQueueOutputIndex)
    _ -> perror
  pif result (pconstant ()) perror

-- | Aiken @availability_challenge_yields.open.withdraw@.
availabilityChallengeOpenYieldValidator :: forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeOpenYieldValidator = plam $ \ownPolicyId hubOraclePolicyIdData parametersData ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  redeemer <- plet $ pfromData $ punsafeCoerceRedeemer @PMintRedeemerV1 $
    Yield.pgetYieldedMintRedeemer # ctx # pfromData ownPolicyId
  result <- plet $ pmatch redeemer $ \case
    POpenChallenge _ hubRefInputIndex bondInputIndex bondOutputIndex challengerInputIndex stateQueueInputIndex stateQueueOutputIndex firstTrancheOutputIndex terminalOutputIndex challenger ->
            pvalidateOpenChallengeV1
              (pfromData hubOraclePolicyIdData)
              (pfromData parametersData)
              (pfromData ownPolicyId)
              pscriptContext'txInfo
              (pfromData hubRefInputIndex)
              (pfromData bondInputIndex)
              (pfromData bondOutputIndex)
              (pfromData challengerInputIndex)
              (pfromData stateQueueInputIndex)
              (pfromData stateQueueOutputIndex)
              (pfromData firstTrancheOutputIndex)
              (pfromData terminalOutputIndex)
              challenger
    _ -> perror
  pif result (pconstant ()) perror

-- | Aiken @availability_challenge_yields.settle.withdraw@.
availabilityChallengeSettleYieldValidator :: forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeSettleYieldValidator = plam $ \ownPolicyId hubOraclePolicyIdData parametersData ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  redeemer <- plet $ pfromData $ punsafeCoerceRedeemer @PMintRedeemerV1 $
    Yield.pgetYieldedMintRedeemer # ctx # pfromData ownPolicyId
  result <- plet $ pmatch redeemer $ \case
    PSettleTranche _ bondRefInputIndex terminalInputIndex terminalOutputIndex trancheInputIndex carrierInputIndex ->
            pvalidateSettleTrancheV1
              (pfromData hubOraclePolicyIdData)
              (pfromData parametersData)
              (pfromData ownPolicyId)
              pscriptContext'txInfo
              (pfromData bondRefInputIndex)
              (pfromData terminalInputIndex)
              (pfromData terminalOutputIndex)
              (pfromData trancheInputIndex)
              (pfromData carrierInputIndex)
    _ -> perror
  pif result (pconstant ()) perror

-- | Aiken @availability_challenge_yields.close.withdraw@.
availabilityChallengeCloseYieldValidator :: forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeCloseYieldValidator = plam $ \ownPolicyId hubOraclePolicyIdData parametersData ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  redeemer <- plet $ pfromData $ punsafeCoerceRedeemer @PMintRedeemerV1 $
    Yield.pgetYieldedMintRedeemer # ctx # pfromData ownPolicyId
  result <- plet $ pmatch redeemer $ \case
    PCloseChallenge _ hubRefInputIndex bondInputIndex terminalInputIndex stateQueueInputIndex stateQueueOutputIndex daRefundOutputIndex challengerRefundOutputIndex ->
            pvalidateCloseChallengeV1
              (pfromData hubOraclePolicyIdData)
              (pfromData parametersData)
              (pfromData ownPolicyId)
              pscriptContext'txInfo
              (pfromData hubRefInputIndex)
              (pfromData bondInputIndex)
              (pfromData terminalInputIndex)
              (pfromData stateQueueInputIndex)
              (pfromData stateQueueOutputIndex)
              (pfromData daRefundOutputIndex)
              (pfromData challengerRefundOutputIndex)
    _ -> perror
  pif result (pconstant ()) perror

-- | Aiken @availability_challenge_yields.timeout.withdraw@.
availabilityChallengeTimeoutYieldValidator :: forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeTimeoutYieldValidator = plam $ \ownPolicyId hubOraclePolicyIdData parametersData ctx -> P.do
  PScriptContext {pscriptContext'txInfo} <- pmatch ctx
  redeemer <- plet $ pfromData $ punsafeCoerceRedeemer @PMintRedeemerV1 $
    Yield.pgetYieldedMintRedeemer # ctx # pfromData ownPolicyId
  result <- plet $ pmatch redeemer $ \case
    PTimeoutChallenge _ hubRefInputIndex bondInputIndex terminalInputIndex stateQueueMintRedeemerIndex daSlashOutputIndex challengerRefundOutputIndex ->
            pvalidateTimeoutChallengeV1
              (pfromData hubOraclePolicyIdData)
              (pfromData parametersData)
              (pfromData ownPolicyId)
              pscriptContext'txInfo
              (pfromData hubRefInputIndex)
              (pfromData bondInputIndex)
              (pfromData terminalInputIndex)
              (pfromData stateQueueMintRedeemerIndex)
              (pfromData daSlashOutputIndex)
              (pfromData challengerRefundOutputIndex)
    _ -> perror
  pif result (pconstant ()) perror

-- | Deployable multi-purpose availability-challenge validator.
availabilityChallengeValidator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PParametersV1 :--> PScriptContext :--> PUnit)
availabilityChallengeValidator = plam $ \hubOraclePolicyId referenceScriptAuthPolicyId parameters ctx -> P.do
  PScriptContext {pscriptContext'scriptInfo} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PMintingScript _ ->
      availabilityChallengeMintValidator
        # hubOraclePolicyId
        # referenceScriptAuthPolicyId
        # parameters
        # ctx
    PSpendingScript _ _ ->
      availabilityChallengeSpendValidator
        # parameters
        # ctx
    _ -> perror
