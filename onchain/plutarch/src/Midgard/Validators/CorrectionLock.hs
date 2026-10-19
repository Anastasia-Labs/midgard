{- |
Module      : Midgard.Validators.CorrectionLock
Description : Plutarch port of @validators/correction-lock.ak@.
-}
module Midgard.Validators.CorrectionLock (correctionLockSpendValidator) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  POutputDatum (..),
  PPosixTime,
  PRedeemer,
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.AvailabilityChallenge (PBondDatumV1 (..), PCommitmentV1 (..))
import Midgard.Common.Utils (
  pgetAuthenticInputOf,
  pgetInclusiveLowerBoundOfInterval,
  pheadSingleton,
 )
import Midgard.CorrectionLock (
  PCorrectionIdentity (..),
  PCorrectionLockDatum (..),
  PCorrectionLockRedeemer (..),
  passetName,
  pdatumTransitionIsValid,
  pdecodeDatum,
  phasNoOutput,
  puniqueInput,
  puniqueOutput,
 )
import Midgard.CorrectionLock qualified as Correction
import Midgard.FraudProof (pgetProvenFraudRecordWithIdentity)
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.StateQueue (
  PAttestationTimeoutRemovalApproach (..),
  PBlockRemovalApproach (..),
  PMintRedeemer (..),
 )
import Midgard.StateQueue qualified as StateQueue

punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer redeemer = punsafeCoerce (pto redeemer)

pstateQueueMintRedeemer ::
  forall s.
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PMintRedeemer
pstateQueueMintRedeemer redeemers stateQueuePolicyId =
  pfromData $ punsafeCoerce @(PAsData PMintRedeemer) $ pto $ pfromData $ (pmatch matching $ \(PBuiltinPair _ pairSecond) -> pairSecond)
  where
    matching =
      pheadSingleton
        #$ pfilter
        # plam
          (\pair -> (pmatch pair $ \(PBuiltinPair pairFirst _) -> pairFirst) #== pdata (pcon (PMinting stateQueuePolicyId)))
        # redeemers

phubDatumFromSpentInput ::
  forall s.
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s PHubOracleDatum
phubDatumFromSpentInput inputs hubOraclePolicyId inputIndex = P.do
  hubInput <-
    plet $
      pgetAuthenticInputOf
        # inputs
        # hubOraclePolicyId
        # Hub.passetName
        # inputIndex
  PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData hubInput
  PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
  pmatch ptxOut'datum $ \case
    POutputDatum {poutputDatum'outputDatum} ->
      pfromData $ punsafeCoerce @(PAsData PHubOracleDatum) $ pto poutputDatum'outputDatum
    _ -> perror

paddressIsPolicyScript ::
  forall s. Term s PAddress -> Term s (PAsData PCurrencySymbol) -> Term s PBool
paddressIsPolicyScript address policyId =
  pmatch address $ \PAddress {paddress'credential, paddress'stakingCredential} ->
    pand'List
      [ pmatch paddress'credential $ \case
          PScriptCredential scriptHash ->
            pto (pfromData scriptHash) #== pto (pfromData policyId)
          _ -> pconstant False
      , pmatch paddress'stakingCredential $ \case
          PDNothing -> pconstant True
          _ -> pconstant False
      ]

pchallengeAcquisitionIsValid ::
  forall s.
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PByteString ->
  Term s (PInterval PPosixTime) ->
  Term s PBool
pchallengeAcquisitionIsValid inputs availabilityPolicyId challengeAssetName unavailableHeaderHash validityRange = P.do
  challengeInput <-
    plet $
      pfromData $
        pheadSingleton
          #$ pfilter
          # plam
            ( \inputData ->
                pmatch (pfromData inputData) $ \PTxInInfo {ptxInInfo'resolved} ->
                  pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'value} ->
                    Value.pvalueOf
                      # pto (pfromData ptxOut'value)
                      # pfromData availabilityPolicyId
                      # pfromData challengeAssetName
                      #== 1
            )
          # inputs
  PTxInInfo {ptxInInfo'resolved} <- pmatch challengeInput
  PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum} <- pmatch ptxInInfo'resolved
  bondDatum <-
    plet $ pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} ->
        pfromData $ punsafeCoerce @(PAsData PBondDatumV1) $ pto poutputDatum'outputDatum
      _ -> perror
  pmatch bondDatum $ \case
    PChallengedBond
      { pbond'commitment
      , pbond'daBondAssetName
      , pbond'challengeAssetName
      , pbond'responseDeadline
      } ->
        pmatch (pfromData pbond'commitment) $ \PCommitmentV1 {pcommitment'headerHash} ->
          pand'List
            [ paddressIsPolicyScript ptxOut'address availabilityPolicyId
            , Value.pvalueOf
                # pto (pfromData ptxOut'value)
                # pfromData availabilityPolicyId
                # pfromData pbond'daBondAssetName
                #== 1
            , pbond'challengeAssetName #== challengeAssetName
            , pcommitment'headerHash #== pdata unavailableHeaderHash
            , pgetInclusiveLowerBoundOfInterval # validityRange
                #>= pfromData pbond'responseDeadline
            ]
    _ -> perror

pcorrectionTransition ::
  forall s.
  Term s PCorrectionLockDatum ->
  Term s PTxInInfo ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PHubOracleDatum ->
  Term s PTxInfo ->
  Term s PBool
pcorrectionTransition currentDatum ownInput hubOraclePolicyId availabilityPolicyId hubDatum txInfo = P.do
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'outputs
    , ptxInfo'referenceInputs
    , ptxInfo'mint
    , ptxInfo'redeemers
    , ptxInfo'validRange
    } <- pmatch txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  PHubOracleDatum {phubOracle'stateQueue, phubOracle'fraudProof} <- pmatch hubDatum
  stateQueueRedeemer <- plet $ pstateQueueMintRedeemer redeemers phubOracle'stateQueue

  expectedAndTerminal <-
    plet $ pmatch stateQueueRedeemer $ \case
      PRemoveFraudulentBlockHeader
        { psqRemove'fraudulentBlocksHeaderHash
        , psqRemove'fraudProofRefInputIndex
        , psqRemove'blockRemovalApproach
        } ->
          pgetProvenFraudRecordWithIdentity
            referenceInputs
            phubOracle'fraudProof
            (pfromData psqRemove'fraudProofRefInputIndex)
            ( \referencedHeaderHash _fraudProver proofAssetName ->
                pif
                  (referencedHeaderHash #== pfromData psqRemove'fraudulentBlocksHeaderHash)
                  ( ppairDataBuiltin
                      # pdata
                        ( pcon $
                            PLocked
                              psqRemove'fraudulentBlocksHeaderHash
                              (pdata $ pcon $ PFraudProof proofAssetName)
                        )
                      # pdata
                        ( pmatch (pfromData psqRemove'blockRemovalApproach) $ \case
                            PRemoveFraudulentBlocksLink {} -> pconstant False
                            PRemoveLastFraudulentBlock {} -> pconstant True
                        )
                  )
                  perror
            )
      PRemoveUnattestedBlockAfterTimeout
        { psqRemoveUnattested'timedOutHeaderHash
        , psqRemoveUnattested'removalApproach
        } ->
          ppairDataBuiltin
            # pdata
              ( pcon $
                  PLocked
                    psqRemoveUnattested'timedOutHeaderHash
                    (pdata $ pcon PAttestationTimeout)
              )
            # pdata
              ( pmatch (pfromData psqRemoveUnattested'removalApproach) $ \case
                  PPruneTimedOutBlockDescendant {} -> pconstant False
                  PRemoveTimedOutHead {} -> pconstant True
              )
      PRemoveUnavailableBlockAfterTimeout
        { psqRemoveUnavailable'unavailableHeaderHash
        , psqRemoveUnavailable'challengeAssetName
        , psqRemoveUnavailable'removalApproach
        } ->
          plet
            ( pcon $
                PLocked
                  psqRemoveUnavailable'unavailableHeaderHash
                  (pdata $ pcon $ PAvailabilityChallenge psqRemoveUnavailable'challengeAssetName)
            )
            $ \expectedLocked ->
              pif
                (plengthBS # pto (pfromData psqRemoveUnavailable'challengeAssetName) #== 32)
                ( pif
                    ( pmatch currentDatum $ \case
                        PIdle ->
                          pchallengeAcquisitionIsValid
                            inputs
                            availabilityPolicyId
                            psqRemoveUnavailable'challengeAssetName
                            (pfromData psqRemoveUnavailable'unavailableHeaderHash)
                            ptxInfo'validRange
                        current -> pcon current #== expectedLocked
                    )
                    ( ppairDataBuiltin
                        # pdata expectedLocked
                        # pdata
                          ( pmatch (pfromData psqRemoveUnavailable'removalApproach) $ \case
                              PPruneTimedOutBlockDescendant {} -> pconstant False
                              PRemoveTimedOutHead {} -> pconstant True
                          )
                    )
                    perror
                )
                perror
      _ -> perror

  expectedLocked <- plet $ pfromData $ (pmatch expectedAndTerminal $ \(PBuiltinPair pairFirst _) -> pairFirst)
  terminal <- plet $ pfromData $ (pmatch expectedAndTerminal $ \(PBuiltinPair _ pairSecond) -> pairSecond)
  PTxInInfo {ptxInInfo'resolved = ownOutput} <- pmatch ownInput
  PTxOut {ptxOut'address = ownAddress, ptxOut'value = ownValue} <- pmatch ownOutput
  lockOutput <- plet $ puniqueOutput # outputs # hubOraclePolicyId # ownAddress
  PTxOut {ptxOut'address = lockAddress, ptxOut'value = lockValue} <- pmatch lockOutput
  pand'List
    [ Value.pvalueOf
        # pto (pfromData ptxInfo'mint)
        # pfromData hubOraclePolicyId
        # pfromData passetName
        #== 0
    , pdatumTransitionIsValid
        # currentDatum
        # (pdecodeDatum # lockOutput)
        # expectedLocked
        # terminal
    , lockAddress #== ownAddress
    , lockValue #== ownValue
    ]

correctionLockSpendValidator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
correctionLockSpendValidator = plam $ \hubOraclePolicyId availabilityPolicyId ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  currentDatum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust datum ->
            pfromData $ punsafeCoerce @(PAsData PCorrectionLockDatum) $ pto $ pfromData datum
          PDNothing -> perror
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'mint, ptxInfo'redeemers} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  hubPolicy <- plet $ punsafeCoerce @(PAsData PCurrencySymbol) hubOraclePolicyId
  ownInput <-
    plet $
      pfromData $
        pheadSingleton
          #$ pfilter
          # plam
            ( \inputData ->
                pmatch (pfromData inputData) $ \PTxInInfo {ptxInInfo'outRef} ->
                  ptxInInfo'outRef #== ownRef
            )
          # inputs
  PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch ownInput
  PTxOut {ptxOut'address = ownAddress} <- pmatch ptxInInfo'resolved
  authenticatedOwnInput <- plet $ puniqueInput # inputs # hubPolicy # ownAddress
  PTxInInfo {ptxInInfo'outRef = authenticatedRef, ptxInInfo'resolved = authenticatedOutput} <-
    pmatch authenticatedOwnInput
  redeemer <-
    plet $ pfromData $ punsafeCoerceOwnRedeemer @PCorrectionLockRedeemer pscriptContext'redeemer
  pif
    ( pand'List
        [ ptxInInfo'outRef #== ownRef
        , authenticatedRef #== ownRef
        , pdecodeDatum # authenticatedOutput #== currentDatum
        , pmatch redeemer $ \case
            PCorrect hubRefInputIndex ->
              pcorrectionTransition
                currentDatum
                authenticatedOwnInput
                hubPolicy
                availabilityPolicyId
                ( Hub.pgetDatum
                    # referenceInputs
                    # hubOraclePolicyId
                    # pfromData hubRefInputIndex
                )
                pscriptContext'txInfo
            Correction.PDeinit hubInputIndex ->
              pmatch
                (phubDatumFromSpentInput inputs hubOraclePolicyId (pfromData hubInputIndex))
                $ \PHubOracleDatum {phubOracle'stateQueue} ->
                  pand'List
                    [ pstateQueueMintRedeemer redeemers phubOracle'stateQueue #== pcon StateQueue.PDeinit
                    , currentDatum #== pcon PIdle
                    , Value.pvalueOf
                        # pto (pfromData ptxInfo'mint)
                        # pfromData hubPolicy
                        # pfromData passetName
                        #== (-1)
                    , phasNoOutput # outputs # hubPolicy
                    ]
        ]
    )
    (pconstant ())
    perror
