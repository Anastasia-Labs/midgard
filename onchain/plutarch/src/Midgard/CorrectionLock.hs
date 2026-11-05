{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.CorrectionLock
Description : Plutarch port of @lib/midgard/correction-lock.ak@.

The correction lock is a deployment-bound singleton outside the state-queue
linked-list namespace. Append and merge transactions reference it in @Idle@;
correction transactions spend it and hold the exact correction identity until
the terminal removal clears it again.
-}
module Midgard.CorrectionLock (
  passetName,
  PCorrectionIdentity (..),
  PCorrectionLockDatum (..),
  PCorrectionLockRedeemer (..),
  poutputHasToken,
  poutputIsDedicated,
  pdecodeDatum,
  puniqueInput,
  puniqueOutput,
  preferencesIdle,
  phasNoOutput,
  pdatumTransitionIsValid,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  POutputDatum (..),
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.LedgerApi.Value (padaSymbol)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pheadSingleton)

-- | Aiken @correction_lock.asset_name@.
passetName :: forall s. Term s (PAsData PTokenName)
passetName = pdata (pcon (PTokenName (pconstant "MIDGARD_CORRECTION_LOCK")))

-- | The authenticated reason for the correction holding the singleton.
data PCorrectionIdentity (s :: S)
  = PFraudProof (Term s (PAsData PTokenName))
  | PAttestationTimeout
  | PAvailabilityChallenge (Term s (PAsData PTokenName))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCorrectionIdentity)

-- | Aiken @correction_lock.Datum@.
data PCorrectionLockDatum (s :: S)
  = PIdle
  | PLocked
      (Term s (PAsData PByteString))
      (Term s (PAsData PCorrectionIdentity))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCorrectionLockDatum)

-- | Aiken @correction_lock.Redeemer@.
data PCorrectionLockRedeemer (s :: S)
  = PCorrect (Term s (PAsData PInteger))
  | PDeinit (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCorrectionLockRedeemer)

-- | Whether an output carries exactly one correction-lock token at the named policy.
poutputHasToken :: forall s.
  Term s (PTxOut :--> PAsData PCurrencySymbol :--> PBool)
poutputHasToken = phoistAcyclic $
  plam $ \output hubOraclePolicyId ->
    pmatch output $ \PTxOut {ptxOut'value} ->
      Value.pvalueOf
        # pto (pfromData ptxOut'value)
        # pfromData hubOraclePolicyId
        # pfromData passetName
        #== 1

-- | Aiken @assets.without_lovelace(value) == assets.from_asset(...)@ without partial matches.
pvalueHasOnlyLockTokenApartFromAda :: forall s.
  Term s (PTxOut :--> PAsData PCurrencySymbol :--> PBool)
pvalueHasOnlyLockTokenApartFromAda = phoistAcyclic $
  plam $ \output hubOraclePolicyId ->
    pmatch output $ \PTxOut {ptxOut'value} -> P.do
      entries <- plet $ pto (pto (pto (pto (pfromData ptxOut'value))))
      nonAda <- plet $
        pmatch entries $ \case
          PNil -> pnil
          PCons entry rest ->
            pif
              (pfromData (pmatch entry $ \(PBuiltinPair pairFirst _) -> pairFirst) #== padaSymbol)
              rest
              entries
      pmatch nonAda $ \case
        PNil -> pconstant False
        PCons policyEntry remainingPolicies ->
          plet (pto (pto (pfromData (pmatch policyEntry $ \(PBuiltinPair _ pairSecond) -> pairSecond)))) $ \tokens ->
            pand'List
              [ pnull # remainingPolicies
              , (pmatch policyEntry $ \(PBuiltinPair pairFirst _) -> pairFirst) #== hubOraclePolicyId
              , tokens
                  #== psingleton
                    # (ppairDataBuiltin # passetName # pdata 1)
              ]

-- | Require the singleton output's exact address/value/reference-script shape.
poutputIsDedicated :: forall s.
  Term s (PTxOut :--> PAsData PCurrencySymbol :--> PAddress :--> PBool)
poutputIsDedicated = phoistAcyclic $
  plam $ \output hubOraclePolicyId correctionLockAddress ->
    pmatch output $ \PTxOut {ptxOut'address, ptxOut'referenceScript} ->
      pand'List
        [ ptxOut'address #== correctionLockAddress
        , poutputHasToken # output # hubOraclePolicyId
        , pvalueHasOnlyLockTokenApartFromAda # output # hubOraclePolicyId
        , pmatch ptxOut'referenceScript $ \case
            PDNothing -> pconstant True
            PDJust _ -> pconstant False
        ]

-- | Decode the required inline correction-lock datum.
pdecodeDatum :: forall s. Term s (PTxOut :--> PCorrectionLockDatum)
pdecodeDatum = phoistAcyclic $
  plam $ \output ->
    pmatch output $ \PTxOut {ptxOut'datum} ->
      pmatch ptxOut'datum $ \case
        POutputDatum {poutputDatum'outputDatum} ->
          pfromData (punsafeCoerce @(PAsData PCorrectionLockDatum) (pto poutputDatum'outputDatum))
        _ -> perror

-- | Resolve the unique spent correction-lock singleton and authenticate its shape.
puniqueInput :: forall s.
  Term s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAddress
        :--> PTxInInfo
    )
puniqueInput = phoistAcyclic $
  plam $ \inputs hubOraclePolicyId correctionLockAddress -> P.do
    matching <-
      plet $
        pfilter
          # plam
            ( \inputData ->
                pmatch (pfromData inputData) $ \PTxInInfo {ptxInInfo'resolved} ->
                  poutputHasToken # ptxInInfo'resolved # hubOraclePolicyId
            )
          # inputs
    input <- plet $ pfromData (pheadSingleton # matching)
    PTxInInfo {ptxInInfo'resolved} <- pmatch input
    pif
      (poutputIsDedicated # ptxInInfo'resolved # hubOraclePolicyId # correctionLockAddress)
      input
      perror

-- | Resolve the unique continuing correction-lock singleton and authenticate its shape.
puniqueOutput :: forall s.
  Term s
    ( PBuiltinList (PAsData PTxOut)
        :--> PAsData PCurrencySymbol
        :--> PAddress
        :--> PTxOut
    )
puniqueOutput = phoistAcyclic $
  plam $ \outputs hubOraclePolicyId correctionLockAddress -> P.do
    matching <-
      plet $
        pfilter
          # plam
            ( \outputData ->
                poutputHasToken # pfromData outputData # hubOraclePolicyId
            )
          # outputs
    output <- plet $ pfromData (pheadSingleton # matching)
    pif
      (poutputIsDedicated # output # hubOraclePolicyId # correctionLockAddress)
      output
      perror

-- | Require the unique referenced singleton to be dedicated and @Idle@.
preferencesIdle :: forall s.
  Term s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAddress
        :--> PBool
    )
preferencesIdle = phoistAcyclic $
  plam $ \referenceInputs hubOraclePolicyId correctionLockAddress -> P.do
    matching <-
      plet $
        pfilter
          # plam
            ( \inputData ->
                pmatch (pfromData inputData) $ \PTxInInfo {ptxInInfo'resolved} ->
                  poutputHasToken # ptxInInfo'resolved # hubOraclePolicyId
            )
          # referenceInputs
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData (pheadSingleton # matching)
    pand'List
      [ poutputIsDedicated
          # ptxInInfo'resolved # hubOraclePolicyId # correctionLockAddress
      , pdecodeDatum # ptxInInfo'resolved #== pcon PIdle
      ]

-- | No output carries one correction-lock token under this policy.
phasNoOutput :: forall s.
  Term s (PBuiltinList (PAsData PTxOut) :--> PAsData PCurrencySymbol :--> PBool)
phasNoOutput = phoistAcyclic $
  plam $ \outputs hubOraclePolicyId ->
    pall
      # plam
        ( \outputData ->
            pnot # (poutputHasToken # pfromData outputData # hubOraclePolicyId)
        )
      # outputs

-- | Exact acquire/resume/terminal-clear state transition.
pdatumTransitionIsValid :: forall s.
  Term s
    ( PCorrectionLockDatum
        :--> PCorrectionLockDatum
        :--> PCorrectionLockDatum
        :--> PBool
        :--> PBool
    )
pdatumTransitionIsValid = phoistAcyclic $
  plam $ \current next expectedLocked terminal ->
    plet (pif terminal (pcon PIdle) expectedLocked) $ \expectedNext ->
      pand'List
        [ current #== pcon PIdle #|| current #== expectedLocked
        , next #== expectedNext
        ]
