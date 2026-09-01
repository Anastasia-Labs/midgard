{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Boundary
Description : Validation-game one-step boundary resolver.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Boundary (
  PPrepareResolutionActionV1 (..),
  boundaryV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationGame (PValidationGameStateV1 (..))
import Midgard.ValidationResolution (
  PValidationBoundaryEvidenceV1 (..),
  pprepareValidationResolution,
  presolverCount,
  presolverIndex,
 )
import Midgard.ValidationResolver (pselectSemanticResolver)
import Midgard.ValidationTrace (PValidationMachineStateV1 (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PPrepareResolutionActionV1 (s :: S)
  = PPrepareResolution
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationBoundaryEvidenceV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPrepareResolutionActionV1)

boundaryV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
boundaryV1Validator = plam $ \resolverHashes computationThreadPolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPrepareResolutionActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PPrepareResolution inputIndex outputIndex resolverIndexD evidenceD) ->
      pmatch
        ( pselectSemanticResolver
            # pfromData resolverHashes # presolverCount # pfromData resolverIndexD
        )
        $ \case
          PNothing -> perror
          PJust resolverScriptHash ->
            pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
            pcontinue
              computationThreadPolicyId
              (pexpectDatum datum)
              (pfromData inputIndex) (pfromData outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
              $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
                pmatch (pexpectStateAs @PValidationGameStateV1 inputState) $ \state ->
                plet (pfromData evidenceD) $ \evidence ->
                pmatch evidence $ \evidenceFields ->
                plet (pfromData $ pboundary'preState evidenceFields) $ \pre ->
                pmatch pre $ \preState ->
                plet
                  ( pprepareValidationResolution
                      # pfromData (pvalidationGame'dispute state)
                      # pre
                      # pfromData (pboundary'operatorPost evidenceFields)
                      # pfromData (pboundary'challengerPost evidenceFields)
                  )
                  $ \expectedResolution ->
                    pfromData resolverIndexD
                      #== presolverIndex # pfromData (pmachineState'phase preState)
                      #&& pfromData outputScriptHash #== resolverScriptHash
                      #&& outputState #== pforgetData (pdata expectedResolution)
