{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Game
Description : Validation dispute game-turn validator.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Game (
  PGameActionV1 (..),
  gameV1Validator,
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
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetInclusiveBoundsOfAShortValidityRange, phasSigned)
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationDispute (
  PDisputeTurn (PReadyForOneStep),
  PDisputeWinner (PChallengerWins),
  PValidationDisputeV1 (..),
  previalChallengerMidpoint,
  previalOperatorMidpoint,
  ptimeoutWinner,
 )
import Midgard.ValidationGame (PValidationGameStateV1 (..))
import Midgard.ValidationTrace (PValidationTraceProof)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PGameActionV1 (s :: S)
  = PRevealOperator
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationTraceProof))
  | PRevealChallenger
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationTraceProof))
  | PEnterResolution
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  | PEnterChallengerTimeout
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PGameActionV1)

gameV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
gameV1Validator = plam $ \boundaryScriptHash timeoutScriptHash computationThreadPolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PGameActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
    \action ->
      plet
        ( pmatch action $ \case
            PRevealOperator inputIndex _ _ -> pfromData inputIndex
            PRevealChallenger inputIndex _ _ -> pfromData inputIndex
            PEnterResolution inputIndex _ -> pfromData inputIndex
            PEnterChallengerTimeout inputIndex _ -> pfromData inputIndex
        )
        $ \inputIndex ->
      plet
        ( pmatch action $ \case
            PRevealOperator _ outputIndex _ -> pfromData outputIndex
            PRevealChallenger _ outputIndex _ -> pfromData outputIndex
            PEnterResolution _ outputIndex -> pfromData outputIndex
            PEnterChallengerTimeout _ outputIndex -> pfromData outputIndex
        )
        $ \outputIndex ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'validRange} ->
      pcontinue
        computationThreadPolicyId
        (pexpectDatum datum)
        inputIndex outputIndex ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
        $ \inputScriptHash _assetName fraudProver inputState outputScriptHash outputState ->
          pmatch (pexpectStateAs @PValidationGameStateV1 inputState) $ \state ->
          plet (pfromData $ pvalidationGame'dispute state) $ \dispute ->
          plet
            ( pforgetData $ pdata $ pcon $ PValidationGameStateV1
                (pvalidationGame'challengedHeaderHash state)
                (pvalidationGame'operatorVkey state)
                (pvalidationGame'dispute state)
            )
            $ \sameState ->
          pmatch action $ \case
            PRevealOperator _ _ proofD ->
              let (_, currentTimeUpper) =
                    pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce ptxInfo'validRange)
               in pif
                    (phasSigned # pvalidationGame'operatorVkey state # pfromData ptxInfo'signatories)
                    ( plet (previalOperatorMidpoint # dispute # pfromData proofD # currentTimeUpper) $ \nextDispute ->
                      plet
                        ( pcon $ PValidationGameStateV1
                            (pvalidationGame'challengedHeaderHash state)
                            (pvalidationGame'operatorVkey state)
                            (pdata nextDispute)
                        )
                        $ \expectedState ->
                          pfromData outputScriptHash #== pfromData inputScriptHash
                            #&& outputState #== pforgetData (pdata expectedState)
                    )
                    perror
            PRevealChallenger _ _ proofD ->
              let (_, currentTimeUpper) =
                    pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce ptxInfo'validRange)
               in pif
                    (phasSigned # fraudProver # pfromData ptxInfo'signatories)
                    ( plet (previalChallengerMidpoint # dispute # pfromData proofD # currentTimeUpper) $ \nextDispute ->
                      plet
                        ( pcon $ PValidationGameStateV1
                            (pvalidationGame'challengedHeaderHash state)
                            (pvalidationGame'operatorVkey state)
                            (pdata nextDispute)
                        )
                        $ \expectedState ->
                          pfromData outputScriptHash #== pfromData inputScriptHash
                            #&& outputState #== pforgetData (pdata expectedState)
                    )
                    perror
            PEnterResolution _ _ ->
              pmatch dispute $ \disputeFields ->
                pfromData (pdispute'turn disputeFields) #== pcon PReadyForOneStep
                  #&& pfromData outputScriptHash #== pfromData boundaryScriptHash
                  #&& outputState #== sameState
            PEnterChallengerTimeout _ _ ->
              let (currentTimeLower, _) =
                    pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce ptxInfo'validRange)
               in ptimeoutWinner # dispute # currentTimeLower #== pcon PChallengerWins
                    #&& pfromData outputScriptHash #== pfromData timeoutScriptHash
                    #&& outputState #== sameState
