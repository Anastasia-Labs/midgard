module Midgard.Validators.FraudProofs.NativeScriptDecoding.Step01 (
  nativeScriptDecodingStep01Validator,
) where

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.NativeScriptDecoding.Engine (
  PBindStateV1 (..),
  pdirectionWrongfulAcceptance,
  pdirectionWrongfulRejection,
  psourceKindForced,
  psourceKindNormal,
 )
import Midgard.FraudProofs.NativeScriptDecoding.Step01 (PStep01Args (..))
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )

nativeScriptDecodingStep01Validator ::
  forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptDecodingStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PBindNormalTransaction carriageD -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          ppassNativeTxToNextStepCarried
            computationThreadPolicy
            hubOracle
            datum
            (pfromData carriageD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
              PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
              PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
              expectedState <- plet $ pcon $ PBindStateV1
                (pdata pdirectionWrongfulAcceptance) (pdata psourceKindNormal) (pdata badTxId)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (pcompact'validityCode #== 0) $
                  pexpecting (outputScriptHash #== step02ScriptHash) $
                    pexpecting
                      (outputStateData #== pforgetData (pdata expectedState))
                      (pconstant True)
        PRecordForcedSource directionD inputIndexD outputIndexD -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
          direction <- plet $ pfromData directionD
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData ->
              plet (pcon $ PBindStateV1 (pdata direction) (pdata psourceKindForced) (pdata $ pconstant "")) $ \expectedState ->
                pexpecting (pstateIsAbsent inputState) $
                  pexpecting (direction #== pdirectionWrongfulAcceptance #|| direction #== pdirectionWrongfulRejection) $
                    pexpecting (outputScriptHash #== step02ScriptHash) $
                      pexpecting
                        (outputStateData #== pforgetData (pdata expectedState))
                        (pconstant True)
