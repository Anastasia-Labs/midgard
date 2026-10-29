{-# LANGUAGE OverloadedStrings #-}

-- | The target's paired stage-seven observer rewarding validators.
module Midgard.ScriptSourcesStageSevenYield (
  observerItemRole,
  observerBoundRole,
  observerItemValidator,
  observerBoundValidator,
) where

import Midgard.PhaseANativeItemYield (pdecodeCarriage)
import Midgard.ScriptSourcesStageSevenSemantics qualified as Semantics
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
 )
import Midgard.ValidationSemanticYield (
  PDispatch (..),
  puniqueSemanticDispatchV1,
 )
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

observerItemRole, observerBoundRole :: forall s. Term s PTokenName
observerItemRole = pcon $ PTokenName $ pconstant "V1VtSsS07ObserverItemYield"
observerBoundRole = pcon $ PTokenName $ pconstant "V1VtSsS07ObserverBoundYield"

observerItemValidator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
observerItemValidator = plam $ \dispatcher certificate ctx ->
  pmatch ctx $ \PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} ->
    pmatch pscriptContext'scriptInfo $ \case
      PRewardingScript _ ->
        pmatch pscriptContext'txInfo $ \tx ->
          pmatch
            ( puniqueSemanticDispatchV1
                # (pcons # dispatcher # pnil)
                # pscriptContext'txInfo
            )
            $ \dispatch ->
              plet (pdispatch'extra dispatch) $ \extra ->
                pif
                  (plength # extra #== 7)
                  ( plet (pelemAt # 2 # extra) $ \rawCarriage ->
                      plet (pdecodeCarriage rawCarriage) $ \carriage ->
                        pmatch (pdispatch'state dispatch) $ \state ->
                          pmatch (pfromData $ pprepared'resolution state) $ \resolution ->
                            pif
                              ( pforgetData (pdata carriage)
                                  #== rawCarriage
                                  #&& Semantics.pitemFacts
                                  # pfromData (presolution'preState resolution)
                                  # pdispatch'transition dispatch
                                  # (pcon $ PMachineFieldDoorV1 (pfromData $ ptxInfo'referenceInputs tx) certificate)
                                  # (pasInt # (pelemAt # 0 # extra))
                                  # (pasInt # (pelemAt # 1 # extra))
                                  # carriage
                                  # (pasByteStr # (pelemAt # 3 # extra))
                                  # (pasInt # (pelemAt # 4 # extra))
                              )
                              (pconstant ())
                              perror
                  )
                  perror
      _ -> perror

observerBoundValidator ::
  forall s.
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
observerBoundValidator = plam $ \dispatcher ctx ->
  pmatch ctx $ \PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} ->
    pmatch pscriptContext'scriptInfo $ \case
      PRewardingScript _ ->
        pmatch
          ( puniqueSemanticDispatchV1
              # (pcons # dispatcher # pnil)
              # pscriptContext'txInfo
          )
          $ \dispatch ->
            plet (pdispatch'extra dispatch) $ \extra ->
              pif
                (plength # extra #== 7)
                ( pmatch (pdispatch'state dispatch) $ \state ->
                    pmatch (pfromData $ pprepared'resolution state) $ \resolution ->
                      pif
                        ( Semantics.pbound
                            # pfromData (presolution'preState resolution)
                            # pdispatch'transition dispatch
                            # (pasByteStr # (pelemAt # 3 # extra))
                            # (pasInt # (pelemAt # 4 # extra))
                        )
                        (pconstant ())
                        perror
                )
                perror
      _ -> perror
