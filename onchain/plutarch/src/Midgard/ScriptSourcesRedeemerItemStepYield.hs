{-# LANGUAGE OverloadedStrings #-}

-- | Shared rewarding validator for ScriptSources descriptor header/tail steps.
module Midgard.ScriptSourcesRedeemerItemStepYield (
  redeemerItemStepRole,
  redeemerItemStepValidator,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude

import Midgard.ScriptSourcesDescriptorWire (pdecodeClaim)
import Midgard.ScriptSourcesLateRawSemantics (pverifyDescriptorStepClaim)
import Midgard.ValidationSemanticYield (
  PDispatch (..),
  puniqueSemanticDispatchV1,
 )

redeemerItemStepRole :: forall s. Term s PTokenName
redeemerItemStepRole = pcon $ PTokenName $ pconstant "V1VtSsRedeemerItemStepYield"

redeemerItemStepValidator ::
  forall s.
  Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
redeemerItemStepValidator = plam $ \dispatchers ctx ->
  pmatch ctx $ \PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} ->
    pmatch pscriptContext'scriptInfo $ \case
      PRewardingScript _ ->
        pmatch (puniqueSemanticDispatchV1 # pfromData dispatchers # pscriptContext'txInfo) $ \dispatch ->
          plet (pdispatch'extra dispatch) $ \extra ->
            pif
              (plength # extra #== 2)
              ( plet (pelemAt # 0 # extra) $ \rawClaim ->
                  plet
                    (pdecodeClaim # rawClaim)
                    $ \claim ->
                      pif
                        ( pforgetData (pdata claim)
                            #== rawClaim
                            #&& pverifyDescriptorStepClaim
                            # pdispatch'transition dispatch
                            # claim
                        )
                        (pconstant ())
                        perror
              )
              perror
      _ -> perror
