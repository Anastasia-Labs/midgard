module Midgard.Validators.FraudProofs.NativeScriptDecoding.Step04 (
  nativeScriptDecodingStep04Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize)
import Midgard.FraudProofs.NativeScriptDecoding.Engine
import Midgard.FraudProofs.NativeScriptDecoding.Step04 (PArgs (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

nativeScriptDecodingStep04Validator ::
  forall s.
  Term s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptDecodingStep04Validator = plam $
  \computationThreadPolicy fraudProofPolicy fraudProofAddress ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PArgs computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
        PArgs
          { pargs'inputIndex
          , pargs'outputIndex
          , pargs'fraudProofMintRedeemerIndex
          } <- pmatch args
        PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
        pfinalize
          computationThreadPolicy
          fraudProofPolicy
          fraudProofAddress
          (pexpectDatum datum)
          (pfromData pargs'inputIndex)
          (pfromData pargs'outputIndex)
          (pfromData pargs'fraudProofMintRedeemerIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          (pto $ pto $ pfromData ptxInfo'redeemers)
          $ \_ownScriptHash _threadName _prover inputState -> P.do
            state <- plet $ pexpectStateAs @PScanThreadStateV1 inputState
            st <- pmatch state
            pif
              (pfromData (pscanState'direction st) #== pdirectionWrongfulRejection)
              ( pexpecting
                  ( pfromData (pscanState'sourceKind st) #== psourceKindForced
                      #&& pfromData (pscanState'refusalClass st) #== prefusalClassMalformed
                      #&& prefusalClassInDomain # pfromData (pscanState'scanReasonClass st)
                  )
                  (pconstant True)
              )
              ( pexpecting
                  ( pfromData (pscanState'direction st) #== pdirectionWrongfulAcceptance
                      #&& prefusalClassInDomain # pfromData (pscanState'refusalClass st)
                  )
                  (pconstant True)
              )

prefusalClassInDomain :: forall s. Term s (PInteger :--> PBool)
prefusalClassInDomain = phoistAcyclic $ plam $ \refusalClass ->
  refusalClass #== prefusalClassMalformed
    #|| refusalClass #== prefusalClassNodeLimit
    #|| refusalClass #== prefusalClassDepthLimit
