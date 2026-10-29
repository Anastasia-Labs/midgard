-- | Native and foreign payload checks for the authenticated item dispatcher.
module Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeItemYields (nativeValidator, foreignValidator) where

import Midgard.PhaseANativeItem qualified as Item
import Midgard.PhaseANativeItemYield qualified as Yield
import Midgard.ValidationResolution qualified as Resolution
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

nativeValidator, foreignValidator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
nativeValidator = payloadValidator True
foreignValidator = payloadValidator False

payloadValidator :: forall s. Bool ->
  Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
payloadValidator native = plam $ \dispatcher award certificate ctx -> pmatch ctx $ \c ->
  pmatch (pscriptContext'scriptInfo c) $ \case
    PRewardingScript _ -> plet (pscriptContext'txInfo c) $ \tx -> pmatch tx $ \t ->
      pmatch (Yield.puniqueDispatch # pfromData dispatcher # tx) $ \(Yield.PDispatch prepared action) ->
      pmatch prepared $ \p -> pmatch (pfromData $ Resolution.pprepared'resolution p) $ \r ->
      pmatch action $ \a ->
      plet (pcon $ PMachineFieldDoorV1 (pfromData $ ptxInfo'referenceInputs t) certificate) $ \door ->
        pif
          (Yield.pwinningOutput # tx # pfromData (Yield.pitem'outputIndex a) # pfromData award
            #&& (if native then Item.pnative else Item.pforeign)
              # pfromData (Resolution.presolution'preState r)
              # pfromData (Yield.pitem'transition a) # door # pfromData (Yield.pitem'carriage a))
          (pconstant ()) perror
    _ -> perror
