{-# LANGUAGE OverloadedStrings #-}

-- | Exact single-dispatch authentication for the Phase-A native item yields.
module Midgard.PhaseANativeItemYield where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.Common.Utils (pheadSingleton)
import Midgard.NativeTxFieldAccess (PFieldCarriageV1 (..))
import Midgard.ValidationMachine (PValidationOneStepWitnessV1)
import Midgard.ValidationResolution (PPreparedValidationResolutionStateV1, pwinningResolution)
import Midgard.ValidationResolutionData (recordFields, integerField, bytesField, decodePrepared, decodeTransition)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

nativeRole, foreignRole :: forall s. Term s PTokenName
nativeRole = pcon $ PTokenName $ pconstant "V1VtPhaseANativeItemNativeYield"
foreignRole = pcon $ PTokenName $ pconstant "V1VtPhaseANativeItemForeignYield"

data PPhaseANativeItemActionV1 s = PVerifyItem
  { pitem'inputIndex :: Term s (PAsData PInteger)
  , pitem'outputIndex :: Term s (PAsData PInteger)
  , pitem'transition :: Term s (PAsData PValidationOneStepWitnessV1)
  , pitem'fieldIndex :: Term s (PAsData PInteger)
  , pitem'itemIndex :: Term s (PAsData PInteger)
  , pitem'carriage :: Term s (PAsData PFieldCarriageV1)
  , pitem'yieldToRefInputIndex :: Term s (PAsData PInteger)
  , pitem'yieldKind :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeItemActionV1)

pdecodeCarriage :: forall s. Term s PData -> Term s PFieldCarriageV1
pdecodeCarriage raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
  pif (tag #== 0 #&& plength # fields #== 1)
    (pcon $ PInline $ bytesField fields 0)
    (pif (tag #== 1 #&& plength # fields #== 1)
      (pcon $ PRawUtxo $ integerField fields 0)
      (pif (tag #== 2 #&& plength # fields #== 2)
        (pcon $ PCertified (integerField fields 0)
          (pdata $ pmap # plam (\value -> pdata $ pasInt # value) # (pasList # (pelemAt # 1 # fields)))) perror))

pdecodeAction :: forall s. Term s PData -> Term s PPhaseANativeItemActionV1
pdecodeAction raw = plet (recordFields 8 raw) $ \f ->
  pcon $ PVerifyItem (integerField f 0) (integerField f 1) (pdata $ decodeTransition $ pelemAt # 2 # f)
    (integerField f 3) (integerField f 4) (pdata $ pdecodeCarriage $ pelemAt # 5 # f) (integerField f 6) (integerField f 7)

data PDispatch s = PDispatch
  { pdispatch'state :: Term s PPreparedValidationResolutionStateV1
  , pdispatch'action :: Term s PPhaseANativeItemActionV1
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PDispatch)

puniqueDispatch :: forall s. Term s (PScriptHash :--> PTxInfo :--> PDispatch)
puniqueDispatch = phoistAcyclic $ plam $ \dispatcher tx -> P.do
  PTxInfo {ptxInfo'inputs, ptxInfo'redeemers} <- pmatch tx
  PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch $ pfromData $ pheadSingleton #
    (pfilter # plam (\input -> P.do
      PTxInInfo {ptxInInfo'resolved = resolved} <- pmatch $ pfromData input
      PTxOut {ptxOut'address} <- pmatch resolved
      PAddress credential _ <- pmatch ptxOut'address
      credential #== pcon (PScriptCredential $ pdata dispatcher)) # pfromData ptxInfo'inputs)
  PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
  rawDatum <- plet $ pmatch ptxOut'datum $ \case POutputDatum dat -> pto dat; _ -> perror
  PBuiltinPair _ rawRedeemer <- pmatch $ pheadSingleton #
    (pfilter # plam (\pair -> pmatch pair $ \(PBuiltinPair purpose _) ->
      pfromData purpose #== pcon (PSpending ptxInInfo'outRef)) # (pto $ pto $ pfromData ptxInfo'redeemers))
  datumFields <- plet $ recordFields 2 rawDatum
  stateRaw <- plet $ phead # recordFields 1 (pelemAt # 1 # datumFields)
  actionRaw <- plet $ pmatch (pasConstr # pto (pfromData rawRedeemer)) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1 #&& plength # fields #== 1) (phead # fields) perror
  state <- plet $ decodePrepared stateRaw
  action <- plet $ pdecodeAction actionRaw
  -- Equality forces every checked field, including fields this yield delegates
  -- to the spending validator; it adds no semantic/domain restriction.
  pif (pforgetData (bytesField datumFields 0) #== pelemAt # 0 # datumFields
      #&& pforgetData (pdata state) #== stateRaw #&& pforgetData (pdata action) #== actionRaw)
    (pcon $ PDispatch state action) perror

pwinningOutput :: forall s. Term s (PTxInfo :--> PInteger :--> PScriptHash :--> PBool)
pwinningOutput = phoistAcyclic $ plam $ \tx index award -> pmatch tx $ \t ->
  pmatch (pfromData $ pelemAt # index # pfromData (ptxInfo'outputs t)) $ \output ->
  pmatch (ptxOut'address output) $ \(PAddress credential _) ->
  credential #== pcon (PScriptCredential $ pdata award)
    #&& (pmatch (ptxOut'datum output) $ \case
      POutputDatum dat -> plet (recordFields 2 $ pto dat) $ \fields ->
        pforgetData (bytesField fields 0) #== pelemAt # 0 # fields
          #&& pelemAt # 1 # fields #== pforgetData (pconstrBuiltin # 0 # (pcons # pforgetData (pdata pwinningResolution) # pnil))
      _ -> perror)
