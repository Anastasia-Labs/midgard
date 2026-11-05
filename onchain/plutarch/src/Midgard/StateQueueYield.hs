{-# LANGUAGE OverloadedStrings #-}

-- | The role-NFT and zero-withdrawal handshake shared by operational policies.
module Midgard.StateQueueYield (
  prequireAuthenticatedZeroYield,
  pgetYieldedMintRedeemer,
) where

import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusCore.Data qualified as Data

import Midgard.Common.Utils (pgetUniqueMintRedeemer, pgetUniqueWithdrawRedeemer, pheadSingleton)

{- | Authenticate exactly one token under the deployment policy. Other policies
may coexist, but another role under this policy must not authorize the arm.
-}
prequireAuthenticatedZeroYield ::
  forall s.
  Term s (PTxInfo :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PScriptHash)
prequireAuthenticatedZeroYield = phoistAcyclic $ plam $ \tx authPolicy role index -> P.do
  PTxInfo {ptxInfo'referenceInputs, ptxInfo'wdrl, ptxInfo'redeemers} <- pmatch tx
  PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData $ pelemAt # index # pfromData ptxInfo'referenceInputs
  PTxOut {ptxOut'value, ptxOut'referenceScript} <- pmatch ptxInInfo'resolved
  scriptHash <- plet $ pmatch ptxOut'referenceScript $ \case
    PDJust hash -> pfromData hash
    PDNothing -> perror
  tokens <- plet $ pmatch (AssocMap.plookup # authPolicy # pto (pto $ pfromData ptxOut'value)) $ \case
    PJust tokenMap -> pto $ pto tokenMap
    PNothing -> perror
  PBuiltinPair candidate quantity <- pmatch $ pheadSingleton # tokens
  withdrawals <-
    plet $
      pfilter
        # plam
          ( \entry ->
              pmatch (pfromData $ pfstBuiltin # entry) $ \case
                PScriptCredential hash -> pfromData hash #== scriptHash
                _ -> pconstant False
          )
        # pto (pto $ pfromData ptxInfo'wdrl)
  PBuiltinPair _ amount <- pmatch $ pheadSingleton # withdrawals
  pif
    (pfromData candidate #== role #&& pfromData quantity #== 1 #&& pto (pfromData amount) #== 0)
    (plet (pgetUniqueWithdrawRedeemer # pto (pto $ pfromData ptxInfo'redeemers) # pto scriptHash) $ \_ -> scriptHash)
    perror

{- | Rewarding validators take a fieldless payload and obtain the operation
only from the unique mint redeemer of their applied policy.
-}
pgetYieldedMintRedeemer ::
  forall s.
  Term s (PScriptContext :--> PCurrencySymbol :--> PAsData PRedeemer)
pgetYieldedMintRedeemer = phoistAcyclic $ plam $ \ctx policy -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PRewardingScript _ ->
      pif
        (pto pscriptContext'redeemer #== pconstant (Data.Constr 0 []))
        ( pmatch pscriptContext'txInfo $ \PTxInfo {ptxInfo'redeemers} ->
            pgetUniqueMintRedeemer # pto (pto $ pfromData ptxInfo'redeemers) # policy
        )
        perror
    _ -> perror
