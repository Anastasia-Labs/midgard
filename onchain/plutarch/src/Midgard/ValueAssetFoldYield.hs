{-# LANGUAGE OverloadedStrings #-}

-- | One zero withdrawal discharges one exact asset-fold spending claim.
module Midgard.ValueAssetFoldYield (validator, prole) where

import Midgard.Common.Utils (pheadSingleton)
import Midgard.MpfProof.Types qualified as Mpf
import Midgard.ValidationMachine
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Midgard.ValidationResolutionData (bytesField, enumField, integerField, recordFields)
import Midgard.ValueAssetFold qualified as Fold
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

prole :: forall s. Term s PTokenName
prole = pcon $ PTokenName $ pconstant "V1VtVamAssetFoldYield"

validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
validator = plam $ \replay output mint ctx -> P.do
  PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PRewardingScript _ -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
      PTxInInfo{ptxInInfo'outRef, ptxInInfo'resolved} <-
        pmatch $
          pfromData $
            pheadSingleton
              # ( pfilter
                    # plam
                      ( \input -> P.do
                          PTxInInfo{ptxInInfo'resolved = resolved} <- pmatch $ pfromData input
                          PTxOut{ptxOut'address} <- pmatch resolved
                          PAddress credential _ <- pmatch ptxOut'address
                          pmatch credential $ \case
                            PScriptCredential hash -> hash #== replay #|| hash #== output #|| hash #== mint
                            _ -> pconstant False
                      )
                    # pfromData ptxInfo'inputs
                )
      PTxOut{ptxOut'address} <- pmatch ptxInInfo'resolved
      PAddress credential _ <- pmatch ptxOut'address
      let isMint = credential #== pcon (PScriptCredential mint)
          isOutput = credential #== pcon (PScriptCredential output)
      PBuiltinPair _ redeemer <-
        pmatch $
          pheadSingleton
            # ( pfilter
                  # plam (\pair -> pmatch pair $ \(PBuiltinPair purpose _) -> pfromData purpose #== pcon (PSpending ptxInInfo'outRef))
                  # (pto $ pto $ pfromData ptxInfo'redeemers)
              )
      action <- plet $ pmatch (pasConstr # pto (pfromData redeemer)) $ \(PBuiltinPair tag fields) ->
        pif (tag #== 1 #&& plength # fields #== 1) (phead # fields) perror
      raw <- plet $ pmatch (pasConstr # action) $ \(PBuiltinPair tag fields) -> pif (tag #== 0) (phead # fields) perror
      claim <- plet $ decodeClaim raw
      pmatch claim $ \c ->
        pif
          ( pforgetData (pdata claim)
              #== raw
              #&& (pmatch (pfromData $ Fold.pclaim'descriptor c) $ \case PDNothing -> isMint; PDJust _ -> pnot # isMint)
              #&& Fold.pverifyClaim
              # claim
              # isOutput
          )
          (pconstant ())
          perror
    _ -> perror

bytesList :: forall s. Term s PData -> Term s (PAsData (PBuiltinList (PAsData PByteString)))
bytesList raw = pdata $ pmap # plam (\x -> pdata $ pasByteStr # x) # (pasList # raw)

peaks :: forall s. Term s PData -> Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
peaks raw = pdata $ pmap # plam (\x -> plet (recordFields 2 x) $ \f -> pdata $ pcon $ PFrontierPeak (integerField f 0) (bytesField f 1)) # (pasList # raw)

decodeValue :: forall s. Term s PData -> Term s PValueAccumulatorV1
decodeValue raw = plet (recordFields 4 raw) $ \f -> pcon $ PValueAccumulatorV1 (integerField f 0) (bytesField f 1) (integerField f 2) (integerField f 3)

decodeProofStep :: forall s. Term s PData -> Term s Mpf.PProofStep
decodeProofStep raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
  pif (tag #== 0 #&& plength # f #== 2) (pcon $ Mpf.PBranch (integerField f 0) (bytesField f 1))
    $ pif
      (tag #== 1 #&& plength # f #== 2)
      (plet (recordFields 3 $ pelemAt # 1 # f) $ \n -> pcon $ Mpf.PFork (integerField f 0) (pdata $ pcon $ Mpf.PNeighbor (integerField n 0) (bytesField n 1) (bytesField n 2)))
    $ pif (tag #== 2 #&& plength # f #== 3) (pcon $ Mpf.PLeaf (integerField f 0) (bytesField f 1) (bytesField f 2)) perror

decodeMutation :: forall s. Term s PData -> Term s PValueAssetMutationWitnessV1
decodeMutation raw = plet (recordFields 3 raw) $ \f ->
  pcon $
    PValueAssetMutationWitnessV1
      (enumField 2 $ pelemAt # 0 # f)
      (integerField f 1)
      (pdata $ pmap # plam (\x -> pdata $ decodeProofStep x) # (pasList # (pelemAt # 2 # f)))

decodeOutcome :: forall s. Term s PData -> Term s PValueAccumulatorUpdateV1
decodeOutcome raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
  pif (tag #== 0 #&& plength # f #== 1) (pcon $ PValueAccumulatorUpdated $ pdata $ decodeValue $ phead # f) $
    pif (tag #== 1 #&& pnull # f) (pcon PValueAccumulatorAssetLimitExceeded) $
      pif (tag #== 2 #&& pnull # f) (pcon PValueAccumulatorMutationInvalid) perror

decodeDescriptor :: forall s. Term s PData -> Term s Fold.PDescriptorClaim
decodeDescriptor raw = plet (recordFields 5 raw) $ \f ->
  pcon $
    Fold.PDescriptorClaim
      (bytesField f 0)
      (integerField f 1)
      (peaks $ pelemAt # 2 # f)
      (bytesList $ pelemAt # 3 # f)
      (integerField f 4)

decodeMaybeDescriptor :: forall s. Term s PData -> Term s (PMaybeData Fold.PDescriptorClaim)
decodeMaybeDescriptor raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
  pif (tag #== 0 #&& plength # f #== 1) (pcon $ PDJust $ pdata $ decodeDescriptor $ phead # f) $
    pif (tag #== 1 #&& pnull # f) (pcon PDNothing) perror

decodeClaim :: forall s. Term s PData -> Term s Fold.PClaim
decodeClaim raw = plet (recordFields 7 raw) $ \f ->
  pcon $
    Fold.PClaim
      (bytesField f 0)
      (bytesField f 1)
      (integerField f 2)
      (pdata $ decodeMutation $ pelemAt # 3 # f)
      (pdata $ decodeValue $ pelemAt # 4 # f)
      (pdata $ decodeOutcome $ pelemAt # 5 # f)
      (pdata $ decodeMaybeDescriptor $ pelemAt # 6 # f)
