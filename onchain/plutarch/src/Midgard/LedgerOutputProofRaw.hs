{-# LANGUAGE OverloadedStrings #-}

{- | The target's canonical output-proof frame grammar. The carrier authenticates
these bytes; individual yields decode only the sub-control they consume.
-}
module Midgard.LedgerOutputProofRaw (
  PFrame,
  pitemCount,
  pspanWindowIndex,
  pfirstFactIndex,
  popen,
  pencode,
  pencodeItem,
  preplace,
  pitem,
  pinteger,
  pbytes,
  pspanWindow,
  pspanWindowData,
  pfact,
  pfactData,
) where

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteArrayHeader)
import Midgard.LedgerOutputProof (PLedgerOutputSpanWindowV1 (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

newtype PFrame s = PFrame (Term s (PBuiltinList PData))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveNewtypePlutusType PFrame)

pitemCount, pspanWindowIndex, pfirstFactIndex :: forall s. Term s PInteger
pitemCount = 17
pspanWindowIndex = 12
pfirstFactIndex = 13

pencodeItem :: forall s. Term s (PData :--> PByteString)
pencodeItem = phoistAcyclic $ pfix $ \self -> plam $ \dat ->
  pforce $
    pchooseData
      # dat
      # pdelay
        ( pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
            pif (tag #== 1 #&& pnull # fields) (pconstant "\xd8\x7a\x80") $
              pif
                (tag #== 0 #&& plength # fields #== 1)
                (pconstant "\xd8\x79\x9f" <> (self # (phead # fields)) <> pconstant "\xff")
                perror
        )
      # pdelay perror
      # pdelay
        ( plet (pasList # dat) $ \items ->
            (pencodeDefiniteArrayHeader # (plength # items))
              <> (pfoldr # plam (\item rest -> (self # item) <> rest) # pconstant "" # items)
        )
      # pdelay (pserialiseData # dat)
      # pdelay (pserialiseData # dat)

pencode :: forall s. Term s (PFrame :--> PByteString)
pencode = phoistAcyclic $ plam $ \frame -> pmatch frame $ \(PFrame items) ->
  pconstant "\x91" <> (pfoldr # plam (\item rest -> (pencodeItem # item) <> rest) # pconstant "" # items)

popen :: forall s. Term s (PByteString :--> PFrame)
popen = phoistAcyclic $ plam $ \cbor -> pmatch (pdeserialise # cbor) $ \case
  PNothing -> perror
  PJust dat -> plet (pasList # dat) $ \items ->
    plet (pcon $ PFrame items) $ \frame ->
      pif
        ( plength
            # items
            #== pitemCount
            #&& pinteger
            # frame
            # 0
            #== 1
            #&& pinteger
            # frame
            # 1
            #>= 0
            #&& pinteger
            # frame
            # 1
            #<= 6
            #&& pinteger
            # frame
            # 2
            #>= 0
            #&& pinteger
            # frame
            # 3
            #> 0
            #&& plengthBS
            # (pbytes # frame # 4)
            #== 32
            #&& pencode
            # frame
            #== cbor
        )
        frame
        perror

pitem :: forall s. Term s (PFrame :--> PInteger :--> PData)
pitem = phoistAcyclic $ plam $ \frame index -> pmatch frame $ \(PFrame items) ->
  pif (index #>= 0) (pelemAt # index # items) perror

pinteger :: forall s. Term s (PFrame :--> PInteger :--> PInteger)
pinteger = phoistAcyclic $ plam $ \frame index -> pasInt # (pitem # frame # index)

pbytes :: forall s. Term s (PFrame :--> PInteger :--> PByteString)
pbytes = phoistAcyclic $ plam $ \frame index -> pasByteStr # (pitem # frame # index)

preplace :: forall s. Term s (PFrame :--> PInteger :--> PData :--> PFrame)
preplace = phoistAcyclic $ plam $ \frame index value -> pmatch frame $ \(PFrame items) ->
  pif
    (index #>= 0 #&& index #< pitemCount)
    ( pcon $
        PFrame $
          ( pfix $ \self -> plam $ \current rest ->
              pelimList (\head tail -> pcons # (pif (current #== index) value head) # (self # (current + 1) # tail)) pnil rest
          )
            # 0
            # items
    )
    perror

pspanWindow :: forall s. Term s (PFrame :--> PMaybeData PLedgerOutputSpanWindowV1)
pspanWindow = phoistAcyclic $ plam $ \frame ->
  pmatch (pasConstr # (pitem # frame # pspanWindowIndex)) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1) (pif (pnull # fields) (pcon PDNothing) perror) $
      pif
        (tag #== 0 #&& plength # fields #== 1)
        ( plet (pasList # (phead # fields)) $ \record ->
            pif
              (plength # record #== 3)
              ( pcon $
                  PDJust $
                    pdata $
                      pcon $
                        PLedgerOutputSpanWindowV1
                          (pdata $ pasInt # (pelemAt # 0 # record))
                          (pdata $ pasInt # (pelemAt # 1 # record))
                          (pdata $ pasByteStr # (pelemAt # 2 # record))
              )
              perror
        )
        perror

pspanWindowData :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PData)
pspanWindowData = phoistAcyclic $ plam $ \start length digest ->
  pforgetData $
    pconstrBuiltin
      # 0
      # ( pcons
            # ( plistData
                  # ( pcons
                        # pforgetData (pdata start)
                        # (pcons # pforgetData (pdata length) # (pcons # pforgetData (pdata digest) # pnil))
                    )
              )
            # pnil
        )

pfact :: forall s. Term s (PFrame :--> PInteger :--> PMaybeData PByteString)
pfact = phoistAcyclic $ plam $ \frame role ->
  pif
    (role #>= 0 #&& role #< 4)
    ( pmatch (pasConstr # (pitem # frame # (pfirstFactIndex + role))) $ \(PBuiltinPair tag fields) ->
        pif (tag #== 1) (pif (pnull # fields) (pcon PDNothing) perror) $
          pif
            (tag #== 0 #&& plength # fields #== 1)
            (pcon $ PDJust $ pdata $ pasByteStr # (phead # fields))
            perror
    )
    perror

pfactData :: forall s. Term s (PByteString :--> PData)
pfactData = phoistAcyclic $ plam $ \commitment ->
  pforgetData $ pconstrBuiltin # 0 # (pcons # pforgetData (pdata commitment) # pnil)
