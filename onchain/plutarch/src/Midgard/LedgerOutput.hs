{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutput (
  pmaxOutputCanonicalCborBytes,
  pmaxDistinctAssetCount,
  pdecodeCanonicalAddressBytes,
  pdecodeCanonicalOutput,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Data (pasByteStr, pasInt, pasList, pasMap)
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CanonicalPlutusData (pisCanonicalPlutusDataV1)
import Midgard.FraudProofs.NativeTx.Codec (pbyteAt)
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxOutput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardCredential (..),
  PMidgardScriptLanguage (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PMidgardVersionedScript (..),
 )

type PAssetList = PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger))
type PParsedAssets = PPair PAssetList PInteger

pfst :: forall (s :: S) (a :: S -> Type) (b :: S -> Type). Term s (PBuiltinPair a b) -> Term s a
pfst pair = pmatch pair $ \(PBuiltinPair first _) -> first

psnd :: forall (s :: S) (a :: S -> Type) (b :: S -> Type). Term s (PBuiltinPair a b) -> Term s b
psnd pair = pmatch pair $ \(PBuiltinPair _ second) -> second

pmaxOutputCanonicalCborBytes :: forall (s :: S). Term s PInteger
pmaxOutputCanonicalCborBytes = 16_384

pmaxDistinctAssetCount :: forall (s :: S). Term s PInteger
pmaxDistinctAssetCount = 16_384

pdataIsMap, pdataIsList, pdataIsInt, pdataIsBytes :: forall (s :: S). Term s (PData :--> PBool)
pdataIsMap = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant True # pconstant False # pconstant False # pconstant False
pdataIsList = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant False # pconstant True # pconstant False # pconstant False
pdataIsInt = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pdataIsBytes = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True

pkeyIs :: forall (s :: S). Term s (PData :--> PInteger :--> PBool)
pkeyIs = phoistAcyclic $ plam $ \dat expected ->
  pif (pdataIsInt # dat) (pasInt # dat #== expected) (pconstant False)

pcanonicalBytesKeyPrecedes :: forall (s :: S). Term s (PByteString :--> PByteString :--> PBool)
pcanonicalBytesKeyPrecedes = phoistAcyclic $ plam $ \left right ->
  plet (plengthBS # left) $ \leftLength ->
  plet (plengthBS # right) $ \rightLength ->
    pif (leftLength #< rightLength) (pconstant True) $
      pif (rightLength #< leftLength) (pconstant False) (left #< right)

pappendAssets :: forall (s :: S). Term s (PAssetList :--> PAssetList :--> PAssetList)
pappendAssets = phoistAcyclic $ pfix $ \self -> plam $ \left right ->
  pelimList (\item rest -> pcons # item # (self # rest # right)) right left

pparseAssetQuantities ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PBuiltinList (PBuiltinPair PData PData)
        :--> PMaybe PByteString
        :--> PInteger
        :--> PMaybe PParsedAssets
    )
pparseAssetQuantities = phoistAcyclic $ pfix $ \self -> plam $ \policyId entries previousName remaining ->
  pelimList
    ( \entry rest ->
        pif (remaining #<= 0) (pcon PNothing) $
          plet (pfst entry) $ \nameData ->
          plet (psnd entry) $ \quantityData ->
          pif (pdataIsBytes # nameData #&& pdataIsInt # quantityData)
            ( plet (pasByteStr # nameData) $ \name ->
              plet (pasInt # quantityData) $ \quantity ->
              plet
                ( pmatch previousName $ \case
                    PNothing -> pconstant True
                    PJust previous -> pcanonicalBytesKeyPrecedes # previous # name
                )
                $ \ordered ->
                  pif (plengthBS # name #<= 32 #&& quantity #> 0 #&& ordered)
                    ( pmatch (self # policyId # rest # pcon (PJust name) # (remaining - 1)) $ \case
                        PNothing -> pcon PNothing
                        PJust parsedRest -> pmatch parsedRest $ \(PPair restAssets restCount) ->
                          pcon $ PJust $ pcon $ PPair
                            (pcons # (ppairDataBuiltin # pdata (policyId <> name) # pdata quantity) # restAssets)
                            (restCount + 1)
                    )
                    (pcon PNothing)
            )
            (pcon PNothing)
    )
    (pcon $ PJust $ pcon $ PPair (pcon PNil) 0)
    entries

pparsePolicyAssets ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PBuiltinPair PData PData)
        :--> PMaybe PByteString
        :--> PInteger
        :--> PMaybe PParsedAssets
    )
pparsePolicyAssets = phoistAcyclic $ pfix $ \self -> plam $ \entries previousPolicy remaining ->
  pelimList
    ( \entry rest ->
        plet (pfst entry) $ \policyData ->
        plet (psnd entry) $ \quantitiesData ->
        pif (pdataIsBytes # policyData #&& pdataIsMap # quantitiesData)
          ( plet (pasByteStr # policyData) $ \policyId ->
            plet (pasMap # quantitiesData) $ \quantityEntries ->
            plet
              ( pmatch previousPolicy $ \case
                  PNothing -> pconstant True
                  PJust previous -> previous #< policyId
              )
              $ \ordered ->
                pif (plengthBS # policyId #== 28 #&& ordered #&& pnot # (pnull # quantityEntries))
                  ( pmatch (pparseAssetQuantities # policyId # quantityEntries # pcon PNothing # remaining) $ \case
                      PNothing -> pcon PNothing
                      PJust parsedPolicy -> pmatch parsedPolicy $ \(PPair policyAssets policyCount) ->
                        pmatch (self # rest # pcon (PJust policyId) # (remaining - policyCount)) $ \case
                          PNothing -> pcon PNothing
                          PJust parsedRest -> pmatch parsedRest $ \(PPair restAssets restCount) ->
                            pcon $ PJust $ pcon $ PPair
                              (pappendAssets # policyAssets # restAssets)
                              (policyCount + restCount)
                  )
                  (pcon PNothing)
          )
          (pcon PNothing)
    )
    (pcon $ PJust $ pcon $ PPair (pcon PNil) 0)
    entries

pparseValue :: forall (s :: S). Term s (PData :--> PMaybe PMidgardValue)
pparseValue = phoistAcyclic $ plam $ \dat ->
  pif (pdataIsList # dat)
    ( plet (pasList # dat) $ \items ->
      pif (plength # items #== 2)
        ( plet (pelemAt # 0 # items) $ \lovelaceData ->
          plet (pelemAt # 1 # items) $ \assetsData ->
          pif (pdataIsInt # lovelaceData #&& pdataIsMap # assetsData)
            ( plet (pasInt # lovelaceData) $ \lovelace ->
              pif (lovelace #>= 0)
                ( pmatch (pparsePolicyAssets # (pasMap # assetsData) # pcon PNothing # pmaxDistinctAssetCount) $ \case
                    PNothing -> pcon PNothing
                    PJust parsed -> pmatch parsed $ \(PPair assets _) ->
                      pcon $ PJust $ pcon $ PMidgardValue
                        (pdata lovelace)
                        (pdata $ pcon $ PAssocMap assets)
                )
                (pcon PNothing)
            )
            (pcon PNothing)
        )
        (pcon PNothing)
    )
    (pcon PNothing)

pdecodeCanonicalAddressBytes :: forall (s :: S). Term s (PByteString :--> PMaybe PMidgardAddress)
pdecodeCanonicalAddressBytes = phoistAcyclic $ plam $ \bytes ->
  plet (plengthBS # bytes) $ \length ->
  pif (length #== 29 #|| length #== 57)
    (plet (pbyteAt # bytes # 0) $ \header ->
      plet (pdiv # header # 16) $ \addressType ->
      plet (header - addressType * 16) $ \networkNibble ->
      plet (networkNibble #>= 8) $ \protected ->
      plet (pif protected (networkNibble - 8) networkNibble) $ \networkId ->
      plet (addressType #== 1 #|| addressType #== 3 #|| addressType #== 7) $ \paymentIsScript ->
      plet (psliceBS # 1 # 28 # bytes) $ \paymentHash ->
      plet
        (pif paymentIsScript
          (pcon $ PMidgardScriptCredential $ pdata paymentHash)
          (pcon $ PMidgardPubKeyCredential $ pdata paymentHash))
        $ \paymentCredential ->
        pif
          ( (networkId #== 0 #|| networkId #== 1)
              #&& pif (length #== 29)
                (addressType #== 6 #|| addressType #== 7)
                (addressType #>= 0 #&& addressType #<= 3)
          )
          ( plet
              ( pif (length #== 57)
                  ( plet (psliceBS # 29 # 28 # bytes) $ \stakeHash ->
                    plet (addressType #== 2 #|| addressType #== 3) $ \stakeIsScript ->
                      pcon $ PDJust $ pdata $ pif stakeIsScript
                        (pcon $ PMidgardScriptCredential $ pdata stakeHash)
                        (pcon $ PMidgardPubKeyCredential $ pdata stakeHash)
                  )
                  (pcon PDNothing)
              )
              $ \stakeCredential -> pcon $ PJust $ pcon $ PMidgardAddress
                (pdata protected)
                (pdata networkId)
                (pdata paymentCredential)
                (pdata stakeCredential)
          )
          (pcon PNothing))
    (pcon PNothing)

pparseAddress :: forall (s :: S). Term s (PData :--> PMaybe PMidgardAddress)
pparseAddress = phoistAcyclic $ plam $ \dat ->
  pif (pdataIsBytes # dat)
    (pdecodeCanonicalAddressBytes #$ pasByteStr # dat)
    (pcon PNothing)

pparseScriptRef :: forall (s :: S). Term s (PData :--> PMaybe PMidgardVersionedScript)
pparseScriptRef = phoistAcyclic $ plam $ \dat ->
  pif (pdataIsList # dat)
    ( plet (pasList # dat) $ \items ->
      pif (plength # items #== 2)
        ( plet (pelemAt # 0 # items) $ \tagData ->
          plet (pelemAt # 1 # items) $ \scriptData ->
          pif (pdataIsInt # tagData #&& pdataIsBytes # scriptData)
            ( plet (pasInt # tagData) $ \tag ->
              plet (pasByteStr # scriptData) $ \scriptBytes ->
                pif (tag #== 0)
                  (pcon $ PJust $ pmkScript PNativeCardanoScript scriptBytes)
                  (pif (tag #== 3)
                    (pcon $ PJust $ pmkScript PPlutusV3Script scriptBytes)
                    (pif (tag #== 128)
                      (pcon $ PJust $ pmkScript PMidgardV1Script scriptBytes)
                      (pcon PNothing)))
            )
            (pcon PNothing)
        )
        (pcon PNothing)
    )
    (pcon PNothing)
  where
    pmkScript language scriptBytes = pcon $ PMidgardVersionedScript
      (pdata $ pcon language)
      (pdata scriptBytes)

pmkOutput ::
  forall (s :: S).
  Term s PMidgardAddress ->
  Term s PMidgardValue ->
  Term s (PMaybeData PByteString) ->
  Term s (PMaybeData PMidgardVersionedScript) ->
  Term s PMidgardTxOutput
pmkOutput address value datumCbor scriptRef = pcon $ PMidgardTxOutput
  (pdata address) (pdata value) (pdata datumCbor) (pdata scriptRef)

pparseRequiredFields ::
  forall (s :: S).
  Term s PData ->
  Term s PData ->
  Term s (PMaybeData PByteString) ->
  Term s (PMaybeData PMidgardVersionedScript) ->
  Term s (PMaybe PMidgardTxOutput)
pparseRequiredFields addressData valueData datumCbor scriptRef =
  pmatch (pparseAddress # addressData) $ \case
    PNothing -> pcon PNothing
    PJust address -> pmatch (pparseValue # valueData) $ \case
      PNothing -> pcon PNothing
      PJust value -> pcon $ PJust $ pmkOutput address value datumCbor scriptRef

pparseOutputData :: forall (s :: S). Term s (PData :--> PMaybe PMidgardTxOutput)
pparseOutputData = phoistAcyclic $ plam $ \dat ->
  pif (pdataIsMap # dat)
    ( plet (pasMap # dat) $ \entries ->
      plet (plength # entries) $ \entryCount ->
      pif (entryCount #>= 2 #&& entryCount #<= 4)
        ( plet (pelemAt # 0 # entries) $ \entry0 ->
          plet (pelemAt # 1 # entries) $ \entry1 ->
          pif (pkeyIs # pfst entry0 # 0 #&& pkeyIs # pfst entry1 # 1)
            ( plet (psnd entry0) $ \addressData ->
              plet (psnd entry1) $ \valueData ->
              pif (entryCount #== 2)
                (pparseRequiredFields addressData valueData (pcon PDNothing) (pcon PDNothing))
                ( plet (pelemAt # 2 # entries) $ \entry2 ->
                  plet (pfst entry2) $ \key2 ->
                  plet (psnd entry2) $ \extraData ->
                  pif (entryCount #== 3)
                    ( pif (pkeyIs # key2 # 2)
                        ( pif (pdataIsBytes # extraData)
                            ( plet (pasByteStr # extraData) $ \datumCbor ->
                              pif (pisCanonicalPlutusDataV1 # datumCbor)
                                (pparseRequiredFields addressData valueData (pcon $ PDJust $ pdata datumCbor) (pcon PDNothing))
                                (pcon PNothing)
                            )
                            (pcon PNothing)
                        )
                        ( pif (pkeyIs # key2 # 3)
                            ( pmatch (pparseScriptRef # extraData) $ \case
                                PNothing -> pcon PNothing
                                PJust scriptRef -> pparseRequiredFields addressData valueData (pcon PDNothing) (pcon $ PDJust $ pdata scriptRef)
                            )
                            (pcon PNothing)
                        )
                    )
                    ( plet (pelemAt # 3 # entries) $ \entry3 ->
                      pif (pkeyIs # key2 # 2 #&& pkeyIs # pfst entry3 # 3 #&& pdataIsBytes # extraData)
                        ( plet (pasByteStr # extraData) $ \datumCbor ->
                          pmatch (pparseScriptRef # psnd entry3) $ \case
                            PNothing -> pcon PNothing
                            PJust scriptRef -> pif (pisCanonicalPlutusDataV1 # datumCbor)
                              (pparseRequiredFields addressData valueData (pcon $ PDJust $ pdata datumCbor) (pcon $ PDJust $ pdata scriptRef))
                              (pcon PNothing)
                        )
                        (pcon PNothing)
                    )
                )
            )
            (pcon PNothing)
        )
        (pcon PNothing)
    )
    (pcon PNothing)

pdecodeCanonicalOutput :: forall (s :: S). Term s (PByteString :--> PMaybe PMidgardTxOutput)
pdecodeCanonicalOutput = phoistAcyclic $ plam $ \outputCbor ->
  pif (plengthBS # outputCbor #> pmaxOutputCanonicalCborBytes)
    (pcon PNothing)
    ( pmatch (pdeserialise # outputCbor) $ \case
        PNothing -> pcon PNothing
        PJust dat -> pmatch (pparseOutputData # dat) $ \case
          PNothing -> pcon PNothing
          PJust output -> pif (pencodeMidgardTxOutput # output #== outputCbor)
            (pcon $ PJust output)
            (pcon PNothing)
    )
