{- |
Module      : Midgard.ScriptProof
Description : Plutarch port of @lib/midgard/script-proof-v1.ak@.

Canonical script-proof commitments for sources, redeemers, signers, outputs,
executions, purposes, and context items. The source-key predicate also pins the
exact CBOR spelling of reference output identifiers.

=== The language tag is not the constructor index

@MidgardScriptLanguage@'s constructors are 0, 1, 2 as declared, but the tag byte
this hash prepends is 0, 3 or 128 — see
'Midgard.FraudProofs.NativeTx.Components.pmidgardScriptLanguageToTag'. Hashing
the constructor index instead would agree with Aiken on native scripts, which are
0 either way, and disagree on everything else. That is exactly the kind of
coincidence that makes a port look right until the second case.
-}
module Midgard.ScriptProof (
  planguageTag,
  pversionedScriptHash,
  pcanonicalReferenceSourceKey,
  pinlineSourceLeafHash, psourceLeafHash, preferenceSourceLeafHash, psourceDescriptorLeafHash,
  predeemerPurposeTag, predeemerItemLeafHash, predeemerLeafHash,
  psignerLeafHash, poutputItemLeafHash, poutputLeafHash, poutputDescriptorLeafHash,
  pexecutionLeafHash, ppurposeLeafHash, pcontextItemLeafHash, presolvedContextItemLeafHash,
) where

import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256)
import Plutarch.Core.Internal.Builtins (pconsBS')
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Components (
  pencodeMidgardRedeemerWitness, pencodeMidgardVersionedScript,
  pmidgardScriptLanguageToTag,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardRedeemerPurpose (..), PMidgardRedeemerWitness, PMidgardScriptLanguage,
  PMidgardVersionedScript (..),
 )

planguageTag :: forall s. Term s (PMidgardScriptLanguage :--> PInteger)
planguageTag = pmidgardScriptLanguageToTag

{- | Aiken @script_proof_v1.versioned_script_hash@.

@blake2b_224 (language_tag ‖ script_bytes)@. Aiken writes it as
@#"" |> bytearray.push(tag) |> bytearray.concat(bytes)@, which is a one-byte cons
onto the script bytes.
-}
pversionedScriptHash ::
  forall (s :: S). Term s (PMidgardVersionedScript :--> PByteString)
pversionedScriptHash = phoistAcyclic $
  plam $ \script -> pmatch script $
    \PMidgardVersionedScript {pversionedScript'language, pversionedScript'scriptBytes} ->
      pblake2b_224
        #$ pconsBS'
        # (pmidgardScriptLanguageToTag # pfromData pversionedScript'language)
        # pfromData pversionedScript'scriptBytes

psourceLeafDomain, pinlineSourceLeafDomain, predeemerLeafDomain, ppurposeLeafDomain,
  psignerLeafDomain, poutputItemLeafDomain, poutputDescriptorLeafDomain, pexecutionLeafDomain,
  pcontextItemLeafDomain, presolvedContextItemLeafDomain :: forall s. Term s PByteString
psourceLeafDomain = pconstant "MidgardScriptSourceLeafV1"
pinlineSourceLeafDomain = pconstant "MidgardInlineScriptSourceLeafV1"
predeemerLeafDomain = pconstant "MidgardRedeemerLeafV1"
ppurposeLeafDomain = pconstant "MidgardScriptPurposeLeafV1"
psignerLeafDomain = pconstant "MidgardSignerLeafV1"
poutputItemLeafDomain = pconstant "MidgardOutputItemLeafV1"
poutputDescriptorLeafDomain = pconstant "MidgardOutputDescriptorLeafV1"
pexecutionLeafDomain = pconstant "MidgardScriptExecutionLeafV1"
pcontextItemLeafDomain = pconstant "MidgardScriptContextItemLeafV1"
presolvedContextItemLeafDomain = pconstant "MidgardResolvedContextItemLeafV1"

pdataIsList, pdataIsInteger, pdataIsBytes :: forall s. Term s (PData :--> PBool)
pdataIsList = phoistAcyclic $ plam $ \d -> pchooseData # d # pconstant False # pconstant False # pconstant True # pconstant False # pconstant False
pdataIsInteger = phoistAcyclic $ plam $ \d -> pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pdataIsBytes = phoistAcyclic $ plam $ \d -> pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True

pcanonicalReferenceSourceKey :: forall s. Term s (PByteString :--> PBool)
pcanonicalReferenceSourceKey = phoistAcyclic $ plam $ \sourceKey ->
  pmatch (pdeserialise # sourceKey) $ \case
    PNothing -> pconstant False
    PJust dat -> pif (pdataIsList # dat)
      (plet (pasList # dat) $ \fields -> pif (plength # fields #== 2)
        (plet (pelemAt # 0 # fields) $ \txIdData -> plet (pelemAt # 1 # fields) $ \indexData ->
          pif (pdataIsBytes # txIdData #&& pdataIsInteger # indexData)
            (plet (pasByteStr # txIdData) $ \txId -> plet (pasInt # indexData) $ \outputIndex ->
              pand'List
                [ plengthBS # txId #== 32
                , outputIndex #>= 0, outputIndex #<= 65_535
                , pconstant "\x82\x58\x20" <> txId <> pcborInt outputIndex #== sourceKey
                ])
            (pconstant False))
        (pconstant False))
      (pconstant False)

planguageTagIsValid :: forall s. Term s PInteger -> Term s PBool
planguageTagIsValid tag = tag #== 0 #|| tag #== 3 #|| tag #== 128

pinlineSourceLeafHash :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString)
pinlineSourceLeafHash = phoistAcyclic $ plam $ \sourceIndex languageTag scriptHash totalLength commitment ->
  pif (sourceIndex #>= 0 #&& planguageTagIsValid languageTag #&& plengthBS # scriptHash #== 28
      #&& totalLength #> 0 #&& plengthBS # commitment #== 32)
    (pblake2b_256 #$ pinlineSourceLeafDomain <> pcborInt sourceIndex <> pcborInt languageTag
      <> pencodeDefiniteBytes # scriptHash <> pcborInt totalLength <> pencodeDefiniteBytes # commitment)
    perror

preferenceSourceLeafHash :: forall s. Term s (PByteString :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString)
preferenceSourceLeafHash = phoistAcyclic $ plam $ \sourceKey languageTag scriptHash totalLength commitment ->
  pif (pcanonicalReferenceSourceKey # sourceKey #&& planguageTagIsValid languageTag
      #&& plengthBS # scriptHash #== 28 #&& totalLength #> 0 #&& plengthBS # commitment #== 32)
    (pblake2b_256 #$ psourceLeafDomain <> pcborInt 1 <> pencodeDefiniteBytes # sourceKey
      <> pcborInt languageTag <> pencodeDefiniteBytes # scriptHash <> pcborInt totalLength
      <> pencodeDefiniteBytes # commitment)
    perror

psourceDescriptorLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString)
psourceDescriptorLeafHash = phoistAcyclic $ plam $ \origin sourceKey languageTag scriptHash totalLength commitment ->
  pif (origin #== 0)
    (pmatch (pdeserialise # sourceKey) $ \case
      PNothing -> perror
      PJust indexData -> plet (pasInt # indexData) $ \index ->
        pif (index #>= 0 #&& pcborInt index #== sourceKey)
          (pinlineSourceLeafHash # index # languageTag # scriptHash # totalLength # commitment) perror)
    (pif (origin #== 1)
      (preferenceSourceLeafHash # sourceKey # languageTag # scriptHash # totalLength # commitment) perror)

psourceLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PMidgardVersionedScript :--> PByteString)
psourceLeafHash = phoistAcyclic $ plam $ \origin sourceKey script ->
  plet (pencodeMidgardVersionedScript # script) $ \scriptCbor -> pmatch script $ \s ->
  pif (origin #== 0)
    (pmatch (pdeserialise # sourceKey) $ \case
      PNothing -> perror
      PJust indexData -> plet (pasInt # indexData) $ \index ->
        pif (index #>= 0 #&& pcborInt index #== sourceKey)
          (pinlineSourceLeafHash # index # (planguageTag # pfromData (pversionedScript'language s))
            # (pversionedScriptHash # script) # (plengthBS # scriptCbor)
            # (Bounded.pfromBytes # 6 # index # scriptCbor)) perror)
    (pif (origin #== 1)
      (plet (pasList # pexpectJustData sourceKey) $ \fields -> plet (pasInt # (pelemAt # 1 # fields)) $ \outputIndex ->
        preferenceSourceLeafHash # sourceKey # (planguageTag # pfromData (pversionedScript'language s))
          # (pversionedScriptHash # script) # (plengthBS # scriptCbor)
          # (Bounded.pfromBytes # 2 # outputIndex # scriptCbor))
      perror)
  where
    pexpectJustData sourceKey = pmatch (pdeserialise # sourceKey) $ \case PNothing -> perror; PJust dat -> dat

predeemerPurposeTag :: forall s. Term s (PMidgardRedeemerPurpose :--> PInteger)
predeemerPurposeTag = phoistAcyclic $ plam $ \purpose -> pmatch purpose $ \case
  PSpendRedeemer -> 0
  PMintRedeemer -> 1
  PCertRedeemer -> 2
  PRewardRedeemer -> 3
  PVoteRedeemer -> 4
  PProposeRedeemer -> 5
  PReceiveRedeemer -> 6

predeemerItemLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PByteString)
predeemerItemLeafHash = phoistAcyclic $ plam $ \index commitment ->
  pif (index #>= 0 #&& plengthBS # commitment #== 32)
    (pblake2b_256 #$ predeemerLeafDomain <> pcborInt index <> pencodeDefiniteBytes # commitment) perror

predeemerLeafHash :: forall s. Term s (PInteger :--> PMidgardRedeemerWitness :--> PByteString)
predeemerLeafHash = phoistAcyclic $ plam $ \index redeemer ->
  predeemerItemLeafHash # index # (Bounded.pfromBytes # 8 # index # (pencodeMidgardRedeemerWitness # redeemer))

psignerLeafHash :: forall s. Term s (PByteString :--> PByteString)
psignerLeafHash = phoistAcyclic $ plam $ \signer -> pif (plengthBS # signer #== 28)
  (pblake2b_256 #$ psignerLeafDomain <> pencodeDefiniteBytes # signer) perror

poutputItemLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PByteString)
poutputItemLeafHash = phoistAcyclic $ plam $ \index commitment ->
  pif (index #>= 0 #&& plengthBS # commitment #== 32)
    (pblake2b_256 #$ poutputItemLeafDomain <> pcborInt index <> pencodeDefiniteBytes # commitment) perror

poutputLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PByteString)
poutputLeafHash = phoistAcyclic $ plam $ \index output ->
  poutputItemLeafHash # index # (Bounded.pfromBytes # 2 # index # output)

poutputDescriptorLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PByteString)
poutputDescriptorLeafHash = phoistAcyclic $ plam $ \index descriptor -> pif (index #>= 0)
  (pblake2b_256 #$ poutputDescriptorLeafDomain <> pcborInt index <> pencodeDefiniteBytes # descriptor) perror

pexecutionLeafHash :: forall s. Term s (PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pexecutionLeafHash = phoistAcyclic $ plam $ \languageTag purpose source redeemer ->
  pif (planguageTagIsValid languageTag #&& plengthBS # purpose #== 32 #&& plengthBS # source #== 32
      #&& (redeemer #== pconstant "" #|| plengthBS # redeemer #== 32))
    (pblake2b_256 #$ pexecutionLeafDomain <> pcborInt languageTag <> pencodeDefiniteBytes # purpose
      <> pencodeDefiniteBytes # source <> pencodeDefiniteBytes # redeemer) perror

ppurposeLeafHash :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
ppurposeLeafHash = phoistAcyclic $ plam $ \kind index scriptHash subject ->
  pif (kind #>= 0 #&& kind #<= 3 #&& index #>= 0 #&& plengthBS # scriptHash #== 28)
    (pblake2b_256 #$ ppurposeLeafDomain <> pcborInt kind <> pcborInt index
      <> pencodeDefiniteBytes # scriptHash <> pencodeDefiniteBytes # subject) perror

pcontextItemLeafHash :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PInteger :--> PInteger :--> PByteString)
pcontextItemLeafHash = phoistAcyclic $ plam $ \kind index root cborLength memory ->
  pif (kind #>= 0 #&& kind #<= 7 #&& index #>= 0 #&& plengthBS # root #== 32
      #&& cborLength #>= 0 #&& memory #>= 0)
    (pblake2b_256 #$ pcontextItemLeafDomain <> pcborInt kind <> pcborInt index
      <> pencodeDefiniteBytes # root <> pcborInt cborLength <> pcborInt memory) perror

presolvedContextItemLeafHash :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
presolvedContextItemLeafHash = phoistAcyclic $ plam $ \kind index key output ->
  pif ((kind #== 0 #|| kind #== 1) #&& index #>= 0)
    (pblake2b_256 #$ presolvedContextItemLeafDomain <> pcborInt kind <> pcborInt index
      <> pencodeDefiniteBytes # key <> pencodeDefiniteBytes # output) perror
