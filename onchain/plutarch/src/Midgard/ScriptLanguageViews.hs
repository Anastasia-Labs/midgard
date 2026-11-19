{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.ScriptLanguageViews
Description : Plutarch port of @lib/midgard/script-language-views-v1.ak@.

The script integrity hash, over the one cost-model view Midgard V1 admits.

Cardano's script integrity hash covers the redeemers, the datums and the
/language views/ — a map from language to that language's cost model, encoded in
the ledger's own idiosyncratic way. Midgard fixes a single canonical cost-model
view shared by PlutusV3 and MidgardV1, so the only thing that varies is which
languages are present, and that is a two-bit bitmap.

=== Four cases, and the empty one is not a hash of nothing

A bitmap of @0@ — no scripts at all — gives @blake2b_256(0xf6)@, the hash of CBOR
@null@, not the hash of the empty string. That is the ledger's rule and it is why
the constant is spelled out rather than derived: getting it wrong would make every
script-free transaction's integrity hash disagree with L1's.

The other three cases are @a1 02@ (PlutusV3 alone), @a1 18 80@ (MidgardV1 alone)
and @a2 02 … 18 80 …@ (both), each followed by the canonical view. The two-entry
map spells its keys in ascending order because that is what canonical CBOR
requires of a definite map, and the hash is over exactly those bytes.
-}
module Midgard.ScriptLanguageViews (
  pcanonicalCostModelView,
  pemptyScriptIntegrityHash,
  pexpectedScriptIntegrityHash,
) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)

{- | Aiken @script_language_views_v1.canonical_cost_model_view@.

The cost-model view both languages share, byte for byte as the ledger encodes
it. Transcribed rather than computed: it is a fixed protocol constant, and a
derivation would be a second thing to get wrong.
-}
pcanonicalCostModelView :: forall (s :: S). Term s PByteString
pcanonicalCostModelView =
  phexByteStr
    "9901291a000189b41901a401011903e818ad00011903e819ea350401192baf18201a000312591920a404193e801864193e801864193e801864193e801864193e801864193e80186418641864193e8018641a000170a718201a00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a0001e143191c893903831906b419022518391a00014f580001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a0001e143191c893903831906b419022518391a00014f5800011a0001614219020700011a000122c118201a00014f581a0001e143191c893903831906b419022518391a00014f580001011a00014f581a0001e143191c893903831906b419022518391a00014f5800011a000e94721a0003414000021a0004213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c1820195108182019654d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a193e801864193e8018641a000eaf1f121a002a6e06061a0006be98011a0321aac7190eac121a00041699121a048e466e1922a4121a0327ec9a121a001e743c18241a0031410f0c1a000dbf9e011a09f2f6d31910d318241a0004578218241a096e44021967b518241a0473cee818241a13e62472011a0f23d40118481a00212c5618481a0022814619fc3b041a00032b00192076041a0013be0419702c183f00011a000f59d919aa6718fb00011a000187551902d61902cf00011a000187551902d61902cf00011a000187551902d61902cf00011a0001a5661902a800011a00017468011a00044a391949a000011a0002bfe2189f01011a00026b371922ee00011a00026e9219226d00011a0001a3e2190ce2011a00019e4919028f011a001df8bb195fc803"

{- | Aiken @script_language_views_v1.empty_script_integrity_hash@.

@blake2b_256(0xf6)@ — the hash of CBOR @null@, which is what the ledger hashes
when a transaction carries no scripts. Not the hash of the empty string, and not
the hash of an empty map.
-}
pemptyScriptIntegrityHash :: forall (s :: S). Term s PByteString
pemptyScriptIntegrityHash = pblake2b_256 # phexByteStr "f6"

{- | Aiken @script_language_views_v1.expected_script_integrity_hash@.

The bitmap is @0..3@: bit 0 is PlutusV3, bit 1 is MidgardV1. An out-of-range
bitmap or a redeemer witness hash that is not 32 bytes aborts, as in the
original — these come from a caller that has already authenticated them.
-}
pexpectedScriptIntegrityHash ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString)
pexpectedScriptIntegrityHash = phoistAcyclic $
  plam $ \redeemerWitnessHash languageBitmap ->
    pif
      (plengthBS # redeemerWitnessHash #== 32 #&& 0 #<= languageBitmap #&& languageBitmap #<= 3)
      `flip` perror
      $ pif (languageBitmap #== 0) pemptyScriptIntegrityHash
      $ plet
        ( pif
            (languageBitmap #== 1)
            (phexByteStr "a102" <> pcanonicalCostModelView)
            ( pif
                (languageBitmap #== 2)
                (phexByteStr "a11880" <> pcanonicalCostModelView)
                ( phexByteStr "a202"
                    <> pcanonicalCostModelView
                    <> phexByteStr "1880"
                    <> pcanonicalCostModelView
                )
            )
        )
      $ \languageViews ->
        pblake2b_256
          #$ phexByteStr "82"
          <> (pencodeDefiniteBytes # redeemerWitnessHash)
          <> languageViews
