{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutput (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxOutput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardCredential (..),
  PMidgardScriptLanguage (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PMidgardVersionedScript (..),
 )
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.LedgerOutput"
  [ testCase "canonical_ledger_output_v1_round_trips" $ passertEvalNoTrace $
      pdecodeCanonicalOutput # (pencodeMidgardTxOutput # simpleOutput) #== pcon (PJust simpleOutput)
  , testCase "canonical_ledger_output_v1_supports_datum_and_reference_script" $ passertEvalNoTrace $
      pdecodeCanonicalOutput # (pencodeMidgardTxOutput # richOutput) #== pcon (PJust richOutput)
  , testCase "ledger_output_v1_rejects_malformed_cbor_without_aborting" $ passertEvalNoTrace $
      pdecodeCanonicalOutput # pconstant "" #== pcon PNothing
        #&& pdecodeCanonicalOutput # bytes "a100" #== pcon PNothing
        #&& pdecodeCanonicalOutput # bytes "bf00ff" #== pcon PNothing
  , testCase "ledger_output_v1_rejects_unsorted_asset_names" $ passertEvalNoTrace $
      pdecodeCanonicalOutput # bytes
        "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a2410201410102"
        #== pcon PNothing
  , testCase "ledger_output_v1_rejects_noncanonical_embedded_datum" $ passertEvalNoTrace $
      pdecodeCanonicalOutput # bytes
        "a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a002421817"
        #== pcon PNothing
  , testCase "ledger_output_v1_rejects_unknown_reference_script_language" $ passertEvalNoTrace $
      pdecodeCanonicalOutput # bytes
        "a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a00382184140"
        #== pcon PNothing
  ]

simpleOutput :: forall (s :: S). Term s PMidgardTxOutput
simpleOutput = pcon $ PMidgardTxOutput
  (pdata $ pcon $ PMidgardAddress
    (pdata $ pconstant False)
    (pdata 0)
    (pdata $ pcon $ PMidgardPubKeyCredential $ pdata $ pconstant $ BS.replicate 28 0xaa)
    (pdata $ pcon PDNothing))
  (pdata $ pcon $ PMidgardValue (pdata 2_000_000) (pdata $ pcon $ PAssocMap pnil))
  (pdata $ pcon PDNothing)
  (pdata $ pcon PDNothing)

richOutput :: forall (s :: S). Term s PMidgardTxOutput
richOutput = pmatch simpleOutput $ \output -> pcon $ PMidgardTxOutput
  (ptxOutput'address output)
  (ptxOutput'value output)
  (pdata $ pcon $ PDJust $ pdata $ bytes "d87980")
  (pdata $ pcon $ PDJust $ pdata $ pcon $ PMidgardVersionedScript
    (pdata $ pcon PMidgardV1Script)
    (pdata $ bytes "010203"))

bytes :: forall (s :: S). BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
