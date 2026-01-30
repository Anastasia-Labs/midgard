{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Testing.TransactionProofs where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-- import Midgard.Utils (pand'List, pintToByteString)

import Data.Text qualified as T
import PlutusLedgerApi.V2 (Address (..), BuiltinByteString, Credential (..))
import PlutusTx.Builtins.HasOpaque
import PlutusTx.Prelude qualified as P
import Testing.Eval (passertEval, psucceeds)

-- import Midgard.Crypto

import Cardano.Crypto.DSIGN.Class (
  ContextDSIGN,
  DSIGNAlgorithm,
  Signable,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawDeserialiseSignKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (EcdsaSecp256k1DSIGN, MessageHash, toMessageHash)
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.SchnorrSecp256k1 (SchnorrSecp256k1DSIGN)
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Numeric (readHex)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Eval (toBuiltinHexString)
import PlutusCore.Crypto.Hash qualified as Hash
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import Prettyprinter
import Testing.ScriptContextBuilder (TxOutBuilder, buildScriptContext, currencySymbolFromHex, withAddress, withFee, withInlineDatum, withMint, withOutRef, withOutput, withReferenceInput, withReferenceScript, withRewardingScript, withScriptInput, withSigner, withTxOutAddress, withTxOutInlineDatum, withTxOutValue, withValidRange, withValue, withWithdrawal)
import Types.StateCommitment

-- [TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 0a04d88a1f166ed9586d8b09e4fd4e5b4c9e0843f49c26887836019903aab146, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",2324041)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 2 [I 1240,I 911],Constr 0 [Constr 0 [B "(\140\182\147\254c\159\n/1\155t\216Ga\NUL\226^\254\179y\252\NULB\251\253\156\221"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995390000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}}
-- ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 1769b7044d8a1866e5d6c0cebda3a97a9891db65999228b3cc73fd24ff08ccea, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",715417902)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 2 [I 1000000000,I 692810458],Constr 0 [Constr 0 [B "5\219h\ESC_\172\237-\133\142\140\171\201\151\162\211\217\SI\ESC\204\238\155\USJLwJ\177"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995389000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}}
-- ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 4ceb433f7c24d3327546924ebf159210c89984f831b22a28ae29705217582f3f, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",70153197)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 0 [I 100000000,I 66339870],Constr 0 [Constr 0 [B "_\131\145]\220\192>\158'\193Fm\167\US\157\181\RS\227ZE\201\167ILk.\251p"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995392000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}}
-- ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 7e738cff3cc809625f817420b7885e0382d5f9885e3f9f403b4185d6343305a0, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",53070680)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 0 [I 75000000,I 49754902],Constr 0 [Constr 0 [B "\ESCQ?\SOHR\GS\166\ACK\139\&8\200\187\234\248\237'\b\167Z[\215\253\142\247\EM\193.\222"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995391000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}}
-- ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = dea935b290efb09b30d109fddadef657c7730b51b13e4c83a70db35644b7158d, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",9685108)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 2 [I 10000000,I 7361978],Constr 0 [Constr 0 [B "_\131\145]\220\192>\158'\193Fm\167\US\157\181\RS\227ZE\201\167ILk.\251p"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995393000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}}
-- ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = e3d1bde2fb55bd2e21a065eb1f415d1d6b9d46619787e0565eb3f4804f64e553, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential c79327e6c5e5d4d282cd46c59c7108b91d11d02512582e2ba7fc88f7, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",2370500)]}),(5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae,Map {unMap = [("DjedMicroUSD",1000000000000000000),("DjedStableCoinNFT",1),("ShenMicroUSD",1000000000000000000)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [I 0,I 0,I 0,Constr 1 [],I 1823130,I 2370500,Constr 1 [],B "^m\SUBo6B\215(:Y\250\\\ETX\146\ETBh\239\187\132t\212\213\US%\162\178\224\174",Constr 0 [Constr 0 [B "P\b\214\&9>\254\255\173\132;\199F=\SYN\DC1k`\153\162fE\176$&\194M \219\217&o\ENQ"],I 0],Constr 0 [Constr 0 [B "P\b\214\&9>\254\255\173\132;\199F=\SYN\DC1k`\153\162fE\176$&\194M \219\217&o\ENQ"],I 0]]}), txOutReferenceScript = Nothing}}
-- ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = e3d1bde2fb55bd2e21a065eb1f415d1d6b9d46619787e0565eb3f4804f64e553, txOutRefIdx = 1}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = PubKeyCredential 1b46202fd51db10e8d29082daa24135e44fd944ce52e285844ef1e53, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",999999999800912543)]})]}}, txOutDatum = NoOutputDatum, txOutReferenceScript = Nothing}}
-- ],

-- txInfoInputs = [TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 0a04d88a1f166ed9586d8b09e4fd4e5b4c9e0843f49c26887836019903aab146, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",2324041)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 2 [I 1240,I 911],Constr 0 [Constr 0 [B "(\140\182\147\254c\159\n/1\155t\216Ga\NUL\226^\254\179y\252\NULB\251\253\156\221"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995390000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}},TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 1769b7044d8a1866e5d6c0cebda3a97a9891db65999228b3cc73fd24ff08ccea, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",715417902)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 2 [I 1000000000,I 692810458],Constr 0 [Constr 0 [B "5\219h\ESC_\172\237-\133\142\140\171\201\151\162\211\217\SI\ESC\204\238\155\USJLwJ\177"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995389000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}},TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 4ceb433f7c24d3327546924ebf159210c89984f831b22a28ae29705217582f3f, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",70153197)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 0 [I 100000000,I 66339870],Constr 0 [Constr 0 [B "_\131\145]\220\192>\158'\193Fm\167\US\157\181\RS\227ZE\201\167ILk.\251p"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995392000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}},TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 7e738cff3cc809625f817420b7885e0382d5f9885e3f9f403b4185d6343305a0, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",53070680)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 0 [I 75000000,I 49754902],Constr 0 [Constr 0 [B "\ESCQ?\SOHR\GS\166\ACK\139\&8\200\187\234\248\237'\b\167Z[\215\253\142\247\EM\193.\222"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995391000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}},TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = dea935b290efb09b30d109fddadef657c7730b51b13e4c83a70db35644b7158d, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",9685108)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [Constr 2 [I 10000000,I 7361978],Constr 0 [Constr 0 [B "_\131\145]\220\192>\158'\193Fm\167\US\157\181\RS\227ZE\201\167ILk.\251p"],Constr 1 []],Constr 0 [I 100,I 153],I 1640995393000,B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]}), txOutReferenceScript = Nothing}},TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = e3d1bde2fb55bd2e21a065eb1f415d1d6b9d46619787e0565eb3f4804f64e553, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential c79327e6c5e5d4d282cd46c59c7108b91d11d02512582e2ba7fc88f7, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",2370500)]}),(5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae,Map {unMap = [("DjedMicroUSD",1000000000000000000),("DjedStableCoinNFT",1),("ShenMicroUSD",1000000000000000000)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 [I 0,I 0,I 0,Constr 1 [],I 1823130,I 2370500,Constr 1 [],B "^m\SUBo6B\215(:Y\250\\\ETX\146\ETBh\239\187\132t\212\213\US%\162\178\224\174",Constr 0 [Constr 0 [B "P\b\214\&9>\254\255\173\132;\199F=\SYN\DC1k`\153\162fE\176$&\194M \219\217&o\ENQ"],I 0],Constr 0 [Constr 0 [B "P\b\214\&9>\254\255\173\132;\199F=\SYN\DC1k`\153\162fE\176$&\194M \219\217&o\ENQ"],I 0]]}), txOutReferenceScript = Nothing}},TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = e3d1bde2fb55bd2e21a065eb1f415d1d6b9d46619787e0565eb3f4804f64e553, txOutRefIdx = 1}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = PubKeyCredential 1b46202fd51db10e8d29082daa24135e44fd944ce52e285844ef1e53, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",999999999800912543)]})]}}, txOutDatum = NoOutputDatum, txOutReferenceScript = Nothing}}]
-- txInfoReferenceInputs =
--   [TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = 85a099cf014316a0ea5361f6380926fbaba831d9cb4fb915e7b8dcce06009610, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential 6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",70162490)]}),(fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71,Map {unMap = [("ProcessOrdersStakeRef",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 []}), txOutReferenceScript = Just f00e837533ffbfb71868d2ef7209a99585a46d1b665babf06fa3dec2}}
--   ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = e5b68b7f5dc68b417bad4effa6af75cd34b55a9c963445c6faa914730706df50, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential 6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",19567400)]}),(fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71,Map {unMap = [("RequestRef",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 []}), txOutReferenceScript = Just f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef}},
--   ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = e5b68b7f5dc68b417bad4effa6af75cd34b55a9c963445c6faa914730706df50, txOutRefIdx = 1}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential 6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",43358600)]}),(fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71,Map {unMap = [("StablecoinRef",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 []}), txOutReferenceScript = Just c79327e6c5e5d4d282cd46c59c7108b91d11d02512582e2ba7fc88f7}}
--   ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = eaa1707746762475e1ba1d1f146e9d9b025a2542e60d57ff044c875a106f1314, txOutRefIdx = 0}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential 6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",39578730)]}),(fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71,Map {unMap = [("OrderMintingRef",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 []}), txOutReferenceScript = Just 6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978}}
--   ,TxInInfo {txInInfoOutRef = TxOutRef {txOutRefId = eaa1707746762475e1ba1d1f146e9d9b025a2542e60d57ff044c875a106f1314, txOutRefIdx = 2}, txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = ScriptCredential 6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622, addressStakingCredential = Nothing}, txOutValue = Value {getValue = Map {unMap = [(,Map {unMap = [("",2840290)]}),(fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71,Map {unMap = [("RequestStakeRef",1)]})]}}, txOutDatum = OutputDatum (Datum {getDatum = toBuiltinData $ Constr 0 []}), txOutReferenceScript = Just 375b0147a033a95077621b603a126a37c32f621d86866c40a4db5fe9}}
--   ]

-- txInfoFee = Value {getValue = Map {unMap = [(,Map {unMap = [("",1105540)]})]}}
-- txInfoMint = Value {getValue = Map {unMap = [(,Map {unMap = [("",0)]}),(6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Map {unMap = [("DjedOrderTicket",-5)]})]}}
-- txInfoDCert = []
-- txInfoWdrl = Map {unMap = [(StakingHash (ScriptCredential 375b0147a033a95077621b603a126a37c32f621d86866c40a4db5fe9),0),(StakingHash (ScriptCredential f00e837533ffbfb71868d2ef7209a99585a46d1b665babf06fa3dec2),0)]}
-- txInfoValidRange = Interval {ivFrom = LowerBound (Finite (POSIXTime {getPOSIXTime = 1640995395000})) True, ivTo = UpperBound (Finite (POSIXTime {getPOSIXTime = 1640995575000})) False}
-- txInfoSignatories = [1b46202fd51db10e8d29082daa24135e44fd944ce52e285844ef1e53]
-- txInfoRedeemers = [(Spending (TxOutRef {txOutRefId = 0a04d88a1f166ed9586d8b09e4fd4e5b4c9e0843f49c26887836019903aab146, txOutRefIdx = 0}),Redeemer {getRedeemer = toBuiltinData $ Constr 0 []}),(Spending (TxOutRef {txOutRefId = 1769b7044d8a1866e5d6c0cebda3a97a9891db65999228b3cc73fd24ff08ccea, txOutRefIdx = 0}),Redeemer {getRedeemer = toBuiltinData $ Constr 0 []}),(Spending (TxOutRef {txOutRefId = 4ceb433f7c24d3327546924ebf159210c89984f831b22a28ae29705217582f3f, txOutRefIdx = 0}),Redeemer {getRedeemer = toBuiltinData $ Constr 0 []}),(Spending (TxOutRef {txOutRefId = 7e738cff3cc809625f817420b7885e0382d5f9885e3f9f403b4185d6343305a0, txOutRefIdx = 0}),Redeemer {getRedeemer = toBuiltinData $ Constr 0 []}),(Spending (TxOutRef {txOutRefId = dea935b290efb09b30d109fddadef657c7730b51b13e4c83a70db35644b7158d, txOutRefIdx = 0}),Redeemer {getRedeemer = toBuiltinData $ Constr 0 []}),(Spending (TxOutRef {txOutRefId = e3d1bde2fb55bd2e21a065eb1f415d1d6b9d46619787e0565eb3f4804f64e553, txOutRefIdx = 0}),Redeemer {getRedeemer = toBuiltinData $ Constr 1 []}),(Minting 6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978,Redeemer {getRedeemer = toBuiltinData $ Constr 1 []}),(Rewarding (StakingHash (ScriptCredential 375b0147a033a95077621b603a126a37c32f621d86866c40a4db5fe9)),Redeemer {getRedeemer = toBuiltinData $ I 5}),(Rewarding (StakingHash (ScriptCredential f00e837533ffbfb71868d2ef7209a99585a46d1b665babf06fa3dec2)),Redeemer {getRedeemer = toBuiltinData $ Constr 0 [B "\SOH\NUL\ETX\STX\EOT"]})]
-- txInfoData = Map {unMap = []},
-- txInfoId = ac6fdf5f0c3e6e3092a7a063d45eca133e96305ed8d27a5e6b8fe2f05db8c62a}

mockInfo :: TxInfo
mockInfo =
  scriptContextTxInfo . buildScriptContext $
    mconcat
      [ withScriptInput (dataToBuiltinData $ Constr 0 []) $
          mconcat
            [ withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "5341cfbf7fcd8bba13672a0b1f2d236e340165c30282ea0c2244873e164fecf6"
                  , txOutRefIdx = 0
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 2323130
                  , singleton (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae") (TokenName "DjedMicroUSD") 5000000
                  , singleton (currencySymbolFromHex "6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978") (TokenName "DjedOrderTicket") 1
                  ]
            , withInlineDatum $
                dataToBuiltinData $
                  Constr 0 [Constr 1 [I 5000000], Constr 0 [Constr 0 [B "\ESCQ?\SOHR\GS\166\ACK\139\&8\200\187\234\248\237'\b\167Z[\215\253\142\247\EM\193.\222"], Constr 1 []], Constr 0 [I 100, I 153], I 1640995578000, B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]
            ]
      , withScriptInput (dataToBuiltinData $ Constr 0 []) $
          mconcat
            [ withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "5f2cf50df72e14ac9d9839ebb4cf1ca4a2baea15d280b78f7eb6e30c0e943904"
                  , txOutRefIdx = 0
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 2323130
                  , singleton (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae") (TokenName "DjedMicroUSD") 20
                  , singleton (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae") (TokenName "ShenMicroUSD") 5
                  , singleton (currencySymbolFromHex "6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978") (TokenName "DjedOrderTicket") 1
                  ]
            , withInlineDatum $
                dataToBuiltinData $
                  Constr 0 [Constr 4 [I 20, I 5], Constr 0 [Constr 0 [B "_\131\145]\220\192>\158'\193Fm\167\US\157\181\RS\227ZE\201\167ILk.\251p"], Constr 1 []], Constr 0 [I 100, I 153], I 1640995579000, B "jod\152\RSW\252\249\205\204\\\201C\177(K\192\253\SYNo\211\203\206g9\151yx"]
            ]
      , withReferenceInput $
          mconcat
            [ withReferenceScript . ScriptHash $ toBuiltinHexString "f00e837533ffbfb71868d2ef7209a99585a46d1b665babf06fa3dec2"
            , withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "85a099cf014316a0ea5361f6380926fbaba831d9cb4fb915e7b8dcce06009610"
                  , txOutRefIdx = 0
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 70162490
                  , singleton (currencySymbolFromHex "fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71") (TokenName "ProcessOrdersStakeRef") 1
                  ]
            , withInlineDatum $ dataToBuiltinData $ Constr 0 []
            ]
      , withReferenceInput $
          mconcat
            [ withReferenceScript . ScriptHash $ toBuiltinHexString "f22fa1f3f22c8e72e82a4ffadeb2a63f33d37eb224d16cd61c3942ef"
            , withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "e5b68b7f5dc68b417bad4effa6af75cd34b55a9c963445c6faa914730706df50"
                  , txOutRefIdx = 0
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 19567400
                  , singleton (currencySymbolFromHex "fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71") (TokenName "RequestRef") 1
                  ]
            , withInlineDatum $ dataToBuiltinData $ Constr 0 []
            ]
      , withReferenceInput $
          mconcat
            [ withReferenceScript . ScriptHash $ toBuiltinHexString "c79327e6c5e5d4d282cd46c59c7108b91d11d02512582e2ba7fc88f7"
            , withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "e5b68b7f5dc68b417bad4effa6af75cd34b55a9c963445c6faa914730706df50"
                  , txOutRefIdx = 1
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 43358600
                  , singleton (currencySymbolFromHex "fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71") (TokenName "StablecoinRef") 1
                  ]
            , withInlineDatum $ dataToBuiltinData $ Constr 0 []
            ]
      , withReferenceInput $
          mconcat
            [ withReferenceScript . ScriptHash $ toBuiltinHexString "6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978"
            , withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "eaa1707746762475e1ba1d1f146e9d9b025a2542e60d57ff044c875a106f1314"
                  , txOutRefIdx = 0
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 39578730
                  , singleton (currencySymbolFromHex "fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71") (TokenName "OrderMintingRef") 1
                  ]
            , withInlineDatum $ dataToBuiltinData $ Constr 0 []
            ]
      , withReferenceInput $
          mconcat
            [ withReferenceScript . ScriptHash $ toBuiltinHexString "375b0147a033a95077621b603a126a37c32f621d86866c40a4db5fe9"
            , withOutRef
                TxOutRef
                  { txOutRefId = TxId $ toBuiltinHexString "eaa1707746762475e1ba1d1f146e9d9b025a2542e60d57ff044c875a106f1314"
                  , txOutRefIdx = 1
                  }
            , withAddress
                Address
                  { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "6b40222d9adccb5574e2b522274c0146264d7114b8a5fdcbd0cfd622"
                  , addressStakingCredential = Nothing
                  }
            , withValue $
                mconcat
                  [ lovelaceValue 2840290
                  , singleton (currencySymbolFromHex "fbd4071892e0403910693a386dab46e03581663e8a4927f0d7d88c71") (TokenName "RequestStakeRef") 1
                  ]
            , withInlineDatum $ dataToBuiltinData $ Constr 0 []
            ]
      , foldMap withOutput mockOutputs
      , withFee 1067218
      , withMint
          (singleton (currencySymbolFromHex "6a6f64981e57fcf9cdcc5cc943b1284bc0fd166fd3cbce6739977978") (TokenName "DjedOrderTicket") (-4))
          . dataToBuiltinData
          $ Constr 1 []
      , withRewardingScript
          (dataToBuiltinData $ I 2)
          (ScriptCredential . ScriptHash $ toBuiltinHexString "375b0147a033a95077621b603a126a37c32f621d86866c40a4db5fe9")
          0
      , withRewardingScript
          (dataToBuiltinData $ Constr 0 [B "\EOT\ENQ\NUL\SOH"])
          (ScriptCredential . ScriptHash $ toBuiltinHexString "f00e837533ffbfb71868d2ef7209a99585a46d1b665babf06fa3dec2")
          0
      , withValidRange
          Interval
            { ivFrom = LowerBound (Finite (POSIXTime {getPOSIXTime = 1640995581000})) True
            , ivTo = UpperBound (Finite (POSIXTime {getPOSIXTime = 1640995761000})) False
            }
      , withSigner . PubKeyHash $ toBuiltinHexString "1b46202fd51db10e8d29082daa24135e44fd944ce52e285844ef1e53"
      ]

-- These existed in the original mockTxInfo but seem unnecessary?
{-
txInfoRedeemers =
  Map.unsafeFromList
    [ (Spending (TxOutRef {txOutRefId = TxId $ toBuiltinHexString "ac6fdf5f0c3e6e3092a7a063d45eca133e96305ed8d27a5e6b8fe2f05db8c62a", txOutRefIdx = 0}), Redeemer {getRedeemer = dataToBuiltinData $ Constr 1 []})
    , (Spending (TxOutRef {txOutRefId = TxId $ toBuiltinHexString "c63f3dccdc455977f9cd557aa7fbb825f9babd29ac4da3251dbde765afd29d54", txOutRefIdx = 0}), Redeemer {getRedeemer = dataToBuiltinData $ Constr 0 []})
    , (Spending (TxOutRef {txOutRefId = TxId $ toBuiltinHexString "e053c715fccb903f438f57bd40c7150e88514c58f6796e4733851d7ca6181d56", txOutRefIdx = 0}), Redeemer {getRedeemer = dataToBuiltinData $ Constr 0 []})
    ]
-}

mockOutputs :: [TxOutBuilder]
mockOutputs =
  [ mconcat
      [ withTxOutAddress
          Address
            { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "c79327e6c5e5d4d282cd46c59c7108b91d11d02512582e2ba7fc88f7"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $
          mconcat
            [ lovelaceValue 818638619
            , singleton (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae") (TokenName "DjedMicroUSD") 999999999825000000
            , singleton (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae") (TokenName "DjedStableCoinNFT") 1
            , singleton (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae") (TokenName "ShenMicroUSD") 999999998989998760
            ]
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ I 816268119
            , I 175000000
            , I 1010001240
            , Constr
                0
                [ Constr
                    0
                    [ Constr
                        0
                        [ Constr 0 [B "\222\169\&5\178\144\239\176\155\&0\209\t\253\218\222\246W\199s\vQ\177>L\131\167\r\179VD\183\NAK\141", I 0]
                        , I 1640995393000
                        ]
                    ]
                , I 1823130
                , I 2370500
                , Constr 1 []
                , B "^m\SUBo6B\215(:Y\250\\\ETX\146\ETBh\239\187\132t\212\213\US%\162\178\224\174"
                , Constr
                    0
                    [ Constr 0 [B "P\b\214\&9>\254\255\173\132;\199F=\SYN\DC1k`\153\162fE\176$&\194M \219\217&o\ENQ", I 0]
                    , Constr 0 [Constr 0 [B "P\b\214\&9>\254\255\173\132;\199F=\SYN\DC1k`\153\162fE\176$&\194M \219\217&o\ENQ", I 0]]
                    ]
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = PubKeyCredential $ PubKeyHash $ toBuiltinHexString "5f83915ddcc03e9e27c1466da71f9db51ee35a45c9a7494c6b2efb70"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $
          mconcat
            [ lovelaceValue 1823130
            , singleton
                (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae")
                (TokenName "ShenMicroUSD")
                10000000
            ]
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ Constr 5 [I 7361978, I 10000000]
            , Constr
                0
                [ Constr
                    0
                    [ B "\222\169\&5\178\144\239\176\155\&0\209\t\253\218\222\246W\199s\vQ\177>L\131\167\r\179VD\183\NAK\141"
                    , I 0
                    ]
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = PubKeyCredential $ PubKeyHash $ toBuiltinHexString "5f83915ddcc03e9e27c1466da71f9db51ee35a45c9a7494c6b2efb70"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $
          mconcat
            [ lovelaceValue 1823130
            , singleton
                (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae")
                (TokenName "DjedMicroUSD")
                100000000
            ]
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ Constr 4 [I 66339870, I 100000000]
            , Constr
                0
                [ Constr
                    0
                    [ B "L\235C?|$\211\&2uF\146N\191\NAK\146\DLE\200\153\132\248\&1\178*(\174)pR\ETBX/?"
                    , I 0
                    ]
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = PubKeyCredential $ PubKeyHash $ toBuiltinHexString "1b513f01521da6068b38c8bbeaf8ed2708a75a5bd7fd8ef719c12ede"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $
          mconcat
            [ lovelaceValue 1823130
            , singleton
                (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae")
                (TokenName "DjedMicroUSD")
                75000000
            ]
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ Constr 4 [I 49754902, I 75000000]
            , Constr
                0
                [ Constr
                    0
                    [ B "~s\140\255<\200\tb_\129t \183\136^\ETX\130\213\249\136^?\159@;A\133\214\&43\ENQ\160"
                    , I 0
                    ]
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = PubKeyCredential $ PubKeyHash $ toBuiltinHexString "288cb693fe639f0a2f319b74d8476100e25efeb379fc0042fbfd9cdd"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $
          mconcat
            [ lovelaceValue 1823130
            , singleton
                (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae")
                (TokenName "ShenMicroUSD")
                1240
            ]
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ Constr 5 [I 911, I 1240]
            , Constr
                0
                [ Constr
                    0
                    [ B "\n\EOT\216\138\US\SYNn\217Xm\139\t\228\253N[L\158\bC\244\156&\136x6\SOH\153\ETX\170\177F"
                    , I 0
                    ]
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = PubKeyCredential $ PubKeyHash $ toBuiltinHexString "35db681b5faced2d858e8cabc997a2d3d90f1bccee9b1f4a4c774ab1"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $
          mconcat
            [ lovelaceValue 1823130
            , singleton
                (currencySymbolFromHex "5e6d1a6f3642d7283a59fa5c03921768efbb8474d4d51f25a2b2e0ae")
                (TokenName "ShenMicroUSD")
                1000000000
            ]
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ Constr 5 [I 692810458, I 1000000000]
            , Constr
                0
                [ Constr
                    0
                    [ B "\ETBi\183\EOTM\138\CANf\229\214\192\206\189\163\169z\152\145\219e\153\146(\179\204s\253$\255\b\204\234"
                    , I 0
                    ]
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = ScriptCredential $ ScriptHash $ toBuiltinHexString "8698142a5109a8c0781fd367ab190c528401b69df6cb00b5459eb45c"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $ lovelaceValue 22740445
      , withTxOutInlineDatum . dataToBuiltinData $
          Constr
            0
            [ Constr
                0
                [ Constr
                    0
                    [ Constr
                        0
                        [ B "\222\169\&5\178\144\239\176\155\&0\209\t\253\218\222\246W\199s\vQ\177>L\131\167\r\179VD\183\NAK\141"
                        , I 0
                        ]
                    , I 1640995393000
                    ]
                , I 1640995635000
                ]
            ]
      ]
  , mconcat
      [ withTxOutAddress
          Address
            { addressCredential = PubKeyCredential $ PubKeyHash $ toBuiltinHexString "1b46202fd51db10e8d29082daa24135e44fd944ce52e285844ef1e53"
            , addressStakingCredential = Nothing
            }
      , withTxOutValue $ lovelaceValue 999999999802333717
      ]
  ]

hashMidgardTx :: MidgardTxInfo -> ByteString
hashMidgardTx midgardTx = plift @PByteString $ pblake2b_256 #$ pserialiseData # pforgetData (pdata $ pconstant @PMidgardTxInfo midgardTx)

txInfoToMidgardTxInfo :: TxInfo -> MidgardTxInfo
txInfoToMidgardTxInfo txInfo =
  MidgardTxInfo
    { mtxInfoInputs = txInfoInputs txInfo
    , mtxInfoReferenceInputs = txInfoReferenceInputs txInfo
    , mtxInfoOutputs = txInfoOutputs txInfo
    , mtxInfoFee = txInfoFee txInfo
    , mtxInfoMint = txInfoMint txInfo
    , mtxInfoObservers = getScriptHashesFromWithdrawals (Map.toList (txInfoWdrl txInfo))
    , mtxInfoValidRange = txInfoValidRange txInfo
    , mtxInfoSignatories = txInfoSignatories txInfo
    , mtxInfoScriptIntegrityHash = BuiltinData $ Constr 0 [] -- Unused
    , mtxInfoAuxDataHash = BuiltinData $ Constr 0 [] -- Unused
    , mtxInfoId = txInfoId txInfo
    }
  where
    getScriptHashesFromWithdrawals :: [(Credential, Lovelace)] -> [ScriptHash]
    getScriptHashesFromWithdrawals wdrls =
      mapMaybe
        ( \(cred, _) ->
            case cred of
              ScriptCredential sh -> Just sh
              _ -> Nothing
        )
        wdrls

dumpDebug :: IO ()
dumpDebug = do
  let midgardTxInfo = txInfoToMidgardTxInfo mockInfo
  putStrLn "Cardano TxInfo\n"
  print (pretty mockInfo)
  putStrLn "\nMidgard TxInfo\n"
  print midgardTxInfo
  putStrLn "\nHashed TxInfo\n"
  print midgardTxInfo
