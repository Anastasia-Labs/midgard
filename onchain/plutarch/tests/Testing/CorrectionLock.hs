{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CorrectionLock
Description : Aiken-parity tests for @lib/midgard/correction-lock.ak@.
-}
module Testing.CorrectionLock (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import PlutusCore.Data qualified as PD
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PTxInInfo (..), PTxOut, PTxOutRef (..))
import Plutarch.Prelude
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  ScriptHash (..),
  TxId (..),
  TxInInfo,
  TxOut,
  TxOutRef (..),
 )
import PlutusTx.Builtins (BuiltinByteString, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CorrectionLock
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (
  currencySymbolFromHex,
  mkAdaValue,
  mkInput,
  mkTxOut,
  withAddress,
  withInlineDatum,
  withOutRef,
  withTxOutAddress,
  withTxOutInlineDatum,
  withTxOutValue,
  withValue,
 )

tests :: TestTree
tests =
  testGroup
    "Correction Lock Aiken Parity"
    [ testCase "correction_lock_transition_accepts_acquire_resume_and_terminal_clear" $
        passertEval transitionAccepts
    , testCase "correction_lock_transition_rejects_stale_target_identity_and_unlock" $
        passertEval transitionRejects
    , testCase "correction_lock_idle_reference_accepts_exact_singleton" $
        passertEval $ referencesIdle [lockInput idleDatumData 0] hubPolicy lockAddress
    , testCase "correction_lock_idle_reference_rejects_missing" $
        pfails $ preferencesIdle # pnil # pdata (pconstant hubPolicy) # pconstant lockAddress
    , testCase "correction_lock_idle_reference_rejects_duplicate" $
        pfails $ referencesIdle [lockInput idleDatumData 0, lockInput idleDatumData 1] hubPolicy lockAddress
    , testCase "correction_lock_idle_reference_rejects_locked" $
        passertEval $ pnot # referencesIdle [lockInput lockedDatumData 0] hubPolicy lockAddress
    , testCase "correction_lock_idle_reference_rejects_forged_policy" $
        pfails $ referencesIdle [lockInputWith lockedDatumData forgedPolicy lockAddress 0] hubPolicy lockAddress
    , testCase "correction_lock_idle_reference_rejects_wrong_script" $
        passertEval $
          pnot # referencesIdle [lockInputWith idleDatumData hubPolicy otherAddress 0] hubPolicy lockAddress
    , testCase "correction_lock_correction_input_requires_unique_dedicated_singleton" $
        passertEval $
          pmatch (uniqueInput [lockInput lockedDatumData 0]) $ \PTxInInfo {ptxInInfo'outRef} ->
            pmatch ptxInInfo'outRef $ \PTxOutRef {ptxOutRef'idx} -> pfromData ptxOutRef'idx #== 0
    , testCase "correction_lock_correction_input_rejects_missing" $
        pfails $ puniqueInput # pnil # pdata (pconstant hubPolicy) # pconstant lockAddress
    , testCase "correction_lock_correction_input_rejects_duplicate" $
        pfails $ uniqueInput [lockInput lockedDatumData 0, lockInput lockedDatumData 1]
    , testCase "correction_lock_correction_output_requires_unique_dedicated_singleton" $
        passertEval $
          pdecodeDatum # uniqueOutput [lockOutput lockedDatumData hubPolicy lockAddress mempty]
            #== lockedDatum
    , testCase "correction_lock_correction_output_rejects_missing" $
        pfails $ puniqueOutput # pnil # pdata (pconstant hubPolicy) # pconstant lockAddress
    , testCase "correction_lock_correction_output_rejects_duplicate" $
        pfails $
          uniqueOutput
            [ lockOutput lockedDatumData hubPolicy lockAddress mempty
            , lockOutput lockedDatumData hubPolicy lockAddress mempty
            ]
    , testCase "correction_lock_rejects_non_dedicated_extra_token" $
        passertEval $
          pnot
            # ( poutputIsDedicated
                  # pconstant
                    ( lockOutput
                        lockedDatumData
                        hubPolicy
                        lockAddress
                        (singleton hubPolicy (TokenName "EXTRA") 1)
                    )
                  # pdata (pconstant hubPolicy)
                  # pconstant lockAddress
              )
    ]

transitionAccepts :: forall s. Term s PBool
transitionAccepts =
  pand'List
    [ pdatumTransitionIsValid # pcon PIdle # lockedDatum # lockedDatum # pconstant False
    , pdatumTransitionIsValid # lockedDatum # lockedDatum # lockedDatum # pconstant False
    , pdatumTransitionIsValid # lockedDatum # pcon PIdle # lockedDatum # pconstant True
    , pdatumTransitionIsValid # pcon PIdle # pcon PIdle # lockedDatum # pconstant True
    ]

transitionRejects :: forall s. Term s PBool
transitionRejects =
  pand'List
    [ pnot # (pdatumTransitionIsValid # otherLockedTarget # lockedDatum # lockedDatum # pconstant False)
    , pnot # (pdatumTransitionIsValid # otherLockedIdentity # lockedDatum # lockedDatum # pconstant False)
    , pnot # (pdatumTransitionIsValid # lockedDatum # pcon PIdle # lockedDatum # pconstant False)
    , pnot # (pdatumTransitionIsValid # lockedDatum # lockedDatum # lockedDatum # pconstant True)
    ]

lockedDatum :: forall s. Term s PCorrectionLockDatum
lockedDatum = lockedTerm targetBytes proofName

otherLockedTarget :: forall s. Term s PCorrectionLockDatum
otherLockedTarget = lockedTerm otherTargetBytes proofName

otherLockedIdentity :: forall s. Term s PCorrectionLockDatum
otherLockedIdentity = lockedTerm targetBytes otherProofName

lockedTerm :: forall s. BS.ByteString -> TokenName -> Term s PCorrectionLockDatum
lockedTerm target name =
  pcon $
    PLocked
      (pdata $ pconstant target)
      (pdata $ pcon $ PFraudProof (pdata $ pconstant name))

referencesIdle :: forall s. [TxInInfo] -> CurrencySymbol -> Address -> Term s PBool
referencesIdle inputs policy address =
  preferencesIdle # pconstant inputs # pdata (pconstant policy) # pconstant address

uniqueInput :: forall s. [TxInInfo] -> Term s PTxInInfo
uniqueInput inputs =
  puniqueInput # pconstant inputs # pdata (pconstant hubPolicy) # pconstant lockAddress

uniqueOutput :: forall s. [TxOut] -> Term s PTxOut
uniqueOutput outputs =
  puniqueOutput # pconstant outputs # pdata (pconstant hubPolicy) # pconstant lockAddress

lockInput :: PD.Data -> Integer -> TxInInfo
lockInput datum = lockInputWith datum hubPolicy lockAddress

lockInputWith :: PD.Data -> CurrencySymbol -> Address -> Integer -> TxInInfo
lockInputWith datum policy address index =
  mkInput $
    withOutRef (TxOutRef fixtureTxId index)
      <> withAddress address
      <> withValue (lockValue policy mempty)
      <> withInlineDatum (dataToBuiltinData datum)

lockOutput :: PD.Data -> CurrencySymbol -> Address -> Value -> TxOut
lockOutput datum policy address extraValue =
  mkTxOut $
    withTxOutAddress address
      <> withTxOutValue (lockValue policy extraValue)
      <> withTxOutInlineDatum (dataToBuiltinData datum)

lockValue :: CurrencySymbol -> Value -> Value
lockValue policy extraValue =
  mkAdaValue 2_000_000
    <> singleton policy correctionAssetName 1
    <> extraValue

idleDatumData :: PD.Data
idleDatumData = PD.Constr 0 []

lockedDatumData :: PD.Data
lockedDatumData = lockedData targetBytes proofName

lockedData :: BS.ByteString -> TokenName -> PD.Data
lockedData target (TokenName name) =
  PD.Constr 1 [PD.B target, PD.Constr 0 [PD.B $ builtinBytes name]]

builtinBytes :: BuiltinByteString -> BS.ByteString
builtinBytes = fromBuiltin

hubPolicy, forgedPolicy :: CurrencySymbol
hubPolicy = currencySymbolFromHex $ replicate 56 '1'
forgedPolicy = currencySymbolFromHex $ replicate 56 '2'

lockAddress, otherAddress :: Address
lockAddress = scriptAddress $ replicate 56 '3'
otherAddress = scriptAddress $ replicate 56 '4'

scriptAddress :: String -> Address
scriptAddress hex =
  Address
    (ScriptCredential $ ScriptHash $ toBuiltin $ decodeHex hex)
    Nothing

targetBytes, otherTargetBytes :: BS.ByteString
targetBytes = decodeHex $ replicate 56 'a'
otherTargetBytes = decodeHex $ replicate 56 'b'

proofName, otherProofName :: TokenName
proofName = TokenName $ toBuiltin $ decodeHex ("00000001" <> replicate 56 'a')
otherProofName = TokenName $ toBuiltin $ decodeHex ("00000002" <> replicate 56 'a')

correctionAssetName :: TokenName
correctionAssetName = TokenName "MIDGARD_CORRECTION_LOCK"

fixtureTxId :: TxId
fixtureTxId = TxId $ toBuiltin $ decodeHex $ replicate 64 'c'

decodeHex :: String -> BS.ByteString
decodeHex = Base16.decodeLenient . BSC.pack
