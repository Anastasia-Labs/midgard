{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CorrectionLockValidator
Description : One-to-one port of the tests in @validators/correction-lock.ak@.
-}
module Testing.CorrectionLockValidator (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  POSIXTime (..),
  POSIXTimeRange,
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (SpendingScript),
  ScriptPurpose (Minting),
  TxId (..),
  TxInInfo,
  TxOut,
  TxOutRef (..),
  scriptContextTxInfo,
  toBuiltinData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
  txInfoValidRange,
 )
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (BuiltinData, builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.CorrectionLock (correctionLockSpendValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (
  buildScriptContext,
  currencySymbolFromHex,
  mkAdaValue,
  mkInput,
  mkTxOut,
  withAddress,
  withInlineDatum,
  withMint,
  withOutRef,
  withTxOutAddress,
  withTxOutInlineDatum,
  withTxOutValue,
  withValue,
 )

tests :: TestTree
tests =
  testGroup
    "Correction Lock Validator Aiken Parity"
    [ testCase "accepts permissionless timeout acquire resume and clear" $
        mapM_ (\(datum, ctx) -> psucceeds $ runCorrection datum ctx) timeoutLifecycle
    , testCase "rejects other target resume" $
        pfails $ runCorrection (timeoutLocked otherTarget) $ correctionTx (timeoutLocked otherTarget) (timeoutLocked target) (timeoutRedeemer False) validHub []
    , testCase "rejects nonterminal unlock" $
        pfails $ runCorrection (timeoutLocked target) $ correctionTx (timeoutLocked target) idleDatum (timeoutRedeemer False) validHub []
    , testCase "rejects non-correction state queue redeemer" $
        pfails $ runCorrection idleDatum $ correctionTx idleDatum idleDatum initRedeemer validHub []
    , testCase "rejects cross-deployment state queue policy" $
        pfails $ runCorrection idleDatum $ correctionTx idleDatum (timeoutLocked target) (timeoutRedeemer False) otherStateQueueHub []
    , testCase "rejects forged hub policy" $
        pfails $ runCorrection idleDatum $ correctionTx idleDatum (timeoutLocked target) (timeoutRedeemer False) forgedHub []
    , testCase "binds fraud proof identity" $
        psucceeds $ runCorrection idleDatum $ correctionTx idleDatum (fraudLocked proofAssetName) (fraudRedeemer False) validHub [fraudProofInput proofAssetName]
    , testCase "rejects other fraud identity resume" $
        pfails $ runCorrection (fraudLocked otherProofAssetName) $ correctionTx (fraudLocked otherProofAssetName) (fraudLocked proofAssetName) (fraudRedeemer False) validHub [fraudProofInput proofAssetName]
    , testCase "accepts availability acquire resume and clear" $
        mapM_ (\(datum, ctx) -> psucceeds $ runCorrection datum ctx) availabilityLifecycle
    , testCase "rejects other availability identity resume" $
        pfails $ runCorrection (availabilityLocked challengeAssetName) $ correctionTx (availabilityLocked challengeAssetName) (availabilityLocked otherChallengeAssetName) (availabilityRedeemer otherChallengeAssetName False) validHub []
    , testCase "rejects availability acquire without challenged bond" $
        let base = availabilityAcquireTx challengeAssetName
         in pfails $ runCorrection idleDatum $ base {scriptContextTxInfo = (scriptContextTxInfo base) {txInfoInputs = [lockInput idleDatum]}}
    ]

timeoutLifecycle :: [(PD.Data, ScriptContext)]
timeoutLifecycle =
  [ (idleDatum, correctionTx idleDatum locked (timeoutRedeemer False) validHub [])
  , (locked, correctionTx locked locked (timeoutRedeemer False) validHub [])
  , (locked, correctionTx locked idleDatum (timeoutRedeemer True) validHub [])
  ]
  where
    locked = timeoutLocked target

availabilityLifecycle :: [(PD.Data, ScriptContext)]
availabilityLifecycle =
  [ (idleDatum, availabilityAcquireTx challengeAssetName)
  , (locked, correctionTx locked locked (availabilityRedeemer challengeAssetName False) validHub [])
  , (locked, correctionTx locked idleDatum (availabilityRedeemer challengeAssetName True) validHub [])
  ]
  where
    locked = availabilityLocked challengeAssetName

runCorrection :: forall s. PD.Data -> ScriptContext -> Term s PUnit
runCorrection datum ctx =
  correctionLockSpendValidator
    # pdata (pconstant $ ScriptHash $ unCurrencySymbol hubPolicy)
    # pdata (pconstant availabilityPolicy)
    # pconstant
      ctx
        { scriptContextRedeemer = Redeemer correctRedeemer
        , scriptContextScriptInfo = SpendingScript lockRef (Just $ Datum $ dataToBuiltinData datum)
        }

correctionTx :: PD.Data -> PD.Data -> BuiltinData -> TxInInfo -> [TxInInfo] -> ScriptContext
correctionTx current next stateQueueRedeemer hubInput' extraReferenceInputs =
  contextWith
    [lockInput current]
    [lockOutput next]
    (hubInput' : extraReferenceInputs)
    stateQueueRedeemer
    defaultRange

availabilityAcquireTx :: TokenName -> ScriptContext
availabilityAcquireTx assetName =
  contextWith
    [lockInput idleDatum, availabilityChallengeInput assetName]
    [lockOutput $ availabilityLocked assetName]
    [validHub]
    (availabilityRedeemer assetName False)
    finiteChallengeRange

contextWith :: [TxInInfo] -> [TxOut] -> [TxInInfo] -> BuiltinData -> POSIXTimeRange -> ScriptContext
contextWith inputs outputs referenceInputs stateQueueRedeemer validRange =
  let base = buildScriptContext $ withMint (singleton stateQueuePolicy (TokenName "MBLC") (-1)) stateQueueRedeemer
      info = scriptContextTxInfo base
   in base
        { scriptContextTxInfo =
            info
              { txInfoInputs = inputs
              , txInfoOutputs = outputs
              , txInfoReferenceInputs = referenceInputs
              , txInfoMint = txInfoMint info
              , txInfoRedeemers = AMap.singleton (Minting stateQueuePolicy) (Redeemer stateQueueRedeemer)
              , txInfoValidRange = validRange
              }
        }

lockInput :: PD.Data -> TxInInfo
lockInput datum =
  mkInput $
    withOutRef lockRef
      <> withAddress lockAddress
      <> withValue lockValue
      <> withInlineDatum (dataToBuiltinData datum)

lockOutput :: PD.Data -> TxOut
lockOutput datum =
  mkTxOut $
    withTxOutAddress lockAddress
      <> withTxOutValue lockValue
      <> withTxOutInlineDatum (dataToBuiltinData datum)

lockValue :: Value
lockValue = mkAdaValue 2_000_000 <> singleton hubPolicy correctionAssetName 1

hubInput :: CurrencySymbol -> CurrencySymbol -> TxInInfo
hubInput authenticPolicy queuePolicy =
  mkInput $
    withOutRef (outRef 1)
      <> withAddress (addressOf authenticPolicy)
      <> withValue (mkAdaValue 2_000_000 <> singleton authenticPolicy hubAssetName 1)
      <> withInlineDatum (dataToBuiltinData $ hubDatum queuePolicy)

validHub, otherStateQueueHub, forgedHub :: TxInInfo
validHub = hubInput hubPolicy stateQueuePolicy
otherStateQueueHub = hubInput hubPolicy otherStateQueuePolicy
forgedHub = hubInput miscPolicy stateQueuePolicy

hubDatum :: CurrencySymbol -> PD.Data
hubDatum queuePolicy =
  PD.Constr
    0
    ( map policyData
        [ miscPolicy, miscPolicy, miscPolicy, miscPolicy, queuePolicy, miscPolicy
        , fraudProofPolicy, miscPolicy, miscPolicy, miscPolicy, miscPolicy, miscPolicy
        ]
        <> map
          addressData
          [ miscAddress, miscAddress, miscAddress, miscAddress, addressOf queuePolicy
          , miscAddress, addressOf fraudProofPolicy, miscAddress, miscAddress, miscAddress
          , miscAddress, miscAddress, miscAddress
          ]
        <> [policyData miscPolicy]
    )

fraudProofInput :: TokenName -> TxInInfo
fraudProofInput assetName =
  mkInput $
    withOutRef (outRef 7)
      <> withAddress (addressOf fraudProofPolicy)
      <> withValue (mkAdaValue 2_000_000 <> singleton fraudProofPolicy assetName 1)
      <> withInlineDatum (dataToBuiltinData $ PD.Constr 0 [keyData operatorKey])

availabilityChallengeInput :: TokenName -> TxInInfo
availabilityChallengeInput assetName =
  mkInput $
    withOutRef (outRef 8)
      <> withAddress (addressOf availabilityPolicy)
      <> withValue
        ( mkAdaValue 2_000_000
            <> singleton availabilityPolicy daBondAssetName 1
            <> singleton availabilityPolicy assetName 1
        )
      <> withInlineDatum (dataToBuiltinData $ challengedBondDatum assetName)

challengedBondDatum :: TokenName -> PD.Data
challengedBondDatum assetName =
  PD.Constr
    1
    [ commitmentDatum
    , tokenData daBondAssetName
    , PD.B $ BS.replicate 32 0x99
    , PD.B ""
    , tokenData assetName
    , keyData operatorKey
    , PD.I 0
    , PD.I 100
    ]

commitmentDatum :: PD.Data
commitmentDatum =
  PD.Constr
    0
    [ PD.I 1
    , policyData hubPolicy
    , PD.B target
    , PD.I 1
    , PD.Constr 0 [PD.I 4096, PD.I (4 * 1024 * 1024), PD.I 16]
    , PD.List []
    , keyData operatorKey
    ]

correctRedeemer, initRedeemer :: BuiltinData
correctRedeemer = dataToBuiltinData $ PD.Constr 0 [PD.I 0]
initRedeemer = dataToBuiltinData $ PD.Constr 0 [PD.I 0]

timeoutRedeemer :: Bool -> BuiltinData
timeoutRedeemer terminal = dataToBuiltinData $ PD.Constr 4 [PD.B target, timeoutRemoval terminal]

fraudRedeemer :: Bool -> BuiltinData
fraudRedeemer terminal =
  dataToBuiltinData $
    PD.Constr
      3
      [ keyData operatorKey
      , PD.B target
      , PD.Constr 2 [PD.I 0, PD.I 0]
      , PD.I 1
      , if terminal
          then PD.Constr 0 [outRefData 5, PD.I 0]
          else PD.Constr 1 [outRefData 6, PD.I 0]
      ]

availabilityRedeemer :: TokenName -> Bool -> BuiltinData
availabilityRedeemer assetName terminal =
  dataToBuiltinData $ PD.Constr 5 [PD.B target, tokenData assetName, timeoutRemoval terminal]

timeoutRemoval :: Bool -> PD.Data
timeoutRemoval terminal =
  if terminal
    then PD.Constr 1 [outRefData 3, PD.I 0]
    else PD.Constr 0 [PD.I 1, outRefData 4, PD.I 0]

idleDatum :: PD.Data
idleDatum = PD.Constr 0 []

timeoutLocked :: BS.ByteString -> PD.Data
timeoutLocked headerHash = PD.Constr 1 [PD.B headerHash, PD.Constr 1 []]

fraudLocked :: TokenName -> PD.Data
fraudLocked assetName = PD.Constr 1 [PD.B target, PD.Constr 0 [tokenData assetName]]

availabilityLocked :: TokenName -> PD.Data
availabilityLocked assetName = PD.Constr 1 [PD.B target, PD.Constr 2 [tokenData assetName]]

policyData :: CurrencySymbol -> PD.Data
policyData = PD.B . fromBuiltin . unCurrencySymbol

tokenData :: TokenName -> PD.Data
tokenData = PD.B . fromBuiltin . unTokenName

keyData :: BS.ByteString -> PD.Data
keyData = PD.B

addressData :: Address -> PD.Data
addressData = builtinDataToData . toBuiltinData

outRefData :: Integer -> PD.Data
outRefData = builtinDataToData . toBuiltinData . outRef

outRef :: Integer -> TxOutRef
outRef index = TxOutRef fixtureTxId index

lockRef :: TxOutRef
lockRef = outRef 2

addressOf :: CurrencySymbol -> Address
addressOf = scriptHashAddress . ScriptHash . unCurrencySymbol

lockAddress, miscAddress :: Address
lockAddress = addressOf lockPolicy
miscAddress = addressOf miscPolicy

defaultRange, finiteChallengeRange :: POSIXTimeRange
defaultRange = Interval (LowerBound NegInf True) (UpperBound PosInf True)
finiteChallengeRange = Interval (LowerBound (Finite $ POSIXTime 100) True) (UpperBound (Finite $ POSIXTime 101) True)

decodeHex :: String -> BS.ByteString
decodeHex = Base16.decodeLenient . BSC.pack

repeatedPolicy :: Char -> CurrencySymbol
repeatedPolicy digit = currencySymbolFromHex $ replicate 56 digit

hubPolicy, stateQueuePolicy, otherStateQueuePolicy, fraudProofPolicy, lockPolicy, miscPolicy, availabilityPolicy :: CurrencySymbol
hubPolicy = repeatedPolicy '1'
stateQueuePolicy = repeatedPolicy '2'
otherStateQueuePolicy = currencySymbolFromHex $ concat $ replicate 28 "23"
fraudProofPolicy = repeatedPolicy '3'
lockPolicy = repeatedPolicy '4'
miscPolicy = repeatedPolicy '5'
availabilityPolicy = miscPolicy

target, otherTarget, operatorKey :: BS.ByteString
target = decodeHex $ replicate 56 'a'
otherTarget = decodeHex $ replicate 56 'b'
operatorKey = decodeHex $ replicate 56 '6'

proofAssetName, otherProofAssetName, challengeAssetName, otherChallengeAssetName, daBondAssetName :: TokenName
proofAssetName = TokenName $ toBuiltin $ decodeHex $ "00000001" <> replicate 56 'a'
otherProofAssetName = TokenName $ toBuiltin $ decodeHex $ "00000002" <> replicate 56 'a'
challengeAssetName = TokenName $ toBuiltin $ decodeHex $ "44414348" <> replicate 56 '8'
otherChallengeAssetName = TokenName $ toBuiltin $ decodeHex $ "44414348" <> replicate 56 '9'
daBondAssetName = TokenName $ toBuiltin $ decodeHex $ "4441424e" <> replicate 56 '8'

hubAssetName, correctionAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"
correctionAssetName = TokenName "MIDGARD_CORRECTION_LOCK"

fixtureTxId :: TxId
fixtureTxId = TxId $ toBuiltin $ decodeHex $ replicate 64 '1'
