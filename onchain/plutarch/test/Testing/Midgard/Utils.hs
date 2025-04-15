{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Testing.Midgard.Utils (main) where

import Data.Text (Text)
import Midgard.Utils
import Plutarch
import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude
import PlutusLedgerApi.V1 (interval)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as BI
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Staking Wrapper Tests"
      [ testGroup
          "Valid rewarding script"
          [ testCase "1" $ assertRight $ mkTest f1 validRewardingCtx1
          , testCase "2" $ assertRight $ mkTest f2 validRewardingCtx
          , testCase "3" $ assertRight $ mkTest f3 validRewardingCtx
          , testCase "4" $ assertRight $ mkTest f4 validRewardingCtx
          ]
      , testGroup
          "Invalid rewarding script"
          [ testCase "1" $ assertLeft $ mkTest f1 invalidRewardingCtx1
          , testCase "2" $ assertLeft $ mkTest f2 invalidRewardingCtx
          , testCase "3" $ assertLeft $ mkTest f3 invalidRewardingCtx
          , testCase "4" $ assertLeft $ mkTest f4 invalidRewardingCtx
          ]
      , testGroup
          "Valid certificates"
          [ testCase "1" $ assertRight $ mkTest f1 validCertsCtx1
          , testCase "2" $ assertRight $ mkTest f2 validCertsCtx
          , testCase "3" $ assertRight $ mkTest f3 validCertsCtx
          , testCase "4" $ assertRight $ mkTest f4 validCertsCtx
          ]
      , testGroup
          "Invalid certificates"
          [ testCase "1" $ assertLeft $ mkTest f1 invalidCertsCtx1
          , testCase "2" $ assertLeft $ mkTest f2 invalidCertsCtx
          , testCase "3" $ assertLeft $ mkTest f3 invalidCertsCtx
          , testCase "4" $ assertLeft $ mkTest f4 invalidCertsCtx
          ]
      , testGroup
          "Invalid redeemer"
          [ testCase "1" $ assertLeft $ mkTest f1 invalidRedCtx
          , testCase "2" $ assertLeft $ mkTest f2 invalidRedCtx
          , testCase "3" $ assertLeft $ mkTest f3 invalidRedCtx
          , testCase "4" $ assertLeft $ mkTest f4 invalidRedCtx
          ]
      ]
  where
    f1, f2, f3, f4 :: ClosedTerm (PScriptContext :--> PUnit)
    f1 = stakingWrapper # fun
    f2 = stakingWrapper2 # fun2
    f3 = stakingWrapper3 # fun3
    f4 = stakingWrapper4 # fun4

    validRewardingCtx = mkCtx rightRed [] (RewardingScript (PubKeyCredential ""))
    invalidRewardingCtx = mkCtx rightRed [] (VotingScript (DRepVoter (DRepCredential (PubKeyCredential ""))))
    validCertsCtx = mkCtx rightRed [rightCert] (CertifyingScript 0 rightCert)
    invalidCertsCtx = mkCtx rightRed [wrongCert] (CertifyingScript 0 wrongCert)
    invalidRedCtx = mkCtx (dataToBuiltinData $ B "") [] (RewardingScript (PubKeyCredential ""))

    validRewardingCtx1 = validRewardingCtx {scriptContextRedeemer = Redeemer rightRed1}
    invalidRewardingCtx1 = invalidRewardingCtx {scriptContextRedeemer = Redeemer rightRed1}
    validCertsCtx1 = validCertsCtx {scriptContextRedeemer = Redeemer rightRed1}
    invalidCertsCtx1 = invalidCertsCtx {scriptContextRedeemer = Redeemer rightRed1}

-- Helper assertions
assertRight :: Either Text a -> Assertion
assertRight (Right _) = return ()
assertRight (Left err) = assertFailure $ "Expected success but got: " ++ show err

assertLeft :: Either Text a -> Assertion
assertLeft (Left _) = return ()
assertLeft (Right _) = assertFailure "Expected failure but got success"

mkTest :: ClosedTerm (PScriptContext :--> PUnit) -> ScriptContext -> Either Text (Script, ExBudget, [Text])
mkTest fun ctx = evalWithArgsT mempty fun [toData ctx]

fun :: Term s (PInteger :--> PBool)
fun = plam $ \a -> a #== 1

fun2 :: Term s (PInteger :--> PInteger :--> PBool)
fun2 = plam $ \a _ -> a #== 1

fun3 :: Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
fun3 = plam $ \a _ _ -> a #== 1

fun4 :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
fun4 = plam $ \a _ _ _ -> a #== 1

rightRed :: BI.BuiltinData
rightRed = PlutusTx.dataToBuiltinData $ PlutusTx.List [PlutusTx.I 1, PlutusTx.I 1, PlutusTx.I 3, PlutusTx.I 4]

rightRed1 :: BI.BuiltinData
rightRed1 = PlutusTx.dataToBuiltinData $ PlutusTx.I 1

mkCtx :: BI.BuiltinData -> [TxCert] -> ScriptInfo -> ScriptContext
mkCtx red certs = ScriptContext txInfo (Redeemer red)
  where
    txInfo =
      TxInfo
        mempty
        mempty
        mempty
        0 -- fee
        emptyMintValue -- mint
        certs -- certs
        AssocMap.empty -- withdrawals
        (interval (POSIXTime 1) (POSIXTime 2)) -- valid range
        mempty -- signatories
        AssocMap.empty -- redeemers
        AssocMap.empty -- data
        (TxId mempty) -- id
        AssocMap.empty -- votes
        mempty -- proposal procedures
        Nothing -- current treasury amount
        Nothing -- current treasury donation

wrongCert :: TxCert
wrongCert = TxCertPoolRetire "" 100

rightCert :: TxCert
rightCert = TxCertRegStaking (PubKeyCredential "") Nothing
