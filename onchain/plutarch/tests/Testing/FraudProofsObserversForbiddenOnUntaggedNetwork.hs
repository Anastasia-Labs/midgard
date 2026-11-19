{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsObserversForbiddenOnUntaggedNetwork (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ObserversForbiddenOnUntaggedNetwork
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.RejectionReason (PRejectionReasonV1 (PNetworkIdMismatch, PObserversForbiddenOnUntaggedNetwork))
import Midgard.ScriptLanguageViews (pemptyScriptIntegrityHash)
import Testing.Eval (passertEvalNoTrace, pfails)

subject :: forall s. Bool -> Bool -> Term s PVerdictSubject
subject forced exactReason =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ BS.pack [0 .. 31])
      (pdata $ pconstant $ if forced then "source" else "")
      ( pdata $
          if forced
            then
              pcon $
                PDJust $
                  pdata $
                    if exactReason
                      then pcon PObserversForbiddenOnUntaggedNetwork
                      else pcon PNetworkIdMismatch
            else pcon PDNothing
      )

state :: forall s. Bool -> Integer -> Term s PStateV1
state forced networkId = pbindStateV1 # subject forced True # pconstant networkId

presentHash :: forall s. Term s PByteString
presentHash = pconstant $ BS.replicate 32 0xab

tests :: TestTree
tests =
  testGroup
    "Observers forbidden on untagged network"
    [ testCase "holds only for present-hash observers on scalar 255" $
        passertEvalNoTrace $
          pforbiddenObserversHoldV1
            # 1
            # 255
            # presentHash
            #&& pnot
            # (pforbiddenObserversHoldV1 # 1 # 255 # pemptyScriptIntegrityHash)
            #&& pnot
            # (pforbiddenObserversHoldV1 # 0 # 255 # presentHash)
            #&& pnot
            # (pforbiddenObserversHoldV1 # 1 # 0 # presentHash)
            #&& pnot
            # (pforbiddenObserversHoldV1 # 1 # 1 # presentHash)
    , testCase "refuses malformed integrity hash" $
        pfails $
          pforbiddenObserversHoldV1 # 1 # 255 # phexByteStr "ab"
    , testCase "convicts accepted nonempty untagged observers" $
        passertEvalNoTrace $
          pterminalContradictionV1 # state False 255 # 1 # presentHash
    , testCase "refuses accepted empty observers" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # state False 255 # 0 # presentHash)
    , testCase "refuses accepted tagged observers" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # state False 1 # 1 # presentHash)
    , testCase "refuses accepted native-only observers" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # state False 255 # 1 # pemptyScriptIntegrityHash)
    , testCase "refuses negative observer count" $
        pfails $
          pterminalContradictionV1 # state False 255 # (-1) # presentHash
    , testCase "convicts wrongful rejection of empty observers" $
        passertEvalNoTrace $
          pterminalContradictionV1 # state True 255 # 0 # presentHash
    , testCase "convicts wrongful rejection of tagged observers" $
        passertEvalNoTrace $
          pterminalContradictionV1 # state True 0 # 1 # presentHash
    , testCase "convicts wrongful rejection of native-only observers" $
        passertEvalNoTrace $
          pterminalContradictionV1 # state True 255 # 1 # pemptyScriptIntegrityHash
    , testCase "refuses honest nonempty untagged rejection" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # state True 255 # 1 # presentHash)
    , testCase "refuses sibling transaction-global reason" $
        pfails $
          pbindStateV1 # subject True False # 255
    , testCase "refuses noncanonical network scalar" $
        pfails $
          pbindStateV1 # subject False True # 2
    , testCase "binds every canonical network scalar" $
        passertEvalNoTrace $
          pmatch (state False 0) (\PStateV1{pstate'networkId} -> pfromData pstate'networkId #== 0)
            #&& pmatch (state False 1) (\PStateV1{pstate'networkId} -> pfromData pstate'networkId #== 1)
            #&& pmatch (state True 255) (\PStateV1{pstate'networkId} -> pfromData pstate'networkId #== 255)
    , testCase "zero bytes are a present integrity hash" $
        passertEvalNoTrace $
          pforbiddenObserversHoldV1 # 1 # 255 # pconstant (BS.replicate 32 0)
    , testCase "state Data ABI matches target" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData (pdata $ state False 255)
            #== pconstant
              (hex "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff18ffff")
    ]

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
