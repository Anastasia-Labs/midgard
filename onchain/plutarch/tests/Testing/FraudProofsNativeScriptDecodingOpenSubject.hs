{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingOpenSubject (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NativeScriptDecoding.OpenSubject (nativeScriptDecodingOpenSubjectValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Native-script decoding open subject"
  [ testCase "commits the authenticated outpoint" $ psucceeds $ run $ runOpen unopenedState openedState nextScript
  , testCase "closes an out-of-domain direction-B ordinal" $ psucceeds $ run $ runOpen directionBState directionBClosed otherScript
  , testCase "rejects an out-of-domain direction-A ordinal" $ pfails $ run $ runOpen directionAOutOfDomain directionAClosed otherScript
  , testCase "rejects a second open" $ pfails $ run $ runOpen openedState openedState nextScript
  ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx = nativeScriptDecodingOpenSubjectValidator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant $ ScriptHash $ toBuiltin otherScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant certificatePolicy)
  # pconstant ctx

acceptedCbor :: BS.ByteString
acceptedCbor = compactWithValidity txScriptSpend (witnessSetHashOf txScriptSpend) 0

fieldOpening :: PD.Data
fieldOpening = bodyOpening acceptedCbor (spendInputsPreimage txScriptSpend)

scanState :: Integer -> Integer -> BS.ByteString -> Integer -> Integer -> PD.Data
scanState direction cursor outpointHash outputIndex refusalClass = PD.Constr 0
  [ PD.I direction
  , PD.I $ if direction == 1 then 1 else 0
  , PD.B txScriptSpendId
  , PD.B $ if direction == 1 then hash32 0x66 else ""
  , PD.I (-1)
  , PD.B $ hash32 0x55
  , PD.I 0
  , PD.I cursor
  , PD.B outpointHash
  , PD.I (-2)
  , PD.I outputIndex
  , PD.I (-1)
  , PD.B ""
  , PD.B ""
  , PD.I refusalClass
  ]

unopenedState, openedState, directionBState, directionBClosed, directionAOutOfDomain, directionAClosed :: PD.Data
unopenedState = scanState 0 0 "" (-1) (-1)
openedState = scanState 0 0 (blake2b256 outpointCbor) outpointIndex (-1)
directionBState = scanState 1 1 "" (-1) (-1)
directionBClosed = scanState 1 1 "" (-1) 0
directionAOutOfDomain = scanState 0 1 "" (-1) (-1)
directionAClosed = scanState 0 1 "" (-1) 0

outpointCbor :: BS.ByteString
outpointCbor = encodedInput outpoint

outpointIndex :: Integer
outpointIndex = snd outpoint

outpoint :: (BS.ByteString, Integer)
outpoint = case spendInputsOf txScriptSpend of
  first : _ -> first
  [] -> error "txScriptSpend fixture has no spend input"

runOpen :: PD.Data -> PD.Data -> BS.ByteString -> ScriptContext
runOpen inputState outputState outputScript =
  spendContext
    (stepDatum $ Just inputState)
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.Constr 0 [fieldOpening]]])
    [threadInput]
    [stepOutput outputScript $ Just outputState]
    []
    []
    mempty
