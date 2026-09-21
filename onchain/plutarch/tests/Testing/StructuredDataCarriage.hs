{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.StructuredDataCarriage (tests) where

import Aiken.Cbor (pdeserialise)
import Data.ByteString qualified as BS
import Midgard.FraudProofs.StructuredDataCarriage (presolve)
import Plutarch.Prelude
import PlutusCore.Data qualified as D
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V3
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (mkAdaValue)

tests :: TestTree
tests =
  testGroup
    "Structured Data Carriage"
    [ testCase "preserves nested constructor, byte, list and map ordering" $
        pass fixture refs expected
    , testCase "inline evidence is independent of reference publications" $
        pass (D.Constr 0 [expected]) [] expected
    , testCase "decodes the target TypeScript map-pair wire golden" $
        passertEval $
          pmatch (pdeserialise # pconstant mapGolden) $ \case
            PNothing -> pconstant False
            PJust tree ->
              presolve
                # (pforgetData $ pconstrBuiltin # 1 # (psingleton # tree))
                # pconstant refs
                #== pconstant (D.Map [(D.B "\xaa", D.B "\xbb")])
    , testCase "rejects obsolete list-of-constructor map pairs" $
        pfails $
          resolve (structured $ D.Constr 3 [D.List [D.Constr 0 [ref 0, ref 1]]]) refs
    , testCase "rejects missing publication" $ pfails $ resolve fixture []
    , testCase "rejects negative reference index" $ pfails $ resolve (structured $ ref (-1)) refs
    , testCase "rejects reference without inline datum" $
        pfails $
          resolve
            (structured $ ref 0)
            [(head refs) {txInInfoResolved = (txInInfoResolved $ head refs) {txOutDatum = NoOutputDatum}}]
    , testCase "reordered references cannot reproduce the committed payload" $
        passertEval $
          pnot # (resolve fixture (refs !! 1 : refs !! 0 : drop 2 refs) #== pconstant expected)
    , testCase "rejects non-byte publication in byte concatenation" $
        pfails $
          resolve (structured $ D.Constr 4 [D.List [ref 2]]) refs
    , testCase "rejects non-list constructor fields" $
        pfails $
          resolve (structured $ D.Constr 1 [D.I 0, ref 0]) refs
    , testCase "rejects non-map publication in map concatenation" $
        pfails $
          resolve (structured $ D.Constr 6 [D.List [ref 2]]) refs
    , testCase "preserves duplicate keys and supplied map order" $
        pass
          (structured $ D.Constr 6 [D.List [ref 4, ref 4]])
          refs
          (D.Map [(D.B "\xaa", D.I 1), (D.B "\xaa", D.I 1)])
    , testCase "accepts empty parts in each collection" $
        passertEval $
          resolve (structured $ D.Constr 4 [D.List []]) []
            #== pconstant (D.B "")
            #&& resolve (structured $ D.Constr 5 [D.List []]) []
            #== pconstant (D.List [])
            #&& resolve (structured $ D.Constr 6 [D.List []]) []
            #== pconstant (D.Map [])
    , testCase "rejects unknown tree constructor" $ pfails $ resolve (structured $ D.Constr 7 []) refs
    , testCase "rejects surplus reference fields" $ pfails $ resolve (structured $ D.Constr 0 [D.I 0, D.I 1]) refs
    ]
  where
    pass evidence references value = passertEval $ resolve evidence references #== pconstant value

resolve :: forall s. D.Data -> [TxInInfo] -> Term s PData
resolve evidence references = presolve # pconstant evidence # pconstant references

structured :: D.Data -> D.Data
structured tree = D.Constr 1 [tree]
ref :: Integer -> D.Data
ref index = D.Constr 0 [D.I index]

fixture, expected :: D.Data
fixture =
  structured $
    D.Constr
      1
      [ D.I 7
      , D.Constr
          2
          [ D.List
              [ D.Constr 4 [D.List [ref 0, ref 1]]
              , D.Constr 5 [D.List [ref 2, D.Constr 2 [D.List [ref 3]]]]
              , D.Constr 6 [D.List [ref 4, D.Constr 3 [D.Map [(ref 1, ref 3)]]]]
              ]
          ]
      ]
expected =
  D.Constr
    7
    [D.B "\xaa\xbb", D.List [D.I 1, D.I 2, D.I 3], D.Map [(D.B "\xaa", D.I 1), (D.B "\xbb", D.I 3)]]

refs :: [TxInInfo]
refs =
  zipWith
    reference
    [0 ..]
    [D.B "\xaa", D.B "\xbb", D.List [D.I 1, D.I 2], D.I 3, D.Map [(D.B "\xaa", D.I 1)]]
  where
    reference index value =
      TxInInfo
        (TxOutRef (TxId "fixture") index)
        ( TxOut
            (scriptHashAddress $ ScriptHash "fixture")
            (mkAdaValue 2_000_000)
            (OutputDatum $ Datum $ dataToBuiltinData value)
            Nothing
        )

mapGolden :: BS.ByteString
mapGolden = BS.pack [0xd8, 0x7c, 0x9f, 0xbf, 0xd8, 0x79, 0x9f, 0x00, 0xff, 0xd8, 0x79, 0x9f, 0x01, 0xff, 0xff, 0xff]
