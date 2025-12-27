{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Midgard.Utils where

import Plutarch.LedgerApi.V3 (
  PRedeemer (PRedeemer),
  PScriptContext,
  PScriptInfo (PCertifyingScript, PRewardingScript),
  PTxCert (PTxCertRegStaking),
 )

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
  Config (..),
  compile,
 )
import Plutarch.Builtin (PIsData (pfromDataImpl), pasList)
import Plutarch.Evaluate (
  applyArguments,
  evalScript,
 )
import Plutarch.Prelude (
  ClosedTerm,
  PBool,
  PListLike (pelimList),
  PUnit,
  Term,
  pconstant,
  perror,
  pfield,
  pif,
  plam,
  pmatchC,
  unTermCont,
  (#),
  type (:-->),
 )
import Plutarch.Script (Script, serialiseScript)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )

stakingWrapper :: (PIsData a) => Term s ((a :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking {} -> pconstant ()
        _ -> perror
    PRewardingScript {} -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $
        pif
          (validationFunction # pfromDataImpl (punsafeCoerce redData))
          (pconstant ())
          perror
    _ -> pure perror

stakingWrapper2 :: (PIsData a, PIsData b) => Term s ((a :--> b :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper2 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking {} -> pconstant ()
        _ -> perror
    PRewardingScript {} -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $
        pelimList
          ( \(pfromDataImpl . punsafeCoerce -> a) xs ->
              pelimList
                ( \(pfromDataImpl . punsafeCoerce -> b) _ ->
                    pif (validationFunction # a # b) (pconstant ()) perror
                )
                perror
                xs
          )
          perror
          (pasList # redData)
    _ -> pure perror

stakingWrapper3 :: (PIsData a, PIsData b, PIsData c) => Term s ((a :--> b :--> c :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper3 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking {} -> pconstant ()
        _ -> perror
    PRewardingScript {} -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $
        pelimList
          ( \(pfromDataImpl . punsafeCoerce -> a) xs ->
              pelimList
                ( \(pfromDataImpl . punsafeCoerce -> b) xs' ->
                    pelimList
                      ( \(pfromDataImpl . punsafeCoerce -> c) _ ->
                          pif (validationFunction # a # b # c) (pconstant ()) perror
                      )
                      perror
                      xs'
                )
                perror
                xs
          )
          perror
          (pasList # redData)
    _ -> pure perror

stakingWrapper4 ::
  (PIsData a, PIsData b, PIsData c, PIsData d) =>
  Term s ((a :--> b :--> c :--> d :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper4 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking {} -> pconstant ()
        _ -> perror
    PRewardingScript {} -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $
        pelimList
          ( \(pfromDataImpl . punsafeCoerce -> a) xs ->
              pelimList
                ( \(pfromDataImpl . punsafeCoerce -> b) xs' ->
                    pelimList
                      ( \(pfromDataImpl . punsafeCoerce -> c) xs'' ->
                          pelimList
                            ( \(pfromDataImpl . punsafeCoerce -> d) _ ->
                                pif (validationFunction # a # b # c # d) (pconstant ()) perror
                            )
                            perror
                            xs''
                      )
                      perror
                      xs'
                )
                perror
                xs
          )
          perror
          (pasList # redData)
    _ -> pure perror

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term = do
  case evalT cfg term of
    Left e -> print e
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

-- writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
-- writePlutusScriptTraceBind title filepath term =
--   writePlutusScript (Tracing LogInfo DoTracingAndBinds) title filepath term

-- writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
-- writePlutusScriptTrace title filepath term =
--   writePlutusScript (Tracing LogInfo DoTracing) title filepath term

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace title filepath term =
  writePlutusScript NoTracing title filepath term
