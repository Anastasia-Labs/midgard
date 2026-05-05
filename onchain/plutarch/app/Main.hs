{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text

import Plutarch.Evaluate (
  applyArguments,
  evalScript,
 )
import Plutarch.Internal.Term
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import System.Directory (createDirectoryIfMissing)
import System.IO
import Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator)

-- | Runs the module's entrypoint.
-- | Writes compiled Plutus scripts to disk for local inspection.
main :: IO ()
main = do
  putStrLn "Writing Plutarch membership scripts to files"
  createDirectoryIfMissing True "generated"
  writePlutusScriptNoTrace
    "midgard.plutarch.phas.membership_stake"
    "generated/membership-stake.plutus.json"
    membershipStakeValidator
  writePlutusScriptNoTrace
    "midgard.plutarch.pexcludes.non_membership_stake"
    "generated/non-membership-stake.plutus.json"
    nonMembershipStakeValidator

-- | Compiles and evaluates a closed term with the supplied arguments.
evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

-- | Compiles and evaluates a closed term without script arguments.
evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

-- | Serializes a compiled Plutarch script as the direct Plutus script CBOR hex.
encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR =
  Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

-- | Writes a compiled term to a cardano-cli compatible Plutus script envelope.
writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term =
  case evalT cfg term of
    Left e -> fail (show e)
    Right (script, _, _) -> do
      let scriptType = "PlutusScriptV3" :: String
          plutusJson =
            object
              [ "type" .= scriptType
              , "description" .= title
              , "cborHex" .= encodeSerialiseCBOR script
              ]
      LBS.writeFile filepath (encodePretty plutusJson)

-- | Writes a compiled term without traces for production script identity.
writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace = writePlutusScript NoTracing
