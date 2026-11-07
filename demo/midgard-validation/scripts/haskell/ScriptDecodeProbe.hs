-- Cardano's own script decoder over `name \t script-hex` rows on stdin.
--
-- Each script is the CBOR-wrapped bytes of a PlutusV3 script, as it sits in
-- a transaction. It is decoded with `deserialiseScript` at protocol version
-- 11, and a decoded one is also checked for evaluation with
-- `mkTermToEvaluate` (no arguments). Each row prints one of
--
--   name \t ok \t <canonical Flat> \t phase2ok
--   name \t ok \t <canonical Flat> \t phase2 <EvaluationError constructor>
--   name \t err \t <the deserialisation error, shown>
--
-- `scripts/generate-script-decode-haskell-v1-vectors.mjs` drives it. Build it
-- with GHC 9.6.7 against plutus-ledger-api and plutus-core 1.71.0.0, for
-- example `ghc -O ScriptDecodeProbe.hs -o script-decode-probe` in an
-- environment that exposes both packages.
import Control.Monad.Except (runExcept)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import PlutusCore.Flat (flat)
import PlutusLedgerApi.Common
import qualified PlutusLedgerApi.V3 as V3
import qualified UntypedPlutusCore as UPLC

split :: String -> (String, String)
split l = let (n, rest) = break (== '\t') l in (n, takeWhile (/= '\t') (drop 1 rest))

main :: IO ()
main = do
  rows <- map split . lines <$> getContents
  mapM_ run rows
  where
    run (name, hex) = do
      let script = SBS.toShort (either error id (B16.decode (BS8.pack hex)))
      case V3.deserialiseScript (MajorProtocolVersion 11) script of
        Left e -> putStrLn $ name ++ "\terr\t" ++ takeWhile (/= '\n') (show e)
        Right s -> do
          let ScriptNamedDeBruijn p = deserialisedScript s
              p' = UPLC.programMapNames UPLC.unNameDeBruijn p
              out = BS8.unpack (B16.encode (flat (UPLC.UnrestrictedProgram p')))
              ph2 = case runExcept (mkTermToEvaluate PlutusV3 (MajorProtocolVersion 11) s []) of
                Left e -> "phase2 " ++ takeWhile (/= ' ') (show (e :: EvaluationError))
                Right _ -> "phase2ok"
          putStrLn $ name ++ "\tok\t" ++ out ++ "\t" ++ ph2
