module Testing.TransactionOutputNonCanonical (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.TransactionOutputNonCanonical
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (PNetworkIdMismatch, POutputNonCanonical))
import Testing.Eval (passertEvalNoTrace, pfails)

subject :: forall s. Bool -> Integer -> Term s PVerdictSubject
subject forced index =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ BS.pack [0 .. 31])
      (pdata $ pconstant $ if forced then "order-key" else "")
      (pdata $ if forced then pcon $ PDJust $ pdata $ pcon $ POutputNonCanonical $ pdata $ pconstant index else pcon PDNothing)

initial :: forall s. Bool -> BS.ByteString -> Term s POutputScanState
initial forced output = pinitialScan # (pbindOutput # subject forced 0 # 0) # pconstant output

scanUntil :: forall s. Term s (POutputScanState :--> PByteString :--> PInteger :--> POutputScanState)
scanUntil = pfix $ \self -> plam $ \state output budget -> P.do
  POutputScanState{poutputScan'control, poutputScan'outcome} <- pmatch state
  pif
    (pfromData poutputScan'outcome #== poutcomeScanning #&& budget #> 0)
    ( P.do
        Scan.PLedgerOutputScanControlV1{Scan.pscan'cursor = cursorData, Scan.pscan'stage = stageData} <- pmatch $ pfromData poutputScan'control
        let cursor = pfromData cursorData
            start = cursor - pmod # cursor # pscanChunkBytes
            maxLen = pif (pfromData stageData #<= 4) pscanWindowBytes pscanChunkBytes
            remaining = plengthBS # output - start
            len = pif (remaining #< maxLen) remaining maxLen
        self # (padvanceScan # state # (psliceBS # start # len # output)) # output # (budget - 1)
    )
    state

terminal :: forall s. Bool -> BS.ByteString -> Term s POutputScanState
terminal forced output = scanUntil # initial forced output # pconstant output # 32

canonical, malformed, maximumOutput :: BS.ByteString
canonical = Base16.decodeLenient "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"
malformed = "\xb8\x02" <> BS.tail canonical
maximumOutput = "\xa3" <> BS.tail canonical <> "\x02\x59\x3f\xa0" <> BS.replicate 16288 0

mutate :: (forall s. POutputScanState s -> POutputScanState s) -> (forall s. Term s POutputScanState) -> (forall s. Term s POutputScanState)
mutate f state = pmatch state $ \st -> pcon $ f st

tests :: TestTree
tests =
  testGroup
    "Transaction output canonicality scan"
    [ testCase "convicts accepted malformed output" $ passertEvalNoTrace $ pterminalContradiction # terminal False malformed
    , testCase "convicts wrongfully rejected canonical output" $ passertEvalNoTrace $ pterminalContradiction # terminal True canonical
    , testCase "refuses honestly accepted canonical output" $ passertEvalNoTrace $ pnot # (pterminalContradiction # terminal False canonical)
    , testCase "refuses honestly rejected malformed output" $ passertEvalNoTrace $ pnot # (pterminalContradiction # terminal True malformed)
    , testCase "binds the exact forced output coordinate" $ passertEvalNoTrace $ pmatch (pbindOutput # subject True 7 # 7) $ \PBoundOutput{pboundOutput'index} -> pfromData pboundOutput'index #== 7
    , testCase "refuses coordinate substitution" $ pfails $ pbindOutput # subject True 7 # 6
    , testCase "refuses negative coordinates" $ pfails $ pbindOutput # subject False 0 # (-1)
    , testCase "refuses another rejection reason" $ pfails $ pbindOutput # (pmatch (subject True 0) $ \s -> pcon s{psubject'rejectionReason = pdata $ pcon $ PDJust $ pdata $ pcon PNetworkIdMismatch}) # 0
    , testCase "refuses substituted output bytes" $ pfails $ padvanceScan # initial False canonical # pconstant malformed
    , testCase "refuses a changed checkpoint length" $ pfails $ padvanceScan # mutate (\s -> s{poutputScan'itemLength = pdata $ pconstant $ fromIntegral $ BS.length canonical + 1}) (initial False canonical) # pconstant canonical
    , testCase "refuses an oversized output before scanning" $ pfails $ initial False $ BS.replicate 16385 0
    , testCase "scans maximum output with bounded windows" $ passertEvalNoTrace $ pterminalContradiction # terminal True maximumOutput
    , testCase "refuses a substituted chunk digest" $ pfails $ padvanceScan # mutate (\s -> s{poutputScan'chunkHashes = pdata $ pcons # pdata (pconstant $ BS.replicate 32 0xff) # pnil}) (initial False canonical) # pconstant canonical
    , testCase "refuses an oversized window" $ pfails $ padvanceScan # initial False canonical # pconstant (canonical <> "\x00")
    , testCase "refuses a truncated window" $ pfails $ padvanceScan # initial False canonical # pconstant (BS.init canonical)
    , testCase "refuses finalization during scanning" $ pfails $ pterminalContradiction # initial True canonical
    , testCase "refuses advancing terminal state" $ pfails $ padvanceScan # terminal True canonical # pconstant canonical
    , testCase "empty output is noncanonical" $ passertEvalNoTrace $ pterminalContradiction # terminal False ""
    , testCase "refuses an invalid outcome" $ passertEvalNoTrace $ pnot # (pstateIsWellFormed # mutate (\s -> s{poutputScan'outcome = pdata 3}) (initial False canonical))
    , testCase "canonical terminal must be exact" $ passertEvalNoTrace $ pnot # (pstateIsWellFormed # mutate (\s -> s{poutputScan'outcome = pdata 1}) (initial False canonical))
    ]
