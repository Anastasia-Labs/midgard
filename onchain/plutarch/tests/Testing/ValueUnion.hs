module Testing.ValueUnion (tests) where

import Data.ByteString qualified as BS
import Midgard.FraudProofs.ValueNotPreserved (PClaimedAssetV1 (..), PClaimedImbalanceDirectionV1 (..))
import Midgard.FraudProofs.ValueUnion
import Midgard.MpfProof.Types (PProof (..))
import Midgard.Validators.FraudProofs.ValueNotPreserved (valueNotPreservedStep01Validator)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTrace, pfails, psucceeds)
import Testing.FraudProofsFixture (ctPolicy, hubOracleHash, nextScript, spendContext, stepDatum, stepOutput, threadInput)

unit :: forall s. Term s PByteString
unit = pconstant $ BS.replicate 28 0x11

add :: forall s. Term s PByteString -> Integer -> Integer -> Term s PByteString
add root qty old = papplyContribution # root # unit # pconstant qty # pconstant old # pcon (PProof pnil)

forced :: forall s. Term s PByteString -> Integer -> Term s PBalanceState
forced root ada = pcon $ PBalanceState (pdata $ pconstant "") (pdata $ pcon PForcedConservation) (pdata $ pconstant "") (pdata $ pconstant ada) (pdata root)

terminal :: forall s. Term s PByteString -> Integer -> Term s PBool
terminal root ada = pterminalClaimHolds # forced root ada # pcon PDNothing

accepted :: forall s. Bool -> Integer -> Term s PByteString -> Term s PBalanceState
accepted token delta root = pmatch (forced root delta) $ \s -> pcon s{pbalanceState'claim = pdata $ pcon $ PAcceptedImbalance (pdata $ if token then pcon $ PTokenAsset (pdata unit) (pdata $ pconstant "") else pcon PAdaAsset) (pdata $ if token then pcon PClaimedAssetDeflated else pcon PClaimedAssetInflated)}

tests :: TestTree
tests =
  testGroup
    "Value union parity"
    [ launchTests
    , testCase "universal conservation closes after input mint output" $ passertEvalNoTrace $ plet (add pemptyDeltaRoot 10 0) $ \input -> plet (add input 3 10) $ \minted ->
        pnot # terminal input 0 #&& pnot # terminal minted 0 #&& terminal (add minted (-13) 13) 0
    , testCase "negative intermediate balance and burn conserve" $ passertEvalNoTrace $ terminal (add (add (add pemptyDeltaRoot (-7) 0) 10 (-7)) (-3) 3) 0
    , testCase "omitted contribution cannot close the universal claim" $ passertEvalNoTrace $ pnot # terminal (add (add pemptyDeltaRoot 10 0) (-9) 10) 0
    , testCase "nonzero ADA prevents conservation" $ passertEvalNoTrace $ pnot # terminal pemptyDeltaRoot 1 #&& pnot # terminal pemptyDeltaRoot (-1)
    , testCase "forged prior delta refuses" $ pfails $ add (add pemptyDeltaRoot 10 0) (-9) 9
    , testCase "zero contribution refuses" $ pfails $ add pemptyDeltaRoot 0 0
    , testCase "short unit refuses" $ pfails $ papplyContribution # pemptyDeltaRoot # pconstant (BS.replicate 27 0) # 1 # 0 # pcon (PProof pnil)
    , testCase "long unit refuses" $ pfails $ papplyContribution # pemptyDeltaRoot # pconstant (BS.replicate 61 0) # 1 # 0 # pcon (PProof pnil)
    , testCase "accepted ADA preserves signed imbalance" $ passertEvalNoTrace $ pterminalClaimHolds # accepted False (-1) pemptyDeltaRoot # pcon PDNothing
    , testCase "balanced accepted ADA refuses conviction" $ passertEvalNoTrace $ pnot # (pterminalClaimHolds # accepted False 0 pemptyDeltaRoot # pcon PDNothing)
    , testCase "opposite accepted imbalance refuses conviction" $ passertEvalNoTrace $ pnot # (pterminalClaimHolds # accepted False 1 pemptyDeltaRoot # pcon PDNothing)
    , testCase "accepted token authenticates signed delta" $ passertEvalNoTrace $ pterminalClaimHolds # accepted True 0 (add pemptyDeltaRoot 1 0) # pcon (PDJust $ pdata $ pcon $ PAssetDeltaWitness (pdata 1) (pdata $ pcon $ PProof pnil))
    , testCase "forced claim refuses selected delta witness" $ pfails $ pterminalClaimHolds # forced pemptyDeltaRoot 0 # pcon (PDJust $ pdata $ pcon $ PAssetDeltaWitness (pdata 0) (pdata $ pcon $ PProof pnil))
    , testCase "mint policy header consumes canonical prefix only" $ passertEvalNoTrace $ pmatch (ppolicyHeader # (phexByteStr "82581c" <> unit <> phexByteStr "a14001") # pcon PDNothing) $ \(PPair policy rest) -> pmatch rest $ \(PPair cursor count) -> policy #== unit #&& cursor #== 32 #&& count #== 1
    , testCase "duplicate policy refuses" $ pfails $ ppolicyHeader # (phexByteStr "82581c" <> unit <> phexByteStr "a14001") # pcon (PDJust $ pdata unit)
    , testCase "empty policy map refuses" $ pfails $ ppolicyHeader # (phexByteStr "82581c" <> unit <> phexByteStr "a0") # pcon PDNothing
    , testCase "asset scan preserves burns and consumed extent" $ passertEvalNoTrace $ pmatch (passet # phexByteStr "414120ff" # pcon PDNothing) $ \(PPair name rest) -> pmatch rest $ \(PPair quantity consumed) -> name #== phexByteStr "41" #&& quantity #== (-1) #&& consumed #== 3
    , testCase "zero asset quantity refuses" $ pfails $ passet # phexByteStr "4000" # pcon PDNothing
    , testCase "duplicate asset refuses" $ pfails $ passet # phexByteStr "414101" # pcon (PDJust $ pdata $ phexByteStr "41")
    ]

launchTests :: TestTree
launchTests =
  testGroup
    "Union launch"
    [ testCase "routes accepted ADA imbalance" $ psucceeds $ launch accepted Nothing nextScript accepted
    , testCase "routes accepted token imbalance" $ psucceeds $ launch token Nothing nextScript token
    , testCase "routes forced conservation" $ psucceeds $ launch forcedClaim Nothing forcedHash forcedClaim
    , testCase "refuses a short token policy" $ pfails $ launch badToken Nothing nextScript badToken
    , testCase "refuses an oversized token name" $ pfails $ launch longName Nothing nextScript longName
    , testCase "refuses relaunch over existing state" $ pfails $ launch accepted (Just accepted) nextScript accepted
    , testCase "refuses the accepted route for forced conservation" $ pfails $ launch forcedClaim Nothing nextScript forcedClaim
    , testCase "refuses the forced route for accepted imbalance" $ pfails $ launch accepted Nothing forcedHash accepted
    , testCase "refuses substituted output claim" $ pfails $ launch accepted Nothing nextScript forcedClaim
    ]
 where
  accepted = PD.Constr 0 [PD.Constr 0 [], PD.Constr 0 []]
  token = PD.Constr 0 [PD.Constr 1 [PD.B $ BS.replicate 28 0x11, PD.B "token"], PD.Constr 1 []]
  forcedClaim = PD.Constr 1 []
  badToken = PD.Constr 0 [PD.Constr 1 [PD.B $ BS.replicate 27 0x11, PD.B "token"], PD.Constr 1 []]
  longName = PD.Constr 0 [PD.Constr 1 [PD.B $ BS.replicate 28 0x11, PD.B $ BS.replicate 33 0], PD.Constr 1 []]

forcedHash :: BS.ByteString
forcedHash = BS.replicate 28 0x99

launch :: forall s. PD.Data -> Maybe PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
launch claim previous destination output =
  valueNotPreservedStep01Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant $ ScriptHash $ toBuiltin forcedHash)
    # pconstant context
 where
  context :: ScriptContext
  context = spendContext (stepDatum previous) (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, claim]]) [threadInput] [stepOutput destination $ Just output] [] [] mempty
