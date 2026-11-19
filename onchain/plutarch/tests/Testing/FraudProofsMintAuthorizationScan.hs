module Testing.FraudProofsMintAuthorizationScan (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Midgard.FraudProofs.MintAuthorizationScan
import Midgard.NativeScriptScan (PNativeScriptFrameV1 (..))
import Midgard.Validators.FraudProofs.MintAuthorization (mintAuthorizationEvaluateValidator, mintAuthorizationWitnessScanValidator)
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (Datum (..), OutputDatum (..), ScriptContext, ScriptHash (..), TxInInfo (..), TxOut (..))
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTrace, pfails, psucceeds)
import Testing.FraudProofsFixture (blake2b256, ctPolicy, nextScript, spendContext, stepDatum, stepOutput, stepScript, threadInput, wrapItem)

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient

mint :: BS.ByteString -> BS.ByteString
mint assets = "\x81" <> wrapItem (hex "82581c" <> BS.replicate 28 0x42 <> hex assets)

scanMint :: forall s. BS.ByteString -> Integer -> Term s PMintControl
scanMint bytes n = padvanceMint # (pinitialMint # pconstant bytes # 0) # pconstant bytes # 0 # pconstant n

complete :: forall s. BS.ByteString -> Term s PBool
complete bytes = pmintComplete # scanMint bytes 32 # pconstant (fromIntegral $ BS.length bytes)

bytesList :: forall s. [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
bytesList = foldr (\bytes rest -> pcons # pdata (pconstant bytes) # rest) pnil

witness :: forall s. BS.ByteString -> Integer -> Term s PWitnessScanState
witness bytes count =
  pcon $
    PWitnessScanState
      (pdata $ pconstant $ BS.replicate 28 0xff)
      (pdata $ pconstant "")
      (pdata $ pconstant "")
      (pdata $ pconstant $ fromIntegral $ BS.length bytes)
      (pdata $ bytesList [blake2b256 bytes])
      (pdata 1)
      (pdata 0)
      (pdata $ pconstant count)

initial :: forall s. BS.ByteString -> Integer -> Integer -> Term s PEvaluateState
initial bytes start end =
  pcon $
    PEvaluateState
      (pdata $ pconstant "")
      (pdata $ pconstant $ fromIntegral $ BS.length bytes)
      (pdata $ pconstant $ fromIntegral $ BS.length bytes)
      (pdata 0)
      (pdata 0)
      (pdata 0)
      (pdata $ bytesList [blake2b256 bytes])
      (pdata pnil)
      (pdata $ pconstant start)
      (pdata $ pconstant end)
      (pdata 0)
      (pdata 0)
      (pdata $ pconstant "")
      (pdata 0)
      (pdata (-1))

operations :: forall s. [Term s PEvaluateOperation] -> Term s (PBuiltinList (PAsData PEvaluateOperation))
operations = foldr (\op rest -> pcons # pdata op # rest) pnil

frame :: forall s. Integer -> Term s PEvaluateOperation
frame kind = pcon $ PFrame $ pdata $ pcon $ PNativeScriptFrameV1 (pdata $ pconstant "") (pdata $ pconstant kind) (pdata 1) (pdata 1) (pdata 0) (pdata 0)

result :: forall s. BS.ByteString -> Integer -> Integer -> Integer -> Term s PBool
result bytes start end expected = pmatch (padvanceEvaluation # initial bytes start end # pconstant bytes # operations [pcon PToken]) $ \s -> pfromData (pevaluate'result s) #== pconstant expected

signerRaw :: BS.ByteString
signerRaw = hex "8200581c" <> BS.replicate 28 0 <> hex "815865825820" <> BS.replicate 32 0 <> hex "5840" <> BS.replicate 64 0

signerState :: forall s. Term s PEvaluateState
signerState = pmatch (initial signerRaw (-1) (-1)) $ \s ->
  pcon
    s
      { pevaluate'scriptLength = pdata 32
      , pevaluate'signerStart = pdata 33
      , pevaluate'signerCount = pdata 1
      }

tests :: TestTree
tests =
  testGroup
    "Mint-authorization scan parity"
    [ stagedTests
    , testGroup
        "Mint-authorization bounded scans"
        [ testCase "opening a selected policy consumes one operation" $ passertEvalNoTrace $ pmatch (scanMint (mint "a241410142414220") 1) $ \s -> pfromData (pmint'remainingAssets s) #== 2
        , testCase "resumes through positive mint and burn" $ passertEvalNoTrace $ let bytes = mint "a241410142414220" in pmintComplete # (padvanceMint # scanMint bytes 1 # pconstant bytes # 0 # 2) # pconstant (fromIntegral $ BS.length bytes)
        , testCase "does not finish after one of two assets" $ passertEvalNoTrace $ pmatch (scanMint (mint "a241410142414220") 2) $ \s -> pfromData (pmint'remainingAssets s) #== 1
        , testCase "trailing field bytes cannot complete" $ passertEvalNoTrace $ pnot # complete (mint "a14001" <> "\x00")
        , testCase "overdeclared policy count refuses" $ pfails $ scanMint ("\x82" <> BS.tail (mint "a14001")) 32
        , testCase "zero mint refuses" $ pfails $ scanMint (mint "a14000") 32
        , testCase "noncanonical asset order refuses" $ pfails $ scanMint (mint "a242414201414101") 32
        , testCase "duplicate asset names refuse" $ pfails $ scanMint (mint "a2414101414101") 32
        , testCase "trailing selected item refuses" $ pfails $ scanMint (mint "a1400100") 32
        , testCase "empty selected map refuses" $ pfails $ scanMint (mint "a0") 32
        , testCase "out-of-range policy refuses" $ pfails $ pinitialMint # pconstant (mint "a14001") # 1
        , testCase "negative policy refuses" $ pfails $ pinitialMint # pconstant (mint "a14001") # (-1)
        , testCase "witness scan consumes a canonical envelope" $ passertEvalNoTrace $ pmatch (padvanceWitness # witness (hex "8143820340") 1 # pconstant (hex "8143820340") # 16) $ \s -> pfromData (pwitness'cursor s) #== 5 #&& pfromData (pwitness'itemIndex s) #== 1
        , testCase "nonminimal witness envelope refuses" $ pfails $ padvanceWitness # witness (hex "815803820340") 1 # pconstant (hex "815803820340") # 16
        , testCase "overdeclared witness count refuses" $ pfails $ padvanceWitness # witness (hex "8143820340") 2 # pconstant (hex "8143820340") # 16
        , testCase "substituted witness chunk refuses" $ pfails $ pauthenticateWitness # witness (hex "8143820340") 1 # bytesList [hex "8143820341"]
        , testCase "authenticates exact witness chunks" $ passertEvalNoTrace $ pauthenticateWitness # witness (hex "8143820340") 1 # bytesList [hex "8143820340"] #== pconstant (hex "8143820340")
        , testCase "nested any evaluates false" $ passertEvalNoTrace $ let bytes = hex "820281820280" in pmatch (padvanceEvaluation # initial bytes (-1) (-1) # pconstant bytes # operations [pcon PToken, pcon PToken, frame 2]) $ \s -> pfromData (pevaluate'result s) #== 0 #&& pfromData (pevaluate'cursor s) #== 6 #&& pfromData (pevaluate'stackDepth s) #== 0 #&& pfromData (pevaluate'stackRoot s) #== pconstant ""
        , testCase "nested empty all evaluates true" $ passertEvalNoTrace $ let bytes = hex "820281820180" in pmatch (padvanceEvaluation # initial bytes (-1) (-1) # pconstant bytes # operations [pcon PToken, pcon PToken, frame 2]) $ \s -> pfromData (pevaluate'result s) #== 1
        , testCase "forged stack frame refuses" $ pfails $ let bytes = hex "820281820280" in padvanceEvaluation # initial bytes (-1) (-1) # pconstant bytes # operations [pcon PToken, pcon PToken, frame 1]
        , testCase "empty operation batch refuses" $ pfails $ padvanceEvaluation # initial (hex "820280") (-1) (-1) # pconstant (hex "820280") # pnil
        , testCase "noncanonical token refuses" $ pfails $ let bytes = hex "82180280" in padvanceEvaluation # initial bytes (-1) (-1) # pconstant bytes # operations [pcon PToken]
        , testCase "signer extraction hashes the verification key" $ passertEvalNoTrace $ pmatch (padvanceEvaluation # signerState # pconstant signerRaw # operations [pcon PSigner]) $ \s -> pfromData (pevaluate'signerIndex s) #== 1 #&& pfromData (phead # pfromData (pevaluate'signerHashes s)) #== pblake2b_224 # pconstant (BS.replicate 32 0)
        , testCase "token before signer completion refuses" $ pfails $ padvanceEvaluation # signerState # pconstant signerRaw # operations [pcon PToken]
        , testCase "extra signer refuses" $ pfails $ padvanceEvaluation # signerState # pconstant signerRaw # operations [pcon PSigner, pcon PSigner]
        , testCase "noncanonical signer envelope refuses" $ pfails $ padvanceEvaluation # signerState # pconstant (BS.take 33 signerRaw <> hex "5864" <> BS.drop 35 signerRaw) # operations [pcon PSigner]
        , testCase "overlong operation batch refuses" $ pfails $ padvanceEvaluation # signerState # pconstant signerRaw # operations (replicate 17 $ pcon PSigner)
        , testCase "changed raw chunk refuses" $ pfails $ pauthenticatePayload # initial (hex "820280") (-1) (-1) # bytesList [hex "820180"]
        ]
    , testGroup
        "Mint-authorization validity boundaries"
        [ testCase (label <> show bound) $ passertEvalNoTrace $ result (hex script) start end expected
        | (label, script, isStart) <- [("after ", "82041864", True), ("before ", "82051864", False)]
        , bound <- [-1, 99, 100, 101]
        , let start = if isStart then bound else -1
              end = if isStart then -1 else bound
              expected = if bound >= 0 && (if isStart then bound >= 100 else bound <= 100) then 1 else 0
        ]
    ]

stagedTests :: TestTree
stagedTests =
  testGroup
    "Staged validator finalization"
    [ testCase "finalizes exact false evaluation" $ psucceeds $ runStage True $ terminalContext True evaluateTerminal False
    , testCase "refuses a satisfied policy" $ pfails $ runStage True $ terminalContext True (replaceField 14 (PD.I 1) evaluateTerminal) False
    , testCase "refuses an unfinished signer fold" $ pfails $ runStage True $ terminalContext True (replaceField 4 (PD.I 1) evaluateTerminal) False
    , testCase "refuses unconsumed script bytes" $ pfails $ runStage True $ terminalContext True (replaceField 10 (PD.I 2) evaluateTerminal) False
    , testCase "refuses an unclosed frame" $ pfails $ runStage True $ terminalContext True (replaceField 13 (PD.I 1) evaluateTerminal) False
    , testCase "refuses a nonempty stack commitment" $ pfails $ runStage True $ terminalContext True (replaceField 12 (PD.B "x") evaluateTerminal) False
    , testCase "refuses wrong evaluation successor" $ pfails $ runStage True $ terminalContext True evaluateTerminal True
    , testCase "finalizes exact witness absence" $ psucceeds $ runStage False $ terminalContext False witnessTerminal False
    , testCase "refuses witnesses left to scan" $ pfails $ runStage False $ terminalContext False (replaceField 7 (PD.I 1) witnessTerminal) False
    , testCase "refuses trailing witness bytes" $ pfails $ runStage False $ terminalContext False (replaceField 3 (PD.I 2) witnessTerminal) False
    , testCase "refuses wrong absence successor" $ pfails $ runStage False $ terminalContext False witnessTerminal True
    , testCase "reads canonical inline reference chunks" $ passertEvalNoTrace $ ppayload # (pchunks # indices [0] # pconstant [chunkRef "x"]) #== pconstant "x"
    , testCase "refuses negative chunk references" $ pfails $ ppayload # (pchunks # indices [-1] # pconstant [chunkRef "x"])
    , testCase "refuses missing chunk references" $ pfails $ ppayload # (pchunks # indices [1] # pconstant [chunkRef "x"])
    , testCase "refuses empty chunks" $ pfails $ ppayload # (pchunks # indices [0] # pconstant [chunkRef ""])
    , testCase "refuses oversized chunks" $ pfails $ ppayload # (pchunks # indices [0] # pconstant [chunkRef $ BS.replicate 15149 0])
    , testCase "refuses too many field chunks" $ pfails $ ppayload # (pchunks # indices [0, 0, 0, 0] # pconstant [chunkRef "x"])
    , testCase "raw payload permits five chunks" $ passertEvalNoTrace $ ppayload # (prawChunks # indices [0, 0, 0, 0, 0] # pconstant [chunkRef "x"]) #== pconstant "xxxxx"
    , testCase "raw payload refuses six chunks" $ pfails $ ppayload # (prawChunks # indices [0, 0, 0, 0, 0, 0] # pconstant [chunkRef "x"])
    ]

indices :: forall s. [Integer] -> Term s (PBuiltinList (PAsData PInteger))
indices = foldr (\i rest -> pcons # pdata (pconstant i) # rest) pnil

chunkRef :: BS.ByteString -> TxInInfo
chunkRef bytes = threadInput{txInInfoResolved = (txInInfoResolved threadInput){txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ PD.B bytes}}

replaceField :: Int -> PD.Data -> PD.Data -> PD.Data
replaceField at value (PD.Constr tag fields) = PD.Constr tag $ take at fields <> [value] <> drop (at + 1) fields
replaceField _ _ _ = error "expected constructor"

evaluateTerminal, witnessTerminal :: PD.Data
evaluateTerminal = PD.Constr 0 [PD.B policy, PD.I 3, PD.I 4, PD.I 4, PD.I 0, PD.I 0, PD.List [], PD.List [], PD.I (-1), PD.I (-1), PD.I 3, PD.I 1, PD.B "", PD.I 0, PD.I 0]
witnessTerminal = PD.Constr 0 [PD.B policy, PD.B "tx", PD.B "root", PD.I 1, PD.List [], PD.I 1, PD.I 0, PD.I 0]

policy :: BS.ByteString
policy = BS.replicate 28 0x42

terminalContext :: Bool -> PD.Data -> Bool -> ScriptContext
terminalContext evaluation state wrongHash =
  spendContext
    (stepDatum $ Just state)
    (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0]])
    [threadInput]
    [stepOutput (if wrongHash then stepScript else nextScript) $ Just $ if evaluation then PD.Constr 0 [PD.B policy, PD.I 1] else PD.Constr 0 [PD.B policy, PD.B "tx", PD.B "root", PD.I 0]]
    []
    []
    mempty

runStage :: forall s. Bool -> ScriptContext -> Term s PUnit
runStage evaluation context =
  (if evaluation then mintAuthorizationEvaluateValidator else mintAuthorizationWitnessScanValidator)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant context
