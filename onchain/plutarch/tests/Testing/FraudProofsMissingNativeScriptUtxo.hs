{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMissingNativeScriptUtxo (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (
    Credential (..),
    Datum (..),
    OutputDatum (..),
    Redeemer (..),
    ScriptContext,
    ScriptHash (..),
    ScriptPurpose (..),
    TokenName (..),
    TxId (..),
    TxInInfo (..),
    TxOut (..),
    TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import MerkleTree.Validators.Membership (membershipStakeValidator)
import Midgard.FraudProofs.ChunkedInclusion (
    PPublishedProofCarriage,
    ppublishedChunkMembership,
 )
import Midgard.Validators.FraudProofs.MissingNativeScriptUtxo
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
    testGroup
        "Missing Native Script UTxO Fraud Proof Tests"
        [ testGroup "step 03" step03Tests
        , testGroup "step 04" step04Tests
        , testCase "step 05 finalizes an absent script" $ psucceeds $ directFinalize False
        , testCase "step 05 rejects a present script" $ pfails $ directFinalize True
        , testGroup "staged steps 05-07" stagedTests
        ]

step03Tests :: [TestTree]
step03Tests =
    [ testCase "missing_native_script_utxo_step_03_authenticates_the_prior_script_output" $
        psucceeds $
            runRedeemerCarriedMembership nativeDescriptor
    , testCase "missing_native_script_utxo_step_03_rejects_a_forged_descriptor" $
        pfails $
            runRedeemerCarriedMembership "\x80"
    , testCase "missing_native_script_utxo_step_03_authenticates_a_maximum_64_step_published_membership" $
        psucceeds $
            runPublishedMembership [2, 3, 4, 5]
    , testCase "missing_native_script_utxo_step_03_rejects_reordered_published_chunks" $
        pfails $
            runPublishedMembership [2, 4, 3, 5]
    ]

step04Tests :: [TestTree]
step04Tests =
    [ testCase "missing_native_script_utxo_step_04_authenticates_the_script_credential_preimage" $
        psucceeds $
            runDescriptor nativeDescriptor nativeScriptBytes
    , testCase "missing_native_script_utxo_step_04_rejects_a_key_locked_output" $
        pfails $
            runDescriptor keyLockedDescriptor nativeScriptBytes
    , testCase "missing_native_script_utxo_step_04_rejects_the_wrong_native_preimage" $
        pfails $
            runDescriptor nativeDescriptor otherNativeScriptBytes
    , testCase "missing_native_script_utxo_step_04_rejects_a_descriptor_index_mismatch" $
        pfails $
            runDescriptor mismatchedIndexDescriptor nativeScriptBytes
    ]

runRedeemerCarriedMembership :: forall s. BS.ByteString -> Term s PUnit
runRedeemerCarriedMembership descriptor =
    plet (runStep03 honestRoot descriptor redeemerCarriedMembership [] [phasEntry honestRoot ledgerKey nativeDescriptor]) $ \_ ->
        membershipStakeValidator # pconstant (proofContext membershipRedeemer)
  where
    honestRoot = singleEntryPhasRoot ledgerKey nativeDescriptor
    membershipRedeemer = PD.List [PD.B honestRoot, PD.B ledgerKey, PD.B nativeDescriptor, emptyProof]

runPublishedMembership :: forall s. [Integer] -> Term s PUnit
runPublishedMembership indices = pif directCheck validatorCheck perror
  where
    steps = adversarialBranchSteps 64
    root = membershipRootOf ledgerKey nativeDescriptor steps
    chunks = proofChunkReferenceInputs 2 steps
    refs = referenceInputs <> chunks
    carriage = publishedMembershipCarriage indices
    claim = chunkClaim root ledgerKey (blake2b256 nativeDescriptor) indices
    directCheck =
        ppublishedChunkMembership
            # pconstant refs
            # publishedProofCarriageT indices
            # pconstant root
            # pconstant ledgerKey
            # pconstant nativeDescriptor
    validatorCheck = runStep03 root nativeDescriptor carriage refs [chunkEntry claim]

runStep03 ::
    forall s.
    BS.ByteString ->
    BS.ByteString ->
    PD.Data ->
    [TxInInfo] ->
    [(ScriptPurpose, Redeemer)] ->
    Term s PUnit
runStep03 root descriptor carriage refs withdrawals =
    missingNativeScriptUtxoStep03Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pconstant
            ( spendContext
                (stepDatum $ Just $ step03State root)
                (continueAction $ step03Args descriptor carriage)
                [threadInput]
                [stepOutput nextScript $ Just $ step04State descriptor]
                refs
                withdrawals
                mempty
            )

runDescriptor :: forall s. BS.ByteString -> BS.ByteString -> Term s PUnit
runDescriptor descriptor suppliedScript =
    missingNativeScriptUtxoStep04Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pconstant
            ( spendContext
                (stepDatum $ Just $ step04State descriptor)
                (continueAction $ PD.Constr 0 [PD.I 0, PD.I 0, PD.B suppliedScript])
                [threadInput]
                [stepOutput nextScript $ Just expectedState]
                []
                []
                mempty
            )
  where
    expectedState = PD.Constr 0 [PD.B lockedScriptHash, PD.B badTxId, PD.B badWitnessSetHash, readyPhase]

step03State :: BS.ByteString -> PD.Data
step03State root =
    PD.Constr
        0
        [ PD.Constr 0 [PD.B outRefTxId, PD.I 0]
        , PD.B badTxId
        , PD.B badWitnessSetHash
        , PD.B root
        ]

step03Args :: BS.ByteString -> PD.Data -> PD.Data
step03Args descriptor carriage =
    PD.Constr 0 [PD.I 0, PD.I 0, outRefData, PD.B descriptor, carriage]

step04State :: BS.ByteString -> PD.Data
step04State descriptor =
    PD.Constr 0 [outRefData, PD.B descriptor, PD.B badTxId, PD.B badWitnessSetHash]

outRefData :: PD.Data
outRefData = PD.Constr 0 [PD.B outRefTxId, PD.I 0]

outRefTxId, badTxId, badWitnessSetHash :: BS.ByteString
outRefTxId = BS.replicate 32 0xcc
badTxId = txIdOf txScriptSpend
badWitnessSetHash = witnessSetHashOf txScriptSpend

ledgerKey :: BS.ByteString
ledgerKey = "\x82" <> defBytes32 outRefTxId <> "\x19\x00\x00"

redeemerCarriedMembership :: PD.Data
redeemerCarriedMembership = PD.Constr 0 [emptyProof, PD.I 0]

nativeDescriptor :: BS.ByteString
nativeDescriptor =
    Base16.decodeLenient
        "90010018295820dda860a40cf826708c54bf3f022e2757f78dc17dcc02b2dbc43a33a8ca666147581d700861a5328e50b6e9c2c2836d267da7781b5a6022864c108f4db451b31a001e8480005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358203099fcf80bce873fc04c7e7ba02f8b1edd1cb8fac5b6ab62ecb935b44ef8f9e4183c18518358203099fcf80bce873fc04c7e7ba02f8b1edd1cb8fac5b6ab62ecb935b44ef8f9e4183c18518358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304"

keyLockedDescriptor :: BS.ByteString
keyLockedDescriptor = BS.take 41 nativeDescriptor <> "\x60" <> BS.drop 42 nativeDescriptor

mismatchedIndexDescriptor :: BS.ByteString
mismatchedIndexDescriptor = BS.take 2 nativeDescriptor <> "\x01" <> BS.drop 3 nativeDescriptor

proofContext :: PD.Data -> ScriptContext
proofContext proofRedeemer =
    asRewarding $
        spendContext
            (stepDatum Nothing)
            proofRedeemer
            []
            []
            []
            []
            mempty

stagedTests :: [TestTree]
stagedTests =
    [ testCase "q33_staged_step_05_starts_only_above_the_direct_limit" $ psucceeds $ startGrammar 65 nextScript
    , testCase "q33_staged_step_05_rejects_a_direct_sized_field" $ pfails $ startGrammar 64 nextScript
    , testCase "q33_staged_step_05_rejects_the_wrong_successor" $ pfails $ startGrammar 65 stepScript
    , testCase "q33_staged_step_05_exact_maximum_fit" $ psucceeds $ startGrammar 224 nextScript
    , testCase "q33_staged_step_06_exact_maximum_grammar_resume_fit" $ psucceeds $ resumeGrammarAt 224 32 False False
    , testCase "q33_staged_step_06_resumes_grammar_only_at_step_06" $ psucceeds $ resumeGrammarAt 65 32 False False
    , testCase "q33_staged_step_06_rejects_ready_phase_skip" $ pfails $ resumeGrammarAt 65 32 True False
    , testCase "q33_staged_step_06_rejects_wrong_successor_during_grammar" $ pfails $ resumeGrammarAt 65 32 False True
    , testCase "q33_staged_step_06_terminal_grammar_starts_semantic_scan" $ psucceeds $ startSemanticScan 65 True False
    , testCase "q33_staged_step_06_rejects_semantic_start_before_grammar_terminal" $ pfails $ startSemanticScan 65 False False
    , testCase "q33_staged_step_06_rejects_wrong_semantic_successor" $ pfails $ startSemanticScan 65 True True
    , testCase "q33_staged_step_06_exact_maximum_semantic_start_fit" $ psucceeds $ startSemanticScan 224 True False
    , testCase "q33_staged_step_07_resumes_only_semantic_state_at_step_07" $ psucceeds $ resumeSemanticScan 65 False False
    , testCase "q33_staged_step_07_rejects_grammar_phase_skip" $ pfails $ resumeSemanticScan 65 True False
    , testCase "q33_staged_step_07_rejects_wrong_successor" $ pfails $ resumeSemanticScan 65 False True
    , testCase "q33_staged_step_07_exact_maximum_resume_fit" $ psucceeds $ resumeSemanticScan 224 False False
    , testCase "q33_staged_step_07_finalizes_only_at_semantic_terminal" $ psucceeds $ finalizeSemanticScan 65 64 False
    , testCase "q33_staged_step_07_rejects_a_valid_block_accumulator" $ pfails $ finalizeSemanticScan 65 64 True
    , testCase "q33_staged_step_07_exact_maximum_terminal_fit" $ psucceeds $ finalizeSemanticScan 224 192 False
    , testCase "q33_staged_step_05_preserves_prover_cancellation" $ psucceeds $ cancelStep05 65
    , testCase "q33_staged_step_06_preserves_prover_cancellation" $ psucceeds $ cancelStep06 65
    , testCase "q33_staged_step_07_preserves_prover_cancellation" $ psucceeds $ cancelStep07 65
    ]

directFinalize :: forall s. Bool -> Term s PUnit
directFinalize present =
    missingNativeScriptUtxoStep05Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( spendContext
                (stepDatum $ Just $ stateOf tx readyPhase)
                (continueAction $ PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, openingOf tx])
                [threadInput]
                [convictionOutput fraudProofAddress threadName]
                referenceInputs
                [fraudProofMintEntry threadName]
                (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)
            )
  where
    tx = if present then txScriptSpend{tScripts = [(0, nativeScriptBytes)]} else txScriptSpend

startGrammar :: forall s. Int -> BS.ByteString -> Term s PUnit
startGrammar count outputScript =
    missingNativeScriptUtxoStep05Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (state count readyPhase)
                (PD.Constr 1 [PD.I 0, PD.I 0, opening count, PD.I 32])
                outputScript
                (state count $ grammarPhase count 32)
            )

resumeGrammarAt :: forall s. Int -> Int -> Bool -> Bool -> Term s PUnit
resumeGrammarAt count at ready wrongSuccessor =
    missingNativeScriptUtxoStep06Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (state count inputPhase)
                (PD.Constr 0 [PD.I 0, PD.I 0, opening count, PD.B (grammarWire count at), PD.I 32])
                outputScript
                (state count $ grammarPhase count $ at + 32)
            )
  where
    inputPhase = if ready then readyPhase else grammarPhase count at
    outputScript = if wrongSuccessor then nextScript else stepScript

startSemanticScan :: forall s. Int -> Bool -> Bool -> Term s PUnit
startSemanticScan count terminal wrongSuccessor =
    missingNativeScriptUtxoStep06Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (state count $ grammarPhase count committedIndex)
                (PD.Constr 1 [PD.I 0, PD.I 0, opening count, PD.B (grammarWire count committedIndex), PD.I 32])
                outputScript
                (state count $ semanticPhase count 32 False)
            )
  where
    committedIndex = if terminal then count else 32
    outputScript = if wrongSuccessor then stepScript else nextScript

resumeSemanticScan :: forall s. Int -> Bool -> Bool -> Term s PUnit
resumeSemanticScan count grammar wrongSuccessor =
    missingNativeScriptUtxoStep07Validator
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (state count inputPhase)
                (PD.Constr 0 [PD.I 0, PD.I 0, opening count, PD.B (semanticWire count 32), PD.I 32])
                outputScript
                (state count $ semanticPhase count 64 False)
            )
  where
    inputPhase = if grammar then PD.Constr 1 [PD.B $ BS.replicate 32 0] else semanticPhase count 32 False
    outputScript = if wrongSuccessor then nextScript else stepScript

finalizeSemanticScan :: forall s. Int -> Int -> Bool -> Term s PUnit
finalizeSemanticScan count alreadyScanned found =
    missingNativeScriptUtxoStep07Validator
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( spendContext
                (stepDatum $ Just $ state count $ semanticPhase count alreadyScanned found)
                ( continueAction $
                    PD.Constr
                        1
                        [ PD.I 0
                        , PD.I 0
                        , PD.I 0
                        , opening count
                        , PD.B (semanticWire count alreadyScanned)
                        , PD.I (fromIntegral $ count - alreadyScanned)
                        ]
                )
                [threadInput]
                [convictionOutput fraudProofAddress threadName]
                referenceInputs
                [fraudProofMintEntry threadName]
                (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)
            )

cancelStep05, cancelStep06, cancelStep07 :: forall s. Int -> Term s PUnit
cancelStep05 count =
    missingNativeScriptUtxoStep05Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant (cancelContext count)
cancelStep06 count =
    missingNativeScriptUtxoStep06Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant (cancelContext count)
cancelStep07 count =
    missingNativeScriptUtxoStep07Validator
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant (cancelContext count)

continueContext :: PD.Data -> PD.Data -> BS.ByteString -> PD.Data -> ScriptContext
continueContext inputState action outputScript outputState =
    spendContext
        (stepDatum $ Just inputState)
        (continueAction action)
        [threadInput]
        [stepOutput outputScript $ Just outputState]
        referenceInputs
        []
        mempty

cancelContext :: Int -> ScriptContext
cancelContext count =
    spendContext
        (stepDatum $ Just $ state count readyPhase)
        cancelRedeemer
        [threadInput]
        []
        []
        [cancelMintEntry threadName]
        mempty

stagedTx :: Int -> Tx
stagedTx count = txScriptSpend{tScripts = [(0, scriptBytes index) | index <- [1 .. count]]}

scriptBytes :: Int -> BS.ByteString
scriptBytes index =
    BS.concat
        [ "\x82\x01\x82\x82\x00\x58\x1c"
        , prover
        , "\x82\x05\x19"
        , bigEndian 2 (20_000 + fromIntegral index)
        ]

openingOf :: Tx -> PD.Data
openingOf tx = witnessOpeningRaw (compactOf tx) (witnessSetHashesOf tx) (scriptWitnessesPreimage tx)

opening :: Int -> PD.Data
opening = openingOf . stagedTx

stateOf :: Tx -> PD.Data -> PD.Data
stateOf tx phase = PD.Constr 0 [PD.B lockedScriptHash, PD.B (txIdOf tx), PD.B (witnessSetHashOf tx), phase]

state :: Int -> PD.Data -> PD.Data
state count = stateOf (stagedTx count)

readyPhase :: PD.Data
readyPhase = PD.Constr 0 []

grammarPhase :: Int -> Int -> PD.Data
grammarPhase count nextIndex = PD.Constr 1 [PD.B $ checkpointHash grammarDomain $ grammarWire count nextIndex]

semanticPhase :: Int -> Int -> Bool -> PD.Data
semanticPhase count nextIndex found =
    PD.Constr 2 [PD.B $ checkpointHash semanticDomain $ semanticWire count nextIndex, PD.Constr (if found then 1 else 0) []]

grammarWire :: Int -> Int -> BS.ByteString
grammarWire count nextIndex =
    BS.concat
        [ "\x87\x58\x20"
        , txIdOf tx
        , "\x41\x06\x58\x20"
        , blake2b256 preimage
        , scalar totalLength
        , scalar $ fromIntegral count
        , scalar $ fromIntegral nextIndex
        , scalar $ 2 + 46 * fromIntegral nextIndex
        ]
  where
    tx = stagedTx count
    preimage = scriptWitnessesPreimage tx
    totalLength = fromIntegral $ BS.length preimage

semanticWire :: Int -> Int -> BS.ByteString
semanticWire count nextIndex =
    BS.concat
        [ "\x86\x58\x20"
        , txIdOf tx
        , "\x41\x06"
        , scalar totalLength
        , scalar $ fromIntegral count
        , scalar $ fromIntegral nextIndex
        , scalar $ 2 + 46 * fromIntegral nextIndex
        ]
  where
    tx = stagedTx count
    totalLength = fromIntegral $ BS.length $ scriptWitnessesPreimage tx

scalar :: Integer -> BS.ByteString
scalar value = "\x43" <> bigEndian 3 value

bigEndian :: Int -> Integer -> BS.ByteString
bigEndian width value =
    BS.pack [fromIntegral (value `div` (256 ^ exponent) `mod` 256) | exponent <- [width - 1, width - 2 .. 0]]

grammarDomain, semanticDomain :: BS.ByteString
grammarDomain = "MidgardFieldGrammarCheckpointV1"
semanticDomain = "MidgardFieldWalkCheckpointV1"

checkpointHash :: BS.ByteString -> BS.ByteString -> BS.ByteString
checkpointHash domain wire = blake2b256 (domain <> wire)

continueAction :: PD.Data -> PD.Data
continueAction action = PD.Constr 1 [action]

publishedProofCarriageData :: [Integer] -> PD.Data
publishedProofCarriageData indices = PD.Constr 0 [PD.List $ map PD.I indices]

publishedProofCarriageT :: forall s. [Integer] -> Term s PPublishedProofCarriage
publishedProofCarriageT indices =
    pfromData $ punsafeCoerce $ pconstant @PData $ publishedProofCarriageData indices

publishedMembershipCarriage :: [Integer] -> PD.Data
publishedMembershipCarriage indices = PD.Constr 1 [publishedProofCarriageData indices]

chunkClaim :: BS.ByteString -> BS.ByteString -> BS.ByteString -> [Integer] -> PD.Data
chunkClaim root key valueHash indices =
    PD.Constr
        0
        [ PD.Constr 0 []
        , PD.B root
        , PD.B key
        , PD.B valueHash
        , PD.List $ map PD.I indices
        ]

chunkEntry :: PD.Data -> (ScriptPurpose, Redeemer)
chunkEntry claim =
    ( Rewarding $ ScriptCredential $ ScriptHash $ toBuiltin chunkedVerifyHash
    , Redeemer $ dataToBuiltinData claim
    )

chunkedVerifyHash :: BS.ByteString
chunkedVerifyHash = Base16.decodeLenient "ea8d998a1396392158fa85afb0d202df7bd6d6ede7d3fbc05f55acd6"

data ProofStepRef = BranchStep Integer BS.ByteString

stepData :: ProofStepRef -> PD.Data
stepData (BranchStep skip neighbors) = PD.Constr 0 [PD.I skip, PD.B neighbors]

adversarialBranchSteps :: Int -> [ProofStepRef]
adversarialBranchSteps count =
    [BranchStep 0 $ branchNeighbors seed | seed <- [count - 1, count - 2 .. 0]]

branchNeighbors :: Int -> BS.ByteString
branchNeighbors seed = a <> b <> c <> d
  where
    a = blake2b256 $ cborInt $ fromIntegral seed
    b = blake2b256 a
    c = blake2b256 b
    d = blake2b256 c

membershipRootOf :: BS.ByteString -> BS.ByteString -> [ProofStepRef] -> BS.ByteString
membershipRootOf key value = foldMembershipRoot (blake2b256 key) (blake2b256 value) 0

foldMembershipRoot :: BS.ByteString -> BS.ByteString -> Int -> [ProofStepRef] -> BS.ByteString
foldMembershipRoot path valueHash cursor [] = combine (suffix path cursor) valueHash
foldMembershipRoot path valueHash cursor (BranchStep skip neighbors : rest) =
    branchRoot path cursor skip neighbors $ foldMembershipRoot path valueHash nextCursor rest
  where
    nextCursor = cursor + 1 + fromIntegral skip

branchRoot :: BS.ByteString -> Int -> Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
branchRoot path cursor skip neighbors childRoot =
    combine
        (nibbles path cursor $ nextCursor - 1)
        ( merkle16
            (nibble path $ nextCursor - 1)
            childRoot
            (BS.take 32 neighbors)
            (BS.take 32 $ BS.drop 32 neighbors)
            (BS.take 32 $ BS.drop 64 neighbors)
            (BS.take 32 $ BS.drop 96 neighbors)
        )
  where
    nextCursor = cursor + 1 + fromIntegral skip

combine :: BS.ByteString -> BS.ByteString -> BS.ByteString
combine left right = blake2b256 $ left <> right

nibble :: BS.ByteString -> Int -> Int
nibble path index
    | even index = fromIntegral (BS.index path $ index `div` 2) `shiftR` 4
    | otherwise = fromIntegral (BS.index path $ index `div` 2) .&. 0x0f

nibbles :: BS.ByteString -> Int -> Int -> BS.ByteString
nibbles path start end = BS.pack [fromIntegral $ nibble path index | index <- [start .. end - 1]]

suffix :: BS.ByteString -> Int -> BS.ByteString
suffix path cursor
    | even cursor = BS.cons 0xff $ BS.drop (cursor `div` 2) path
    | otherwise = BS.pack [0, fromIntegral $ nibble path cursor] <> BS.drop ((cursor + 1) `div` 2) path

merkle16 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle16 branch root n8 n4 n2 n1
    | branch <= 7 = combine (merkle8 branch root n4 n2 n1) n8
    | otherwise = combine n8 (merkle8 (branch - 8) root n4 n2 n1)

merkle8 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle8 branch root n4 n2 n1
    | branch <= 3 = combine (merkle4 branch root n2 n1) n4
    | otherwise = combine n4 (merkle4 (branch - 4) root n2 n1)

merkle4 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle4 branch root n2 n1
    | branch <= 1 = combine (merkle2 branch root n1) n2
    | otherwise = combine n2 (merkle2 (branch - 2) root n1)

merkle2 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle2 branch root n1
    | branch == 0 = combine root n1
    | otherwise = combine n1 root

proofChunkReferenceInputs :: Integer -> [ProofStepRef] -> [TxInInfo]
proofChunkReferenceInputs offset steps =
    [chunkRefInput (offset + fromIntegral index) chunk | (index, chunk) <- zip [0 :: Int ..] $ chunksOf 16 steps]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size items = take size items : chunksOf size (drop size items)

chunkRefInput :: Integer -> [ProofStepRef] -> TxInInfo
chunkRefInput index steps =
    TxInInfo
        (TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x11) index)
        ( TxOut
            (scriptHashAddress $ ScriptHash $ toBuiltin $ BS.replicate 28 0x90)
            mempty
            (OutputDatum $ Datum $ dataToBuiltinData $ PD.Constr 0 [PD.List $ map stepData steps])
            Nothing
        )
