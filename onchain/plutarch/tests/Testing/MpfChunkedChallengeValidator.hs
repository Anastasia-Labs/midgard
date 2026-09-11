{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.MpfChunkedChallengeValidator
Description : Current-Aiken parity tests for the MPF proof challenge thread.
-}
module Testing.MpfChunkedChallengeValidator (tests) where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
    Address,
    Datum (..),
    OutputDatum (..),
    PubKeyHash (..),
    ScriptContext (..),
    ScriptHash (..),
    ScriptInfo (..),
    TxId (..),
    TxInInfo (..),
    TxOut (..),
    TxOutRef (..),
    toBuiltinData,
 )
import PlutusTx.Builtins (blake2b_256, dataToBuiltinData, fromBuiltin, serialiseData, toBuiltin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.MpfChunkedChallenge (mpfChunkedChallengeValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture (
    adaValue,
    commitCountedRoot,
    hubPolicy,
    l2Count,
    outRefN,
    ownRef,
    prover,
    referenceInputsWithTransactionsRoot,
    stepScript,
    transactionsDomain,
    unCS,
 )
import Testing.FraudProofsFixture qualified as Fixture

tests :: TestTree
tests =
    testGroup
        "MPF Chunked Challenge Validator"
        [ testGroup "initialization" initializationTests
        , testGroup "thread retirement" burnTests
        , testGroup "finalization" finalizationTests
        ]

initializationTests :: [TestTree]
initializationTests =
    [ testCase "initializes a header-bound thread" $
        psucceeds $
            run honestInitContext
    , testCase "rejects a foreign header hash" $
        pfails $
            run $
                initContext (honestChallenge{cHeaderHash = hash28 0xbb}) transactionsDomain nonceBytes 1 challengeAddress Nothing
    , testCase "rejects a root the header does not commit" $
        pfails $
            run $
                initContext (honestChallenge{cRoot = hash32 0x0b}) transactionsDomain nonceBytes 1 challengeAddress Nothing
    , testCase "rejects a leaf count the header does not commit" $
        pfails $
            run $
                initContext (honestChallenge{cLeafCount = l2Count + 1}) transactionsDomain nonceBytes 1 challengeAddress Nothing
    , testCase "rejects a substituted root domain" $
        pfails $
            run $
                initContext honestChallenge 3 nonceBytes 1 challengeAddress Nothing
    , testCase "rejects a datum domain not bound by the proof" $
        pfails $
            run $
                initContext (honestChallenge{cDomain = 3}) transactionsDomain nonceBytes 1 challengeAddress Nothing
    , testCase "rejects a malformed challenge datum" $
        pfails $
            run $
                initContext (honestChallenge{cTargetKey = hash28 0x44}) transactionsDomain nonceBytes 1 challengeAddress Nothing
    , testCase "rejects an asset name that is not the nonce" $
        pfails $
            run $
                initContext honestChallenge transactionsDomain forgedNonce 1 challengeAddress Nothing
    , testCase "rejects more than one minted unit" $
        pfails $
            run $
                initContext honestChallenge transactionsDomain nonceBytes 2 challengeAddress Nothing
    , testCase "rejects a thread output off its own address" $
        pfails $
            run $
                initContext honestChallenge transactionsDomain nonceBytes 1 foreignScriptAddress Nothing
    , testCase "rejects a thread output with a reference script" $
        pfails $
            run $
                initContext honestChallenge transactionsDomain nonceBytes 1 challengeAddress (Just $ ScriptHash $ toBuiltin stepScript)
    ]

burnTests :: [TestTree]
burnTests =
    [ testCase "burns the thread token with its own UTxO" $
        psucceeds $
            run $
                burnContext honestChallenge challengeAddress nonceBytes nonceBytes (-1)
    , testCase "rejects an input off the thread address" $
        pfails $
            run $
                burnContext honestChallenge foreignScriptAddress nonceBytes nonceBytes (-1)
    , testCase "rejects retiring a token the input does not hold" $
        pfails $
            run $
                burnContext honestChallenge challengeAddress nonceBytes forgedNonce (-1)
    , testCase "rejects a burn quantity other than minus one" $
        pfails $
            run $
                burnContext honestChallenge challengeAddress nonceBytes nonceBytes (-2)
    ]

finalizationTests :: [TestTree]
finalizationTests =
    [ testCase "finalizes a verified zero-step proof" $
        psucceeds $
            run $
                honestFinalizeContext honestChallenge
    , testCase "finalizes a proof carried by a published chunk" $
        psucceeds $
            run $
                finalizeContext absenceChallenge [] [rewardOutput prover challengeLovelace] (threadMint (-1)) [chunkReferenceInput] [0]
    , testCase "the same published chunk proves nothing about another root" $
        pfails $
            run $
                finalizeContext (absenceChallenge{cRoot = hash32 0x0b}) [] [rewardOutput prover challengeLovelace] (threadMint (-1)) [chunkReferenceInput] [0]
    , testCase "rejects an unburnt thread token" $
        pfails $
            run $
                finalizeContext honestChallenge [] [rewardOutput prover challengeLovelace] mempty [] []
    , testCase "rejects a re-minted thread token" $
        pfails $
            run $
                finalizeContext honestChallenge [] [rewardOutput prover challengeLovelace] (threadMint 1) [] []
    , testCase "rejects a reward paid elsewhere" $
        pfails $
            run $
                finalizeContext honestChallenge [] [rewardOutput stepScript challengeLovelace] (threadMint (-1)) [] []
    , testCase "rejects an underpaid reward" $
        pfails $
            run $
                finalizeContext honestChallenge [] [rewardOutput prover (challengeLovelace - 1)] (threadMint (-1)) [] []
    , testCase "rejects a second thread input" $
        pfails $
            run $
                finalizeContext honestChallenge [duplicateThreadInput] [rewardOutput prover challengeLovelace] (threadMint (-1)) [] []
    , testCase "rejects an unverified proof" $
        pfails $
            run $
                finalizeContext (honestChallenge{cTargetDigest = hash32 0x0d}) [] [rewardOutput prover challengeLovelace] (threadMint (-1)) [] []
    ]

run :: ScriptContext -> Term s PUnit
run ctx =
    mpfChunkedChallengeValidator
        # pdata (pconstant hubPolicy)
        # pconstant ctx

data Challenge = Challenge
    { cOwner :: BS.ByteString
    , cHeaderHash :: BS.ByteString
    , cDomain :: Integer
    , cTargetKey :: BS.ByteString
    , cTargetDigest :: BS.ByteString
    , cRoot :: BS.ByteString
    , cLeafCount :: Integer
    , cMode :: Integer
    }

honestChallenge :: Challenge
honestChallenge =
    Challenge
        { cOwner = prover
        , cHeaderHash = hash28 0xaa
        , cDomain = transactionsDomain
        , cTargetKey = targetKey
        , cTargetDigest = targetDigest
        , cRoot = singletonRoot
        , cLeafCount = l2Count
        , cMode = 0
        }

absenceChallenge :: Challenge
absenceChallenge =
    honestChallenge
        { cRoot = absenceRoot
        , cMode = 1
        }

challengeData :: Challenge -> PD.Data
challengeData challenge =
    PD.Constr
        0
        [ PD.B $ cOwner challenge
        , PD.B $ cHeaderHash challenge
        , PD.Constr (cDomain challenge) []
        , PD.B $ cTargetKey challenge
        , PD.B $ cTargetDigest challenge
        , PD.B $ cRoot challenge
        , PD.I $ cLeafCount challenge
        , PD.Constr (cMode challenge) []
        ]

challengePolicy :: CurrencySymbol
challengePolicy = CurrencySymbol $ toBuiltin $ hash28 0x88

challengeAddress, foreignScriptAddress :: Address
challengeAddress = scriptHashAddress $ ScriptHash $ toBuiltin $ unCS challengePolicy
foreignScriptAddress = scriptHashAddress $ ScriptHash $ toBuiltin stepScript

challengeLovelace :: Integer
challengeLovelace = 50_000_000

nonceBytes :: BS.ByteString
nonceBytes = fromBuiltin $ blake2b_256 $ serialiseData $ toBuiltinData ownRef

forgedNonce :: BS.ByteString
forgedNonce = hash32 0xf0

threadMint :: Integer -> Value
threadMint = singleton challengePolicy (TokenName $ toBuiltin nonceBytes)

threadValue :: BS.ByteString -> Value
threadValue assetName =
    adaValue challengeLovelace
        <> singleton challengePolicy (TokenName $ toBuiltin assetName) 1

nonceInput :: TxInInfo
nonceInput =
    TxInInfo
        ownRef
        (TxOut (pubKeyHashAddress $ PubKeyHash $ toBuiltin prover) (adaValue 100_000_000) NoOutputDatum Nothing)

challengeOutput :: Challenge -> BS.ByteString -> Address -> Maybe ScriptHash -> TxOut
challengeOutput challenge assetName address referenceScript =
    TxOut
        address
        (threadValue assetName)
        (OutputDatum $ Datum $ dataToBuiltinData $ challengeData challenge)
        referenceScript

initContext :: Challenge -> Integer -> BS.ByteString -> Integer -> Address -> Maybe ScriptHash -> ScriptContext
initContext challenge redeemerDomain assetName quantity address referenceScript =
    asChallengeMint $
        Fixture.spendContext
            (PD.Constr 0 [])
            (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.Constr redeemerDomain []])
            [nonceInput]
            [challengeOutput challenge assetName address referenceScript]
            ( referenceInputsWithTransactionsRoot $
                commitCountedRoot transactionsDomain singletonRoot l2Count
            )
            []
            (singleton challengePolicy (TokenName $ toBuiltin assetName) quantity)

honestInitContext :: ScriptContext
honestInitContext =
    initContext honestChallenge transactionsDomain nonceBytes 1 challengeAddress Nothing

asChallengeMint :: ScriptContext -> ScriptContext
asChallengeMint (ScriptContext txInfo redeemer _) =
    ScriptContext txInfo redeemer (MintingScript challengePolicy)

challengeInput :: Challenge -> Address -> BS.ByteString -> TxOutRef -> TxInInfo
challengeInput challenge address assetName outRef =
    TxInInfo
        outRef
        (challengeOutput challenge assetName address Nothing)

burnContext :: Challenge -> Address -> BS.ByteString -> BS.ByteString -> Integer -> ScriptContext
burnContext challenge address heldName burntName quantity =
    asChallengeMint $
        Fixture.spendContext
            (PD.Constr 0 [])
            (PD.Constr 1 [PD.I 0])
            [challengeInput challenge address heldName ownRef]
            []
            []
            []
            (singleton challengePolicy (TokenName $ toBuiltin burntName) quantity)

finalizeContext ::
    Challenge ->
    [TxInInfo] ->
    [TxOut] ->
    Value ->
    [TxInInfo] ->
    [Integer] ->
    ScriptContext
finalizeContext challenge extraInputs outputs mint referenceInputs indices =
    Fixture.spendContext
        (challengeData challenge)
        (PD.Constr 0 [PD.List $ map PD.I indices])
        (challengeInput challenge challengeAddress nonceBytes ownRef : extraInputs)
        outputs
        referenceInputs
        []
        mint

honestFinalizeContext :: Challenge -> ScriptContext
honestFinalizeContext challenge =
    finalizeContext challenge [] [rewardOutput prover challengeLovelace] (threadMint (-1)) [] []

rewardOutput :: BS.ByteString -> Integer -> TxOut
rewardOutput owner lovelace =
    TxOut
        (pubKeyHashAddress $ PubKeyHash $ toBuiltin owner)
        (adaValue lovelace)
        NoOutputDatum
        Nothing

duplicateThreadInput :: TxInInfo
duplicateThreadInput =
    challengeInput
        honestChallenge
        challengeAddress
        nonceBytes
        (TxOutRef (TxId $ toBuiltin $ hash32 0xbe) 1)

chunkReferenceInput :: TxInInfo
chunkReferenceInput =
    TxInInfo
        (outRefN 7)
        ( TxOut
            foreignScriptAddress
            (adaValue 2_000_000)
            (OutputDatum $ Datum $ dataToBuiltinData chunkData)
            Nothing
        )
  where
    chunkData = PD.Constr 0 [PD.List [PD.Constr 2 [PD.I 0, PD.B absentPath, PD.B absentValueHash]]]

targetKey, targetValue, targetDigest, singletonRoot :: BS.ByteString
targetKey = hash32 0x44
targetValue = "challenge-target"
targetDigest = blake2b256 targetValue
singletonRoot = blake2b256 $ BS.cons 0xff (blake2b256 targetKey <> targetDigest)

absentKey, absentPath, absentValueHash, absenceRoot :: BS.ByteString
absentKey = hash32 0x55
absentPath = blake2b256 absentKey
absentValueHash = hash32 0x66
absenceRoot = blake2b256 $ BS.cons 0xff (absentPath <> absentValueHash)

hash28, hash32 :: Word8 -> BS.ByteString
hash28 byte = BS.replicate 28 byte
hash32 byte = BS.replicate 32 byte

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . blake2b_256 . toBuiltin
