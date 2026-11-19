{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMintAuthorizationStep02 (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.Validators.FraudProofs.MintAuthorization (mintAuthorizationStep02Validator)
import Plutarch.Prelude
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Mint-authorization step 02"
    [ testCase "reads the committed policy in direction A" $ psucceeds $ run $ validContext 0
    , testCase "reads the committed policy in direction B" $ psucceeds $ run $ validContext 1
    , testCase "rejects an out-of-domain direction" $ pfails $ run $ validContext 2
    , testCase "rejects a substituted mint preimage" $ pfails $ run substitutedContext
    , testCase "rejects an empty mint" $ pfails $ run emptyMintContext
    , testCase "rejects a zero-quantity entry" $ pfails $ run zeroQuantityContext
    , testCase "rejects a foreign header" $ pfails $ run foreignHeaderContext
    ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx =
  mintAuthorizationStep02Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

policyA, policyB, witnessHash, priorRoot, foreignPriorRoot, postRoot, categoryId :: BS.ByteString
policyA = BS.replicate 28 0x42
policyB = BS.replicate 28 0x59
witnessHash = hash32 0x77
priorRoot = hash32 0x55
foreignPriorRoot = hash32 0x66
postRoot = hash32 0x88
categoryId = BS.replicate 4 0xa1

mintItem :: BS.ByteString -> Integer -> BS.ByteString
mintItem policy quantity = "\x82\x58\x1c" <> policy <> "\xa1\x45TOKEN" <> cborInt quantity

mintPreimage :: BS.ByteString -> Integer -> BS.ByteString
mintPreimage policy quantity = arrayHeader 1 <> wrapItem (mintItem policy quantity)

compactBody :: BS.ByteString -> BS.ByteString
compactBody preimage =
  BS.concat
    [ "\x8c"
    , defBytes32 $ hash32 0x10
    , defBytes32 $ hash32 0x11
    , defBytes32 $ hash32 0x12
    , cborInt 7_000_000
    , cborInt 0
    , cborInt 65_536
    , defBytes32 $ hash32 0x13
    , defBytes32 $ hash32 0x14
    , defBytes32 $ blake2b256 preimage
    , defBytes32 $ hash32 0x16
    , defBytes32 $ hash32 0x17
    , cborInt 1
    ]

compactAndId :: BS.ByteString -> (BS.ByteString, BS.ByteString)
compactAndId preimage = (compact, txId)
  where
    body = compactBody preimage
    compact = "\x84\x01" <> body <> defBytes32 witnessHash <> "\x00"
    txId = blake2b256 $ "MidgardNativeTxBodyV1\x01" <> body

claim :: BS.ByteString -> BS.ByteString -> (PD.Data, PD.Data, PD.Data)
claim txId preRoot = (header, eventMembership, stepMembership)
  where
    eventKey = PD.Constr 2 [PD.B txId]
    phase = PD.Constr 2 []
    step = PD.Constr 0 [PD.I 1, PD.I 0, eventKey, phase, PD.B preRoot, PD.B postRoot]
    eventValue = PD.Constr 0 [PD.I 0, phase]
    stepPhas = singleEntryPhasRoot (serialise $ PD.I 0) (serialise step)
    eventPhas = singleEntryPhasRoot (serialise eventKey) (serialise eventValue)
    stepRoot = commitCountedRoot 4 stepPhas 1
    eventRoot = commitCountedRoot 5 eventPhas 1
    header =
      PD.Constr
        0
        [ PD.B $ hash32 0x01
        , PD.B $ hash32 0x02
        , PD.B $ hash32 0x03
        , PD.B $ hash32 0x04
        , PD.B $ hash32 0x05
        , PD.B $ hash32 0x06
        , PD.B stepRoot
        , PD.B eventRoot
        , PD.B $ hash32 0x09
        , PD.I 0
        , PD.I 0
        , PD.I 1
        , PD.I 0
        , PD.I 1
        , PD.I 1
        , PD.I 0
        , PD.I 100
        , PD.I 200
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ BS.replicate 28 0x02
        , PD.B prover
        , PD.I 1
        ]
    eventMembership = membershipProof 5 eventRoot eventPhas 1 eventKey eventValue
    stepMembership = membershipProof 4 stepRoot stepPhas 1 (PD.I 0) step

assetNameOf :: PD.Data -> BS.ByteString
assetNameOf header = categoryId <> blake2b224 (serialise header)

step02Context :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString -> PD.Data -> PD.Data -> PD.Data -> BS.ByteString -> ScriptContext
step02Context compact txId direction openedPreimage header eventProof stepProof assetName =
  spendContext
    (stepDatum $ Just $ PD.Constr 0 [PD.B txId, PD.B witnessHash, PD.I 0, PD.I 65_536])
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , header
            , eventProof
            , stepProof
            , PD.I 0
            , PD.I direction
            , bodyOpening compact openedPreimage
            ]
        ]
    )
    [threadInputWithName assetName]
    [ stepOutputWithName
        nextScript
        ( Just $
            PD.Constr
              0
              [ PD.B policyA
              , PD.I direction
              , PD.B txId
              , PD.B witnessHash
              , PD.I 0
              , PD.I 65_536
              , PD.B priorRoot
              ]
        )
        assetName
    ]
    []
    []
    mempty

contextFor :: BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString -> ScriptContext
contextFor committedPreimage direction openedPreimage claimedPolicy =
  step02Context compact txId direction openedPreimage header eventProof stepProof (assetNameOf header)
  where
    {-
    The expected state always names policyA in the common constructor. Tests
    that need another expected policy fail at the opening before that equality.
    -}

    (compact, txId) = compactAndId committedPreimage
    (header, eventProof, stepProof) = claim txId priorRoot
    _ = claimedPolicy

validContext :: Integer -> ScriptContext
validContext direction = contextFor honestPreimage direction honestPreimage policyA

honestPreimage, foreignPreimage, emptyPreimage, zeroPreimage :: BS.ByteString
honestPreimage = mintPreimage policyA 5
foreignPreimage = mintPreimage policyB 5
emptyPreimage = "\x80"
zeroPreimage = mintPreimage policyA 0

substitutedContext, emptyMintContext, zeroQuantityContext, foreignHeaderContext :: ScriptContext
substitutedContext = contextFor honestPreimage 0 foreignPreimage policyB
emptyMintContext = contextFor emptyPreimage 0 emptyPreimage policyA
zeroQuantityContext = contextFor zeroPreimage 0 zeroPreimage policyA
foreignHeaderContext = step02Context compact txId 0 honestPreimage foreignHeader foreignEvent foreignStep (assetNameOf honestHeader)
  where
    (compact, txId) = compactAndId honestPreimage
    (honestHeader, _, _) = claim txId priorRoot
    (foreignHeader, foreignEvent, foreignStep) = claim txId foreignPriorRoot
