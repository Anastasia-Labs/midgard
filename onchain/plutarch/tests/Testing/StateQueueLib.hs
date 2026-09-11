{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.StateQueueLib
Description : Tests for @lib/midgard/state-queue.ak@.

Four things here carry real conditions. @decode_header_view@ is a
protocol-version gate — it is what stops a genesis sentinel or a future version
being read as a v1 block. @commit_bound_header_time_is_valid@ ties a block's
event interval to the transaction committing it. The readers enforce the
root/node split, which matters because the two payloads are different types at
the same field positions. And @validate_da_attestation_attachment@ pins
everything about a block except the one field it is allowed to write.
-}
module Testing.StateQueueLib (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value (..), singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
  ScriptHash (..),
  ToData,
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (blake2b_224, builtinDataToData, dataToBuiltinData, fromBuiltin, serialiseData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PMintValue, PPosixTime, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.LedgerState (PConfirmedState, PHeaderV1)
import Midgard.StateQueue (
  PStateQueueNode,
  pcommitBoundHeaderTimeIsValid,
  pdecodeHeaderView,
  pgetBlockDatumV1,
  pgetConfirmedState,
  pgetPrevHeaderHashOfNodeV1,
  pgetStateQueueNode,
  pvalidateDaAttestationAttachment,
 )
import Midgard.Validators.StateQueue (
  pauthenticatedQueueHeadForAppendV1,
  ppruneTimedOutBlockDescendantV1,
  ppruneUnavailableBlockDescendantV1,
  premoveFraudulentBlocksLinkV1,
  premoveLastFraudulentBlockV1,
  premoveUnattestedHeadAfterTimeoutV1,
  premoveUnavailableHeadV1,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "State Queue Library Tests"
    [ testGroup
        "decodeHeaderView"
        [ -- The gate returns the header unchanged, so this asserts both that it
          -- accepts and that it is the identity on what it accepts.
          testCase "accepts a header at protocol version 1, unchanged" $
            passertEval $ decodes 1 #== headerTerm 1
        , -- Version zero identifies the genesis sentinel, not a block.
          testCase "rejects a header at protocol version 0" $
            pfails $ decodes 0
        , testCase "rejects a header at a future protocol version" $
            pfails $ decodes 2
        ]
    , testGroup
        "commitBoundHeaderTimeIsValid"
        [ testCase "accepts an interval ending at the commit's upper bound" $
            passertEval $ commitBound 1_000 2_000 (closed 1_500 2_000)
        , testCase "rejects an interval ending before the commit's upper bound" $
            passertEval $ pnot #$ commitBound 1_000 1_900 (closed 1_500 2_000)
        , testCase "rejects an interval ending after the commit's upper bound" $
            passertEval $ pnot #$ commitBound 1_000 2_100 (closed 1_500 2_000)
        , testCase "rejects an empty interval" $
            passertEval $ pnot #$ commitBound 2_000 2_000 (closed 1_500 2_000)
        , testCase "rejects an inverted interval" $
            passertEval $ pnot #$ commitBound 2_500 2_000 (closed 1_500 2_000)
        , -- max_validity_range_length is eight minutes.
          testCase "accepts a commit range exactly at the width cap" $
            passertEval $ commitBound 1_000 480_000 (closed 0 480_000)
        , testCase "rejects a commit range wider than the cap" $
            pfails $ commitBound 1_000 480_001 (closed 0 480_001)
        , testCase "rejects a commit range unbounded above" $
            pfails $ commitBound 1_000 2_000 fromNegInf
        ]
    , testGroup
        "readers"
        [ -- The root/node split is the whole safety property of these two.
          -- ConfirmedState and StateQueueNode are different types at the same
          -- field positions, so a reader taking the wrong element would decode
          -- one as the other rather than fail.
          testCase "getConfirmedState reads the root's payload" $
            passertEval $
              pgetConfirmedState refInputs sqPolicy 0
                #== confirmedStateTerm
        , testCase "getConfirmedState rejects a node" $
            pfails $ pgetConfirmedState refInputs sqPolicy 1
        , testCase "getStateQueueNode rejects the root" $
            pfails $ pgetStateQueueNode refInputs sqPolicy 0 (\_ _ -> pconstant @PBool True)
        , -- The key is the node's NFT asset name with "MBLC" stripped, not
          -- anything stated in the datum — it is minted, so it authenticates
          -- which block was read.
          testCase "getStateQueueNode hands back the prefix-stripped header hash" $
            passertEval $
              pgetStateQueueNode refInputs sqPolicy 1 $
                \_node headerHash -> headerHash #== pconstant hashA
        , -- get_state_queue_node does not gate on protocol version; only
          -- get_block_datum_v1 does. This pair is what separates them.
          testCase "getStateQueueNode accepts a node at a future protocol version" $
            passertEval $
              pgetStateQueueNode refInputs sqPolicy 2 (\_ _ -> pconstant @PBool True)
        , testCase "getBlockDatumV1 rejects a node at a future protocol version" $
            pfails $ pgetBlockDatumV1 refInputs sqPolicy 2 (\_ _ -> pconstant @PBool True)
        , testCase "getBlockDatumV1 accepts a v1 node" $
            passertEval $
              pgetBlockDatumV1 refInputs sqPolicy 1 $
                \header _ -> header #== headerTerm 1
        , testCase "getPrevHeaderHashOfNodeV1 returns the predecessor" $
            passertEval $
              pgetPrevHeaderHashOfNodeV1 refInputs sqPolicy 1 (pconstant hashA)
                #== pdata (pconstant prevHash)
        , -- Without this check the caller learns the predecessor of whichever
          -- block sat at that index, not of the one it asked about.
          testCase "getPrevHeaderHashOfNodeV1 rejects a mismatched header hash" $
            pfails $
              pgetPrevHeaderHashOfNodeV1 refInputs sqPolicy 1 (pconstant hashB)
        ]
    , testGroup
        "validateDaAttestationAttachment"
        [ testCase "accepts an attestation attached to an unattested block" $
            passertEval $ attach unattested attested
        , testCase "accepts added lovelace" $
            passertEval $ attach unattested attested {nodeLovelace = 3_000_000}
        , testCase "rejects removed lovelace" $
            pfails $ attach unattested attested {nodeLovelace = 1_500_000}
        , testCase "rejects a changed address" $
            pfails $ attach unattested attested {nodeAddress = otherAddress}
        , testCase "rejects a wrong header hash on the output" $
            pfails $ attach unattested attested {nodeKey = hashB}
        , testCase "rejects a wrong header hash on the input" $
            pfails $ attach unattested {nodeKey = hashB} attested
        , testCase "rejects a changed link" $
            pfails $ attach unattested attested {nodeLink = linkTo hashB}
        , -- One-shot: a block that already carries an attestation cannot have
          -- that attestation swapped for another.
          testCase "rejects a block that is already attested" $
            pfails $ attach unattested {nodeAttestation = attestedStatus otherDaBondAssetNameBytes} attested
        , testCase "rejects a changed header" $
            pfails $ attach unattested attested {nodePrevHash = otherPrevHash}
        , testCase "rejects a wrong DA bond asset name" $
            pfails $ attach unattested attested {nodeAttestation = attestedStatus otherDaBondAssetNameBytes}
        , testCase "accepts a window ending at the attachment deadline" $
            passertEval $ attachWithin (closed 3_599_200 3_600_200) unattested attested
        , testCase "rejects a window ending after the attachment deadline" $
            passertEval $ pnot #$ attachWithin (closed 3_599_201 3_600_201) unattested attested
        ]
    , testGroup
        "authenticatedQueueHeadForAppendV1"
        [ testCase "uses the authenticated anchor for a one-node queue" $
            passertEval $
              authenticatedHead hashA (nodeData unattested) (pcon PDNothing)
                #== stateQueueNodeTerm unattested
        , testCase "reads the separately authenticated head for a deeper queue" $
            passertEval $
              authenticatedHead hashB (nodeData unattested {nodeKey = hashB}) (pcon $ PDJust $ pdata 1)
                #== stateQueueNodeTerm unattested
        , testCase "rejects a missing deep-head reference" $
            pfails $ authenticatedHead hashB (nodeData unattested {nodeKey = hashB}) (pcon PDNothing)
        , testCase "rejects a reference whose NFT key is not the root head" $
            pfails $
              authenticatedHead hashB (nodeData unattested {nodeKey = hashB}) (pcon $ PDJust $ pdata 2)
        ]
    , timeoutRemovalTests
    , fraudulentRemovalTests
    ]

fraudulentRemovalTests :: TestTree
fraudulentRemovalTests =
  testGroup
    "fraudulent removal helpers"
    [ testCase "prunes a transitive descendant committed by a rotated operator" $
        passertEval $ removeFraudLink hashA rotatedDescendant
    , testCase "rejects a descendant prune from the wrong fraud-proved anchor" $
        pfails $ removeFraudLink hashC rotatedDescendant
    , testCase "removes the terminal fraud-proved target" $
        passertEval $ removeLastFraudulent defaultOperator fraudTail
    , testCase "rejects a target naming another operator" $
        pfails $ removeLastFraudulent rotatedOperator fraudTail
    , testCase "rejects removing a nonterminal fraud-proved target" $
        pfails $ removeLastFraudulent defaultOperator fraudTail {nodeLink = linkTo hashC}
    ]

timeoutRemovalTests :: TestTree
timeoutRemovalTests =
  testGroup
    "timeout removal helpers"
    [ testCase "prunes a descendant of the current unattested head" $
        passertEval $ pruneUnattested (rootOutWithLink $ linkTo hashA) timedOutHead
    , testCase "rejects pruning from an unattested node that is not the head" $
        pfails $ pruneUnattested (rootOutWithLink $ linkTo hashB) timedOutHead
    , testCase "removes the terminal unattested head at the boundary" $
        passertEval $ removeUnattested timedOutTerminal hashA
    , testCase "rejects an attested head after the attachment race" $
        pfails $ removeUnattested attested hashA
    , testCase "rejects removing a nonterminal unattested head" $
        pfails $ removeUnattested timedOutHead hashA
    , testCase "rejects a wrong unattested-header identity" $
        pfails $ removeUnattested timedOutTerminal hashB
    , testCase "prunes a descendant of the exact challenged head" $
        passertEval $ pruneUnavailable challengedHead challengeAssetNameBytes
    , testCase "removes the terminal exact challenged head" $
        passertEval $ removeUnavailable challengedTerminal challengeAssetNameBytes
    , testCase "rejects a substituted challenge identity" $
        pfails $ removeUnavailable challengedTerminal otherChallengeAssetNameBytes
    , testCase "rejects an attested but unchallenged head" $
        pfails $ removeUnavailable attested challengeAssetNameBytes
    ]

--------------------------------------------------------------------------------
-- Interval fixtures
--------------------------------------------------------------------------------

closed :: Integer -> Integer -> Interval POSIXTime
closed lo hi =
  Interval
    (LowerBound (Finite (POSIXTime lo)) True)
    (UpperBound (Finite (POSIXTime hi)) True)

fromNegInf :: Interval POSIXTime
fromNegInf = Interval (LowerBound NegInf True) (UpperBound (Finite (POSIXTime 2_000)) True)

commitBound :: forall s. Integer -> Integer -> Interval POSIXTime -> Term s PBool
commitBound startTime endTime range =
  pcommitBoundHeaderTimeIsValid
    (pconstant startTime)
    (pconstant endTime)
    (pconstant range :: Term s (PInterval PPosixTime))

--------------------------------------------------------------------------------
-- Header fixtures
--------------------------------------------------------------------------------

prevHash :: BS.ByteString
prevHash = BS.replicate 28 0x02

otherPrevHash :: BS.ByteString
otherPrevHash = BS.replicate 28 0x09

{- | A @HeaderV1@ at a chosen protocol version.

Twenty-five fields; only two are varied, but the arity has to be right or the
positional read lands elsewhere.
-}
headerData :: Integer -> BS.ByteString -> PD.Data
headerData protocolVersion prev = headerDataWithOperator protocolVersion prev defaultOperator

headerDataWithOperator :: Integer -> BS.ByteString -> BS.ByteString -> PD.Data
headerDataWithOperator protocolVersion prev blockOperator =
  PD.Constr
    0
    ( replicate 9 (PD.B emptyRoot) -- nine empty counted roots
        <> replicate 7 (PD.I 0) -- seven counts
        <> [PD.I 100, PD.I 200, PD.I 0, PD.I 0, PD.I 0, PD.I 0] -- times and fees
        <> [PD.B prev]
        <> [PD.B blockOperator]
        <> [PD.I protocolVersion]
    )

emptyRoot :: BS.ByteString
emptyRoot =
  BS.pack
    [ 0x0e, 0x57, 0x51, 0xc0, 0x26, 0xe5, 0x43, 0xb2
    , 0xe8, 0xab, 0x2e, 0xb0, 0x60, 0x99, 0xda, 0xa1
    , 0xd1, 0xe5, 0xdf, 0x47, 0x77, 0x8f, 0x77, 0x87
    , 0xfa, 0xab, 0x45, 0xcd, 0xf1, 0x2f, 0xe3, 0xa8
    ]

decodes :: forall s. Integer -> Term s (PAsData PHeaderV1)
decodes protocolVersion = pdecodeHeaderView # headerTerm protocolVersion

headerTerm :: forall s. Integer -> Term s (PAsData PHeaderV1)
headerTerm protocolVersion =
  punsafeCoerce (pconstant @PData (headerData protocolVersion prevHash))

--------------------------------------------------------------------------------
-- Linked-list element fixtures
--------------------------------------------------------------------------------

sqPolicyHex :: String
sqPolicyHex = "5f2f0ca9a7ea60caf6a30c5b1f3a4c4a1f9c0e3b2d8a7c6b5a4938271"

sqPolicySymbol :: CurrencySymbol
sqPolicySymbol = currencySymbolFromHex sqPolicyHex

sqPolicy :: forall s. Term s (PAsData PCurrencySymbol)
sqPolicy = pdata (pconstant sqPolicySymbol)

daBondAssetNameBytes :: BS.ByteString
daBondAssetNameBytes = "DABN" <> BS.replicate 28 0x77

otherDaBondAssetNameBytes :: BS.ByteString
otherDaBondAssetNameBytes = "DABN" <> BS.replicate 28 0x78

challengeAssetNameBytes, otherChallengeAssetNameBytes :: BS.ByteString
challengeAssetNameBytes = "DACH" <> BS.replicate 28 0x88
otherChallengeAssetNameBytes = "DACH" <> BS.replicate 28 0x89

unattestedStatus :: PD.Data
unattestedStatus = PD.Constr 0 []

attestedStatus :: BS.ByteString -> PD.Data
attestedStatus assetName = PD.Constr 1 [PD.B assetName]

challengedStatus :: BS.ByteString -> BS.ByteString -> PD.Data
challengedStatus bondAssetName challengeAssetName =
  PD.Constr 2 [PD.B bondAssetName, PD.B challengeAssetName]

hashA :: BS.ByteString
hashA =
  fromBuiltin $
    blake2b_224 $
      serialiseData $
        dataToBuiltinData (headerData 1 prevHash)

hashB :: BS.ByteString
hashB = BS.replicate 28 0xbb

hashC :: BS.ByteString
hashC = BS.replicate 28 0xcc

defaultOperator, rotatedOperator :: BS.ByteString
defaultOperator = BS.replicate 28 0x03
rotatedOperator = BS.replicate 28 0x04

queueAddress :: Address
queueAddress = scriptHashAddress (ScriptHash (unCurrencySymbol sqPolicySymbol))

otherAddress :: Address
otherAddress = scriptHashAddress (ScriptHash (toBuiltin (BS.replicate 28 0x5c)))

linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

{- | The mutable parts of a state-queue node UTxO.

A record so that each rejection test names the single field it changed, rather
than restating a whole fixture and leaving the reader to diff them.
-}
data Node = Node
  { nodeAddress :: Address
  , nodeLovelace :: Int
  , nodeKey :: BS.ByteString
  , nodePrevHash :: BS.ByteString
  , nodeVersion :: Integer
  , nodeOperator :: BS.ByteString
  , nodeAttestation :: PD.Data
  , nodeLink :: PD.Data
  }

unattested :: Node
unattested =
  Node
    { nodeAddress = queueAddress
    , nodeLovelace = 2_000_000
    , nodeKey = hashA
    , nodePrevHash = prevHash
    , nodeVersion = 1
    , nodeOperator = defaultOperator
    , nodeAttestation = unattestedStatus
    , nodeLink = linkNone
    }

attested :: Node
attested = unattested {nodeAttestation = attestedStatus daBondAssetNameBytes}

timedOutHead, timedOutTerminal, challengedHead, challengedTerminal :: Node
timedOutHead = unattested {nodeLink = linkTo hashB}
timedOutTerminal = unattested {nodeLink = linkNone}
challengedHead = timedOutHead {nodeAttestation = challengedStatus daBondAssetNameBytes challengeAssetNameBytes}
challengedTerminal = timedOutTerminal {nodeAttestation = challengedStatus daBondAssetNameBytes challengeAssetNameBytes}

-- | @Element { data: Node(StateQueueNode { header, da_attestation }), link }@.
nodeOut :: Node -> TxOut
nodeOut n =
  TxOut
    (nodeAddress n)
    (mkAdaValue (nodeLovelace n) <> singleton sqPolicySymbol (blockName (nodeKey n)) 1)
    ( OutputDatum . Datum . dataToBuiltinData $
        element
          (PD.Constr 1 [nodeData n])
          (nodeLink n)
    )
    Nothing

nodeData :: Node -> PD.Data
nodeData n =
  PD.Constr
    0
    [ headerDataWithOperator (nodeVersion n) (nodePrevHash n) (nodeOperator n)
    , nodeAttestation n
    ]

stateQueueNodeTerm :: forall s. Node -> Term s PStateQueueNode
stateQueueNodeTerm n = pfromData (punsafeCoerce (pconstant @PData (nodeData n)))

-- | The root: @Element { data: Root(ConfirmedState), link }@.
rootOut :: TxOut
rootOut = rootOutWithLink (linkTo hashA)

rootOutWithLink :: PD.Data -> TxOut
rootOutWithLink rootLink =
  TxOut
    queueAddress
    (mkAdaValue 2_000_000 <> singleton sqPolicySymbol (TokenName (toBuiltin ("MIDGARD_CONFIRMED_STATE" :: BS.ByteString))) 1)
    (OutputDatum . Datum . dataToBuiltinData $ element (PD.Constr 0 [confirmedStateData]) rootLink)
    Nothing

confirmedStateTerm :: forall s. Term s PConfirmedState
confirmedStateTerm = pfromData (punsafeCoerce (pconstant @PData confirmedStateData))

confirmedStateData :: PD.Data
confirmedStateData =
  PD.Constr
    0
    [ PD.B (BS.replicate 28 0x10)
    , PD.B (BS.replicate 28 0x11)
    , PD.B (BS.replicate 32 0x12)
    , PD.I 0
    , PD.I 100
    , PD.I 1
    ]

element :: PD.Data -> PD.Data -> PD.Data
element elementData link = PD.Constr 0 [elementData, link]

blockName :: BS.ByteString -> TokenName
blockName key = TokenName (toBuiltin ("MBLC" <> key))

--------------------------------------------------------------------------------
-- Term plumbing
--------------------------------------------------------------------------------

-- | Reference inputs: root at 0, a v1 block at 1, a v2 block at 2.
refInputs :: forall s. Term s (PBuiltinList (PAsData PTxInInfo))
refInputs =
  dataList
    [ toPD (TxInInfo (outRefN 0) rootOut)
    , toPD (TxInInfo (outRefN 1) (nodeOut unattested))
    , toPD (TxInInfo (outRefN 2) (nodeOut unattested {nodeKey = hashB, nodeVersion = 2}))
    ]

attach :: forall s. Node -> Node -> Term s PBool
attach = attachWithin (closed 3_599_200 3_600_200)

attachWithin :: forall s. Interval POSIXTime -> Node -> Node -> Term s PBool
attachWithin validityRange inputNode outputNode =
  pvalidateDaAttestationAttachment
    (dataList [toPD (TxInInfo (outRefN 0) (nodeOut inputNode))])
    (dataList [toPD (nodeOut outputNode)])
    sqPolicy
    0
    0
    (pconstant hashA)
    (pdata (pconstant (TokenName (toBuiltin daBondAssetNameBytes))))
    (pconstant validityRange)

authenticatedHead ::
  forall s.
  BS.ByteString ->
  PD.Data ->
  Term s (PMaybeData PInteger) ->
  Term s PStateQueueNode
authenticatedHead appendAnchorHash appendAnchorData mHeadRefIndex =
  pauthenticatedQueueHeadForAppendV1
    refInputs
    sqPolicy
    (pconstant appendAnchorHash)
    (pconstant appendAnchorData)
    0
    mHeadRefIndex

pruneUnattested :: forall s. TxOut -> Node -> Term s PBool
pruneUnattested rootReference headNode =
  ppruneTimedOutBlockDescendantV1
    sqPolicy
    ( dataList
        [ toPD (TxInInfo (outRefN 1) (nodeOut headNode))
        , toPD (TxInInfo (outRefN 2) (nodeOut descendantNode))
        ]
    )
    (dataList [toPD (nodeOut headNode {nodeLink = linkNone})])
    (dataList [toPD (TxInInfo (outRefN 0) rootReference)])
    (pmint $ burnNode hashB)
    (pconstant $ closed 3_600_200 3_601_200)
    (pconstant hashA)
    0
    (pconstant $ outRefN 1)
    0

removeUnattested :: forall s. Node -> BS.ByteString -> Term s PBool
removeUnattested headNode requestedHeaderHash =
  premoveUnattestedHeadAfterTimeoutV1
    sqPolicy
    ( dataList
        [ toPD (TxInInfo (outRefN 0) rootOut)
        , toPD (TxInInfo (outRefN 1) (nodeOut headNode))
        ]
    )
    (dataList [toPD (rootOutWithLink linkNone)])
    (pmint $ burnNode hashA)
    (pconstant $ closed 3_600_200 3_601_200)
    (pconstant requestedHeaderHash)
    (pconstant $ outRefN 0)
    0

pruneUnavailable :: forall s. Node -> BS.ByteString -> Term s PBool
pruneUnavailable headNode challengeName =
  ppruneUnavailableBlockDescendantV1
    sqPolicy
    ( dataList
        [ toPD (TxInInfo (outRefN 1) (nodeOut headNode))
        , toPD (TxInInfo (outRefN 2) (nodeOut descendantNode))
        ]
    )
    (dataList [toPD (nodeOut headNode {nodeLink = linkNone})])
    (dataList [toPD (TxInInfo (outRefN 0) rootOut)])
    (pmint $ burnNode hashB)
    (pconstant hashA)
    (pconstant challengeName)
    0
    (pconstant $ outRefN 1)
    0

removeUnavailable :: forall s. Node -> BS.ByteString -> Term s PBool
removeUnavailable headNode challengeName =
  premoveUnavailableHeadV1
    sqPolicy
    ( dataList
        [ toPD (TxInInfo (outRefN 0) rootOut)
        , toPD (TxInInfo (outRefN 1) (nodeOut headNode))
        ]
    )
    (dataList [toPD (rootOutWithLink linkNone)])
    (pmint $ burnNode hashA)
    (pconstant hashA)
    (pconstant challengeName)
    (pconstant $ outRefN 0)
    0

descendantNode :: Node
descendantNode =
  unattested
    { nodeKey = hashB
    , nodePrevHash = hashA
    , nodeLink = linkNone
    }

burnNode :: BS.ByteString -> MintValue
burnNode key = UnsafeMintValue (getValue $ singleton sqPolicySymbol (blockName key) (-1))

pmint :: forall s. MintValue -> Term s PMintValue
pmint value = pfromData (pconstant @(PAsData PMintValue) value)

fraudAnchor, rotatedDescendant, fraudParent, fraudTail :: Node
fraudAnchor = unattested {nodeKey = hashA, nodeLink = linkTo hashB}
rotatedDescendant =
  unattested
    { nodeKey = hashB
    , nodePrevHash = hashA
    , nodeOperator = rotatedOperator
    , nodeLink = linkNone
    }
fraudParent =
  unattested
    { nodeKey = hashA
    , nodeOperator = rotatedOperator
    , nodeLink = linkTo hashB
    }
fraudTail =
  unattested
    { nodeKey = hashB
    , nodePrevHash = hashA
    , nodeOperator = defaultOperator
    , nodeLink = linkNone
    }

removeFraudLink :: forall s. BS.ByteString -> Node -> Term s PBool
removeFraudLink requestedAnchorHash removedNode =
  premoveFraudulentBlocksLinkV1
    sqPolicy
    ( dataList
        [ toPD (TxInInfo (outRefN 1) (nodeOut fraudAnchor))
        , toPD (TxInInfo (outRefN 2) (nodeOut removedNode))
        ]
    )
    (dataList [toPD (nodeOut fraudAnchor {nodeLink = nodeLink removedNode})])
    (pmint $ burnNode $ nodeKey removedNode)
    (pconstant requestedAnchorHash)
    (pconstant $ outRefN 1)
    0

removeLastFraudulent :: forall s. BS.ByteString -> Node -> Term s PBool
removeLastFraudulent expectedOperator removedNode =
  premoveLastFraudulentBlockV1
    sqPolicy
    ( dataList
        [ toPD (TxInInfo (outRefN 1) (nodeOut fraudParent))
        , toPD (TxInInfo (outRefN 2) (nodeOut removedNode))
        ]
    )
    (dataList [toPD (nodeOut fraudParent {nodeLink = nodeLink removedNode})])
    (pmint $ burnNode $ nodeKey removedNode)
    (punsafeCoerce $ pconstant @PData $ PD.B expectedOperator)
    (pconstant $ nodeKey removedNode)
    (pconstant $ outRefN 1)
    0

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

toPD :: ToData a => a -> PD.Data
toPD = builtinDataToData . toBuiltinData

{- | A @Data@ list read back as a builtin list of Data-encoded values.

Going through @Data@ rather than a lift instance keeps the fixture honest about
what the on-chain code actually receives.
-}
dataList :: forall s a. [PD.Data] -> Term s (PBuiltinList (PAsData a))
dataList xs = punsafeCoerce (pasList # pconstant @PData (PD.List xs))
