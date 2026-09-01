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
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
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
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PPosixTime, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.LedgerState (PConfirmedState, PHeaderV1)
import Midgard.StateQueue (
  pcommitBoundHeaderTimeIsValid,
  pdecodeHeaderView,
  pgetBlockDatumV1,
  pgetConfirmedState,
  pgetPrevHeaderHashOfNodeV1,
  pgetStateQueueNode,
  pvalidateDaAttestationAttachment,
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
            pfails $ attach unattested {nodeAttestation = daPolicyBytes} attested
        , testCase "rejects a changed header" $
            pfails $ attach unattested attested {nodePrevHash = otherPrevHash}
        , testCase "rejects a wrong attestation policy id" $
            pfails $ attach unattested attested {nodeAttestation = otherDaPolicyBytes}
        ]
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
headerData protocolVersion prev =
  PD.Constr
    0
    ( replicate 9 (PD.B (BS.replicate 32 0x01)) -- nine roots
        <> replicate 7 (PD.I 0) -- seven counts
        <> [PD.I 100, PD.I 200, PD.I 0, PD.I 0, PD.I 0, PD.I 0] -- times and fees
        <> [PD.B prev]
        <> [PD.B (BS.replicate 28 0x03)] -- operator_vkey
        <> [PD.I protocolVersion]
    )

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

daPolicyBytes :: BS.ByteString
daPolicyBytes = BS.replicate 28 0x77

otherDaPolicyBytes :: BS.ByteString
otherDaPolicyBytes = BS.replicate 28 0x78

daPolicy :: forall s. Term s (PAsData PCurrencySymbol)
daPolicy = pdata (pconstant (CurrencySymbol (toBuiltin daPolicyBytes)))

hashA :: BS.ByteString
hashA = BS.replicate 28 0xaa

hashB :: BS.ByteString
hashB = BS.replicate 28 0xbb

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
  , nodeAttestation :: BS.ByteString
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
    , nodeAttestation = "" -- no_da_attestation
    , nodeLink = linkNone
    }

attested :: Node
attested = unattested {nodeAttestation = daPolicyBytes}

-- | @Element { data: Node(StateQueueNode { header, da_attestation }), link }@.
nodeOut :: Node -> TxOut
nodeOut n =
  TxOut
    (nodeAddress n)
    (mkAdaValue (nodeLovelace n) <> singleton sqPolicySymbol (blockName (nodeKey n)) 1)
    ( OutputDatum . Datum . dataToBuiltinData $
        element
          (PD.Constr 1 [PD.Constr 0 [headerData (nodeVersion n) (nodePrevHash n), PD.B (nodeAttestation n)]])
          (nodeLink n)
    )
    Nothing

-- | The root: @Element { data: Root(ConfirmedState), link }@.
rootOut :: TxOut
rootOut =
  TxOut
    queueAddress
    (mkAdaValue 2_000_000 <> singleton sqPolicySymbol (TokenName (toBuiltin ("MIDGARD_CONFIRMED_STATE" :: BS.ByteString))) 1)
    (OutputDatum . Datum . dataToBuiltinData $ element (PD.Constr 0 [confirmedStateData]) (linkTo hashA))
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
attach inputNode outputNode =
  pvalidateDaAttestationAttachment
    (dataList [toPD (TxInInfo (outRefN 0) (nodeOut inputNode))])
    (dataList [toPD (nodeOut outputNode)])
    sqPolicy
    0
    0
    (pconstant hashA)
    daPolicy

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
