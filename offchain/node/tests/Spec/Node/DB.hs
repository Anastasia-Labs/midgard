module Spec.Node.DB (tests) where

import Midgard.Node.DB.AddressHistory (lookupAddressTxs)
import Midgard.Node.DB.Blocks (lookupBlockTxHashes)
import Midgard.Node.DB.Deposits (
  DepositStatus (..),
  lookupDepositByEventId,
  lookupDepositsByCardanoTxHash,
 )
import Midgard.Node.DB.Hex qualified as DB.Hex
import Midgard.Node.DB.MempoolLedger (
  StoredUtxo (..),
  lookupUtxoByOutRef,
  lookupUtxosByAddress,
  lookupUtxosByOutRefs,
 )
import Midgard.Node.DB.Transactions (lookupTxByHash)
import Midgard.Node.DB.TxStatus (TxAdmissionStatus (..), TxRejection (..), TxStatusFacts (..), lookupTxStatusFacts)
import Midgard.Node.DB.Types qualified as DB.Types
import Spec.Node.DB.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "node-db"
    [ testCase "lookupTxByHash rejects invalid mempool transaction CBOR" $
        withTestPool $ \pool -> do
          let txHash = testTxHash 1
              mempoolTx = blob 2
              immutableTx = blob 3
          seedMempoolRow pool (DB.Types.unTxHash txHash) mempoolTx
          seedImmutableRow pool (DB.Types.unTxHash txHash) immutableTx
          actual <- lookupTxByHash pool txHash
          case actual of
            Left _ -> pure ()
            Right _ -> assertFailure "expected invalid mempool transaction CBOR to be rejected"
    , testCase "lookupTxByHash rejects invalid immutable transaction CBOR" $
        withTestPool $ \pool -> do
          let txHash = testTxHash 4
              immutableTx = blob 5
          seedImmutableRow pool (DB.Types.unTxHash txHash) immutableTx
          actual <- lookupTxByHash pool txHash
          case actual of
            Left _ -> pure ()
            Right _ -> assertFailure "expected invalid immutable transaction CBOR to be rejected"
    , testCase "lookupBlockTxHashes returns all tx hashes for the block" $
        withTestPool $ \pool -> do
          let headerHash = testHeaderHash 6
              txHashes = [blob 7, blob 8]
          mapM_ (seedBlockRow pool (DB.Types.unHeaderHash headerHash)) txHashes
          seedBlockRow pool (blob 9) (blob 10)
          actual <- lookupBlockTxHashes pool headerHash
          actual @?= txHashes
    , testCase "lookupUtxoByOutRef returns a single stored UTxO" $
        withTestPool $ \pool -> do
          let expected = StoredUtxo (outRefBlob 11) (blob 12)
          seedLedgerRow pool expected.outref "addr_test1vr4l7q0midgard" expected.outputCbor
          actual <- lookupUtxoByOutRef pool (testTxOutRef 11)
          actual @?= Just expected
    , testCase "lookupUtxosByAddress returns only rows for the requested address" $
        withTestPool $ \pool -> do
          let address = "addr_test1vr4l7q0midgard"
              expected =
                [ StoredUtxo (outRefBlob 13) (blob 14)
                , StoredUtxo (outRefBlob 15) (blob 16)
                ]
          mapM_ (\utxo -> seedLedgerRow pool utxo.outref address utxo.outputCbor) expected
          seedLedgerRow pool (outRefBlob 17) "addr_test1vq8other" (blob 18)
          actual <- lookupUtxosByAddress pool address
          actual @?= expected
    , testCase "lookupUtxosByOutRefs preserves request order and skips missing rows" $
        withTestPool $ \pool -> do
          let utxoA = StoredUtxo (outRefBlob 19) (blob 20)
              utxoB = StoredUtxo (outRefBlob 21) (blob 22)
              utxoARef = testTxOutRef 19
              utxoBRef = testTxOutRef 21
          seedLedgerRow pool utxoA.outref "addr_test1vr4l7q0midgard" utxoA.outputCbor
          seedLedgerRow pool utxoB.outref "addr_test1vr4l7q0midgard" utxoB.outputCbor
          actual <- lookupUtxosByOutRefs pool [utxoBRef, testTxOutRef 23, utxoARef]
          actual @?= [utxoB, utxoA]
    , testCase "lookupAddressTxs rejects invalid transaction CBOR linked through address history" $
        withTestPool $ \pool -> do
          let address = "addr_test1vr4l7q0midgard"
              txHashA = testTxHash 24
              txHashB = testTxHash 25
              txA = blob 26
              txB = blob 27
          seedMempoolRow pool (DB.Types.unTxHash txHashA) txA
          seedImmutableRow pool (DB.Types.unTxHash txHashB) txB
          seedAddressHistoryRow pool address (DB.Types.unTxHash txHashA)
          seedAddressHistoryRow pool address (DB.Types.unTxHash txHashB)
          seedAddressHistoryRow pool "addr_test1vq8other" (blob 28)
          actual <- lookupAddressTxs pool address
          case actual of
            Left _ -> pure ()
            Right _ -> assertFailure "expected invalid address-history transaction CBOR to be rejected"
    , testCase "TxHash rejects the wrong byte length" $
        case DB.Types.mkTxHash (blob 1) of
          Left _ -> pure ()
          Right _ -> assertFailure "expected mkTxHash to reject a non-32-byte blob"
    , testCase "HeaderHash rejects the wrong byte length" $
        case DB.Types.mkHeaderHash (txHashBlob 2) of
          Left _ -> pure ()
          Right _ -> assertFailure "expected mkHeaderHash to reject a non-28-byte blob"
    , testCase "hex decoders enforce schema lengths" $ do
        assertBool "tx hash hex should decode" (either (const False) (const True) (DB.Hex.decodeTxHashHex (hexText (txHashBlob 30))))
        assertBool "header hash hex should decode" (either (const False) (const True) (DB.Hex.decodeHeaderHashHex (hexText (headerHashBlob 31))))
        assertBool "short tx hash hex should be rejected" (either (const True) (const False) (DB.Hex.decodeTxHashHex (hexText (blob 32))))
        assertBool "32-byte block hash hex should be rejected as header hash" (either (const True) (const False) (DB.Hex.decodeHeaderHashHex (hexText (txHashBlob 33))))
    , testCase "lookupTxStatusFacts combines admission, rejection, and tx lifecycle rows" $
        withTestPool $ \pool -> do
          let txHash = testTxHash 34
              rejectedAt = testTime 10
          seedTxAdmissionRow pool (DB.Types.unTxHash txHash) "queued"
          seedTxRejectionRow pool (DB.Types.unTxHash txHash) "invalid_witness" (Just "bad signature") rejectedAt
          seedProcessedMempoolRow pool (DB.Types.unTxHash txHash) (blob 35)
          actual <- lookupTxStatusFacts pool txHash
          actual
            @?= TxStatusFacts
              { admissionStatus = Just (TxAdmissionStatus "queued")
              , rejection = Just (TxRejection "invalid_witness" (Just "bad signature") rejectedAt)
              , inImmutable = False
              , inMempool = False
              , inProcessedMempool = True
              }
    , testCase "lookupTxStatusFacts reports committed txs from immutable" $
        withTestPool $ \pool -> do
          let txHash = testTxHash 36
          seedImmutableRow pool (DB.Types.unTxHash txHash) (blob 37)
          actual <- lookupTxStatusFacts pool txHash
          actual.inImmutable @?= True
    , testCase "deposit status can be found by event id and Cardano tx hash" $
        withTestPool $ \pool -> do
          let eventId = testTxOutRef 38
              cardanoTxHash = testTxHash 39
              expected =
                DepositStatus
                  { eventId = DB.Types.unTxOutRefCbor eventId
                  , eventInfo = blob 40
                  , inclusionTime = testTime 41
                  , cardanoTxHash = DB.Types.unTxHash cardanoTxHash
                  , ledgerTxId = txHashBlob 42
                  , ledgerOutput = blob 43
                  , ledgerAddress = "addr_test1vr4l7q0midgard"
                  , projectedHeaderHash = Just (headerHashBlob 44)
                  , status = "projected"
                  }
          seedDepositRow pool expected
          byEventId <- lookupDepositByEventId pool eventId
          byTxHash <- lookupDepositsByCardanoTxHash pool cardanoTxHash
          byEventId @?= Just expected
          byTxHash @?= [expected]
    ]
