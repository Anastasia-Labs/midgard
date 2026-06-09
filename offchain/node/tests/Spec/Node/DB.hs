module Spec.Node.DB (tests) where

import Control.Exception (bracket)
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Database.Persist.Sql (PersistValue (PersistByteString, PersistText), SqlBackend, rawExecute)
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import Midgard.Node.DB.AddressHistory (lookupAddressTxs)
import Midgard.Node.DB.Blocks (lookupBlockTxHashes)
import Midgard.Node.DB.Hex qualified as DB.Hex
import Midgard.Node.DB.MempoolLedger (
  StoredUtxo (..),
  lookupUtxoByOutRef,
  lookupUtxosByAddress,
  lookupUtxosByOutRefs,
 )
import Midgard.Node.DB.Transactions (lookupTxCborByHash)
import Midgard.Node.DB.Types qualified as DB.Types
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "node-db"
    [ testCase "lookupTxCborByHash prefers mempool rows over immutable rows" $
        withTestPool $ \pool -> do
          let txHash = testTxHash 1
              mempoolTx = blob 2
              immutableTx = blob 3
          seedMempoolRow pool (DB.Types.unTxHash txHash) mempoolTx
          seedImmutableRow pool (DB.Types.unTxHash txHash) immutableTx
          actual <- lookupTxCborByHash pool txHash
          actual @?= Just mempoolTx
    , testCase "lookupTxCborByHash falls back to immutable rows" $
        withTestPool $ \pool -> do
          let txHash = testTxHash 4
              immutableTx = blob 5
          seedImmutableRow pool (DB.Types.unTxHash txHash) immutableTx
          actual <- lookupTxCborByHash pool txHash
          actual @?= Just immutableTx
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
    , testCase "lookupAddressTxs returns tx CBOR for rows linked through address history" $
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
          actual @?= [txA, txB]
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
    ]

withTestPool :: (Pool SqlBackend -> IO a) -> IO a
withTestPool action =
  bracket acquire release action'
  where
    acquire = do
      dbPath <- freshDbPath
      pool <- runNoLoggingT (createSqlitePool (Text.pack dbPath) 1)
      initialiseSchema pool
      pure (dbPath, pool)
    release (dbPath, pool) = do
      destroyAllResources pool
      removeFile dbPath
    action' (_, pool) = action pool

{- | The query modules are backend-agnostic raw SQL, so SQLite gives us a fast
local harness for seeding deterministic mock rows and validating result
decoding without needing a live YugaByte instance.
-}
initialiseSchema :: Pool SqlBackend -> IO ()
initialiseSchema pool =
  runSqlPool setup pool
  where
    setup = do
      rawExecute "CREATE TABLE mempool (tx_id BLOB PRIMARY KEY, tx BLOB NOT NULL)" []
      rawExecute "CREATE TABLE immutable (tx_id BLOB PRIMARY KEY, tx BLOB NOT NULL)" []
      rawExecute "CREATE TABLE blocks (header_hash BLOB NOT NULL, tx_id BLOB NOT NULL)" []
      rawExecute "CREATE TABLE mempool_ledger (outref BLOB PRIMARY KEY, output BLOB NOT NULL, address TEXT NOT NULL)" []
      rawExecute "CREATE TABLE address_history (address TEXT NOT NULL, tx_id BLOB NOT NULL)" []

seedMempoolRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedMempoolRow pool txHash tx =
  runSqlPool (rawExecute "INSERT INTO mempool (tx_id, tx) VALUES (?, ?)" [toPersistBlob txHash, toPersistBlob tx]) pool

seedImmutableRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedImmutableRow pool txHash tx =
  runSqlPool (rawExecute "INSERT INTO immutable (tx_id, tx) VALUES (?, ?)" [toPersistBlob txHash, toPersistBlob tx]) pool

seedBlockRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedBlockRow pool headerHash txHash =
  runSqlPool (rawExecute "INSERT INTO blocks (header_hash, tx_id) VALUES (?, ?)" [toPersistBlob headerHash, toPersistBlob txHash]) pool

seedLedgerRow :: Pool SqlBackend -> ByteString -> Text -> ByteString -> IO ()
seedLedgerRow pool outref address output =
  runSqlPool
    ( rawExecute
        "INSERT INTO mempool_ledger (outref, output, address) VALUES (?, ?, ?)"
        [toPersistBlob outref, toPersistBlob output, toPersistText address]
    )
    pool

seedAddressHistoryRow :: Pool SqlBackend -> Text -> ByteString -> IO ()
seedAddressHistoryRow pool address txHash =
  runSqlPool
    (rawExecute "INSERT INTO address_history (address, tx_id) VALUES (?, ?)" [toPersistText address, toPersistBlob txHash])
    pool

toPersistBlob :: ByteString -> PersistValue
toPersistBlob = PersistByteString

toPersistText :: Text -> PersistValue
toPersistText = PersistText

blob :: Word8 -> ByteString
blob byte = ByteString.pack [byte, byte + 1, byte + 2]

txHashBlob :: Word8 -> ByteString
txHashBlob byte = deterministicBlob 32 byte

headerHashBlob :: Word8 -> ByteString
headerHashBlob byte = deterministicBlob 28 byte

outRefBlob :: Word8 -> ByteString
outRefBlob byte = deterministicBlob 10 byte

deterministicBlob :: Int -> Word8 -> ByteString
deterministicBlob len seed =
  ByteString.pack [seed + fromIntegral offset | offset <- [0 .. len - 1]]

testTxHash :: Word8 -> DB.Types.TxHash
testTxHash byte =
  either (error . Text.unpack) id (DB.Types.mkTxHash (txHashBlob byte))

testHeaderHash :: Word8 -> DB.Types.HeaderHash
testHeaderHash byte =
  either (error . Text.unpack) id (DB.Types.mkHeaderHash (headerHashBlob byte))

testTxOutRef :: Word8 -> DB.Types.TxOutRefCbor
testTxOutRef byte =
  either (error . Text.unpack) id (DB.Types.mkTxOutRefCbor (outRefBlob byte))

hexText :: ByteString -> Text
hexText = DB.Hex.encodeHex

freshDbPath :: IO FilePath
freshDbPath = do
  tempDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tempDir "midgard-node-db-tests.sqlite"
  hClose handle
  pure path
