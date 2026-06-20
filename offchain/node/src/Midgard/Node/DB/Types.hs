module Midgard.Node.DB.Types (
  TxHash,
  mkTxHash,
  unTxHash,
  HeaderHash,
  mkHeaderHash,
  unHeaderHash,
  TxOutRefCbor,
  mkTxOutRefCbor,
  unTxOutRefCbor,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Persist.Class (PersistField)

newtype TxHash = TxHash
  { unTxHash :: ByteString
  }
  deriving stock (Eq, Show)
  deriving newtype (PersistField)

newtype HeaderHash = HeaderHash
  { unHeaderHash :: ByteString
  }
  deriving stock (Eq, Show)
  deriving newtype (PersistField)

newtype TxOutRefCbor = TxOutRefCbor
  { unTxOutRefCbor :: ByteString
  }
  deriving stock (Eq, Show)
  deriving newtype (PersistField)

mkTxHash :: ByteString -> Either Text TxHash
mkTxHash =
  mkFixedLength "transaction hash" 32 TxHash

mkHeaderHash :: ByteString -> Either Text HeaderHash
mkHeaderHash =
  mkFixedLength "block header hash" 28 HeaderHash

{- | TxOutRef values are stored as CBOR bytes in the Midgard DB/API surface.
The encoding is meaningful, but unlike tx ids and header hashes it is not a
single fixed width blob, so for now we only reject the obviously empty case.
-}
mkTxOutRefCbor :: ByteString -> Either Text TxOutRefCbor
mkTxOutRefCbor bytes
  | ByteString.null bytes = Left "txOutRef must not be empty"
  | otherwise = Right (TxOutRefCbor bytes)

mkFixedLength :: Text -> Int -> (ByteString -> a) -> ByteString -> Either Text a
mkFixedLength label expectedLength wrap bytes
  | ByteString.length bytes == expectedLength = Right (wrap bytes)
  | otherwise = Left (label <> " must be exactly " <> showText expectedLength <> " bytes")

showText :: (Show a) => a -> Text
showText = Text.pack . show
