module Midgard.Node.TxOutRef (
  parseTxOutRefLabel,
  txOutRefCborBytes,
) where

import Control.Monad (unless)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as B16
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import Midgard.Node.DB.Types (TxHash, TxOutRefCbor)
import Midgard.Node.DB.Types qualified as DB.Types
import Text.Read (readMaybe)

parseTxOutRefLabel :: Text -> Either Text TxOutRefCbor
parseTxOutRefLabel label = do
  let (txHashText, rest) = Text.breakOn "#" label
  outputIndexText <-
    case Text.stripPrefix "#" rest of
      Nothing -> Left "txOutRef must be formatted as txHash#outputIndex"
      Just value -> Right value
  txHash <- decodeTxHashText txHashText
  outputIndex <- parseOutputIndex outputIndexText
  DB.Types.mkTxOutRefCbor (txOutRefCborBytes txHash outputIndex)

txOutRefCborBytes :: TxHash -> Word64 -> ByteString
txOutRefCborBytes txHash outputIndex =
  ByteString.pack [0x82, 0x58, 0x20]
    <> DB.Types.unTxHash txHash
    <> encodeCborWord outputIndex

decodeTxHashText :: Text -> Either Text TxHash
decodeTxHashText value =
  case B16.decode (Text.encodeUtf8 value) of
    Left err -> Left (Text.pack err)
    Right bytes -> DB.Types.mkTxHash bytes

parseOutputIndex :: Text -> Either Text Word64
parseOutputIndex value = do
  unless (not (Text.null value) && Text.all isDigit value) $
    Left "outputIndex must be a non-negative integer"
  case readMaybe (Text.unpack value) of
    Nothing -> Left "outputIndex is too large"
    Just parsed -> Right parsed

encodeCborWord :: Word64 -> ByteString
encodeCborWord value
  | value <= 23 = ByteString.singleton (fromIntegral value)
  | value <= 0xff = ByteString.pack [0x18, fromIntegral value]
  | value <= 0xffff = ByteString.cons 0x19 (encodeBigEndian 2 value)
  | value <= 0xffffffff = ByteString.cons 0x1a (encodeBigEndian 4 value)
  | otherwise = ByteString.cons 0x1b (encodeBigEndian 8 value)

encodeBigEndian :: Int -> Word64 -> ByteString
encodeBigEndian byteCount value =
  ByteString.pack
    [ fromIntegral ((value `div` (256 ^ power)) .|. 0)
    | power <- reverse [0 .. byteCount - 1]
    ]
