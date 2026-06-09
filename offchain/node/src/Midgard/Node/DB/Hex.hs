module Midgard.Node.DB.Hex (
  decodeHex,
  decodeHexOfLength,
  decodeHeaderHashHex,
  decodeTxHashHex,
  decodeTxOutRefHex,
  encodeHex,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text qualified as DataText
import Data.Text.Encoding qualified as Text
import Midgard.Node.DB.Types (HeaderHash, TxHash, TxOutRefCbor)
import Midgard.Node.DB.Types qualified as DB.Types

decodeHex :: Text -> Either Text ByteString
decodeHex value =
  firstToText (B16.decode (Text.encodeUtf8 value))

decodeHexOfLength :: Int -> Text -> Either Text ByteString
decodeHexOfLength expectedLength value = do
  decoded <- decodeHex value
  if BS.length decoded == expectedLength
    then Right decoded
    else Left "invalid hex string length"

decodeTxHashHex :: Text -> Either Text TxHash
decodeTxHashHex value =
  decodeHex value >>= DB.Types.mkTxHash

decodeHeaderHashHex :: Text -> Either Text HeaderHash
decodeHeaderHashHex value =
  decodeHex value >>= DB.Types.mkHeaderHash

decodeTxOutRefHex :: Text -> Either Text TxOutRefCbor
decodeTxOutRefHex value =
  decodeHex value >>= DB.Types.mkTxOutRefCbor

encodeHex :: ByteString -> Text
encodeHex = Text.decodeUtf8 . B16.encode

firstToText :: Either String ByteString -> Either Text ByteString
firstToText =
  either
    (Left . DataText.pack)
    Right
