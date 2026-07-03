module Midgard.Node.DB.Types (
  TxIdPersist (..),
  AddressPersist (..),
  HeaderHashPersist (..),
  TxOutRefPersist (..),
  RetainedRootMemberIdPersist (..),
  TxOutPersist (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Typeable (typeRep)

import Cardano.Api qualified as C
import Cardano.Ledger.Binary qualified as CBOR
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (
  PersistFieldSql (..),
  PersistValue (PersistByteString, PersistText),
  SqlType (SqlBlob, SqlString),
 )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import Web.PathPieces (PathPiece (fromPathPiece, toPathPiece))

newtype TxIdPersist = TxIdPersist {unTxIdPersist :: C.TxId}
  deriving stock (Eq, Ord)
  deriving (PersistField, PersistFieldSql) via PersistUsingRawBytes C.TxId

newtype HeaderHashPersist = HeaderHashPersist {unHeaderHashPersist :: C.Hash C.BlockHeader}
  deriving stock (Eq, Ord)
  deriving (PersistField, PersistFieldSql) via PersistUsingRawBytes (C.Hash C.BlockHeader)

newtype AddressPersist = AddressPersist {unAddressPersist :: C.Address C.ShelleyAddr}
  deriving stock (Eq, Ord, Show)

encodeAddressPersistText :: AddressPersist -> Text
encodeAddressPersistText = C.serialiseAddress . unAddressPersist

decodeAddressPersistText :: Text -> Either Text AddressPersist
decodeAddressPersistText t =
  case C.deserialiseAddress (C.AsAddress C.AsShelleyAddr) t of
    Nothing -> Left $ "When trying to deserialize a AddressPersist: malformed bech32 Address: " <> t
    Just x -> Right $ AddressPersist x

instance PersistField AddressPersist where
  toPersistValue = PersistText . encodeAddressPersistText
  fromPersistValue (PersistText t) = decodeAddressPersistText t
  fromPersistValue x =
    Left $
      "When trying to deserialize a AddressPersist: expected PersistText, received: "
        <> Text.pack (show x)

instance PersistFieldSql AddressPersist where
  sqlType _ = SqlString

encodeSerialiseAsRawBytesText :: (C.SerialiseAsRawBytes a) => a -> Text
encodeSerialiseAsRawBytesText = Text.decodeUtf8 . B16.encode . C.serialiseToRawBytes

decodeSerialiseAsRawBytesText ::
  forall a.
  (C.SerialiseAsRawBytes a) =>
  Proxy a ->
  Text ->
  Either Text a
decodeSerialiseAsRawBytesText _ t = do
  bytes <- first Text.pack $ B16.decode (Text.encodeUtf8 t)
  first (Text.pack . show) $
    C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @a) bytes

instance Show TxIdPersist where
  show = show . encodeSerialiseAsRawBytesText . unTxIdPersist

instance Read TxIdPersist where
  readsPrec d input = do
    (rawText, rest0) <- readsPrec d input
    case decodeSerialiseAsRawBytesText (Proxy @C.TxId) (Text.pack rawText) of
      Left _ -> []
      Right value -> pure (TxIdPersist value, rest0)

instance ToJSON TxIdPersist where
  toJSON = toJSON . encodeSerialiseAsRawBytesText . unTxIdPersist

instance FromJSON TxIdPersist where
  parseJSON =
    withText "TxIdPersist" $
      fmap TxIdPersist . either (fail . Text.unpack) pure . decodeSerialiseAsRawBytesText (Proxy @C.TxId)

instance ToHttpApiData TxIdPersist where
  toUrlPiece = encodeSerialiseAsRawBytesText . unTxIdPersist

instance FromHttpApiData TxIdPersist where
  parseUrlPiece =
    fmap TxIdPersist . decodeSerialiseAsRawBytesText (Proxy @C.TxId)

instance PathPiece TxIdPersist where
  toPathPiece = encodeSerialiseAsRawBytesText . unTxIdPersist
  fromPathPiece =
    either (const Nothing) (Just . TxIdPersist) . decodeSerialiseAsRawBytesText (Proxy @C.TxId)

instance Show HeaderHashPersist where
  show = show . encodeSerialiseAsRawBytesText . unHeaderHashPersist

instance Read HeaderHashPersist where
  readsPrec d input = do
    (rawText, rest0) <- readsPrec d input
    case decodeSerialiseAsRawBytesText (Proxy @(C.Hash C.BlockHeader)) (Text.pack rawText) of
      Left _ -> []
      Right value -> pure (HeaderHashPersist value, rest0)

instance ToJSON HeaderHashPersist where
  toJSON = toJSON . encodeSerialiseAsRawBytesText . unHeaderHashPersist

instance FromJSON HeaderHashPersist where
  parseJSON =
    withText "HeaderHashPersist" $
      fmap HeaderHashPersist
        . either (fail . Text.unpack) pure
        . decodeSerialiseAsRawBytesText (Proxy @(C.Hash C.BlockHeader))

instance ToHttpApiData HeaderHashPersist where
  toUrlPiece = encodeSerialiseAsRawBytesText . unHeaderHashPersist

instance FromHttpApiData HeaderHashPersist where
  parseUrlPiece =
    fmap HeaderHashPersist . decodeSerialiseAsRawBytesText (Proxy @(C.Hash C.BlockHeader))

instance PathPiece HeaderHashPersist where
  toPathPiece = encodeSerialiseAsRawBytesText . unHeaderHashPersist
  fromPathPiece =
    either (const Nothing) (Just . HeaderHashPersist)
      . decodeSerialiseAsRawBytesText (Proxy @(C.Hash C.BlockHeader))

instance ToJSON AddressPersist where
  toJSON = toJSON . encodeAddressPersistText

instance FromJSON AddressPersist where
  parseJSON =
    withText "AddressPersist" $
      either (fail . Text.unpack) pure . decodeAddressPersistText

instance ToHttpApiData AddressPersist where
  toUrlPiece = encodeAddressPersistText

instance FromHttpApiData AddressPersist where
  parseUrlPiece = decodeAddressPersistText

instance PathPiece AddressPersist where
  toPathPiece = encodeAddressPersistText
  fromPathPiece = either (const Nothing) Just . decodeAddressPersistText

newtype TxOutRefPersist = TxOutRefPersist {unTxOutRefPersist :: C.TxIn}
  deriving newtype (Eq, Ord)

-- TODO: Ensure this is the right protocol version to use for this.
txOutRefPersistProtVer :: CBOR.Version
txOutRefPersistProtVer = CBOR.shelleyProtVer

txOutRefPersistToCBOR :: TxOutRefPersist -> ByteString
txOutRefPersistToCBOR =
  CBOR.serialize' txOutRefPersistProtVer
    . C.toShelleyTxIn
    . unTxOutRefPersist

txOutRefPersistFromCBOR :: ByteString -> Either Text TxOutRefPersist
txOutRefPersistFromCBOR =
  bimap (Text.pack . show) TxOutRefPersist
    . (C.fromShelleyTxIn <$>)
    . CBOR.decodeFull' txOutRefPersistProtVer

instance Show TxOutRefPersist where
  show = show . B16.encode . txOutRefPersistToCBOR

instance Read TxOutRefPersist where
  readsPrec d input = do
    (bs16, rest0) <- readsPrec d input
    bs <- either (const []) pure $ B16.decode bs16
    case txOutRefPersistFromCBOR bs of
      Left _ -> []
      Right value -> pure (value, rest0)

instance PersistField TxOutRefPersist where
  toPersistValue = PersistByteString . txOutRefPersistToCBOR
  fromPersistValue (PersistByteString t) = txOutRefPersistFromCBOR t
  fromPersistValue x =
    Left $
      "When trying to deserialize a TxOutRefPersist: expected PersistByteString, received: "
        <> Text.pack (show x)

instance PersistFieldSql TxOutRefPersist where
  sqlType _ = SqlBlob

instance ToJSON TxOutRefPersist where
  toJSON = toJSON . Text.decodeUtf8 . B16.encode . txOutRefPersistToCBOR

instance FromJSON TxOutRefPersist where
  parseJSON = withText "TxOutRefPersist" $ \t -> do
    let bs16 = Text.encodeUtf8 t
    bs <- either fail pure $ B16.decode bs16
    either (fail . Text.unpack) pure $ txOutRefPersistFromCBOR bs

instance ToHttpApiData TxOutRefPersist where
  toUrlPiece = Text.decodeUtf8 . B16.encode . txOutRefPersistToCBOR

instance FromHttpApiData TxOutRefPersist where
  parseUrlPiece t = do
    let bs16 = Text.encodeUtf8 t
    bs <- first Text.pack $ B16.decode bs16
    txOutRefPersistFromCBOR bs

instance PathPiece TxOutRefPersist where
  toPathPiece = Text.decodeUtf8 . B16.encode . txOutRefPersistToCBOR
  fromPathPiece t = do
    let bs16 = Text.encodeUtf8 t
    bs <- either (const Nothing) pure $ B16.decode bs16
    either (const Nothing) pure $ txOutRefPersistFromCBOR bs

newtype RetainedRootMemberIdPersist = RetainedRootMemberIdPersist
  { unRetainedRootMemberIdPersist :: ByteString
  }
  deriving stock (Eq, Ord)

instance Show RetainedRootMemberIdPersist where
  show = show . B16.encode . unRetainedRootMemberIdPersist

instance Read RetainedRootMemberIdPersist where
  readsPrec d input = do
    (bs16, rest0) <- readsPrec d input
    bs <- either (const []) pure $ B16.decode bs16
    pure (RetainedRootMemberIdPersist bs, rest0)

instance PersistField RetainedRootMemberIdPersist where
  toPersistValue = PersistByteString . unRetainedRootMemberIdPersist
  fromPersistValue (PersistByteString t) = Right (RetainedRootMemberIdPersist t)
  fromPersistValue x =
    Left $
      "When trying to deserialize a RetainedRootMemberIdPersist: expected PersistByteString, received: "
        <> Text.pack (show x)

instance PersistFieldSql RetainedRootMemberIdPersist where
  sqlType _ = SqlBlob

instance ToJSON RetainedRootMemberIdPersist where
  toJSON = toJSON . Text.decodeUtf8 . B16.encode . unRetainedRootMemberIdPersist

instance FromJSON RetainedRootMemberIdPersist where
  parseJSON = withText "RetainedRootMemberIdPersist" $ \t -> do
    let bs16 = Text.encodeUtf8 t
    bs <- either fail pure $ B16.decode bs16
    pure (RetainedRootMemberIdPersist bs)

-- TODO: This should use the Midgard UTxO format, not Cardano UTxO.
newtype TxOutPersist = TxOutPersist {unTxOutPersist :: C.TxOut C.CtxUTxO C.ConwayEra}
  deriving newtype (Eq, Show)

txOutPersistProtVer :: CBOR.Version
txOutPersistProtVer = CBOR.shelleyProtVer

instance PersistField TxOutPersist where
  toPersistValue (TxOutPersist txOut) = PersistByteString $ txOutCBOR txOut
  fromPersistValue (PersistByteString t) =
    bimap (Text.pack . show) TxOutPersist $
      C.fromShelleyTxOut C.shelleyBasedEra <$> CBOR.decodeFull' txOutPersistProtVer t
  fromPersistValue x =
    Left $
      "When trying to deserialize a TxOutPersist: expected PersistByteString, received: "
        <> Text.pack (show x)

instance PersistFieldSql TxOutPersist where
  sqlType _ = SqlBlob

txOutCBOR :: C.TxOut C.CtxUTxO C.ConwayEra -> ByteString
txOutCBOR =
  CBOR.serialize' txOutPersistProtVer
    . C.toShelleyTxOut C.shelleyBasedEra

-- | Helper to derive PersistField for bytestring wrappers that use their serialised representations.
newtype PersistUsingRawBytes a = PersistUsingRawBytes a

instance (C.SerialiseAsRawBytes a) => PersistField (PersistUsingRawBytes a) where
  toPersistValue (PersistUsingRawBytes txId) = PersistByteString $ C.serialiseToRawBytes txId
  fromPersistValue (PersistByteString t) =
    bimap (Text.pack . show) PersistUsingRawBytes $
      C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @a) t
  fromPersistValue x =
    Left $
      "When trying to deserialize a "
        <> Text.pack (show . typeRep $ Proxy @a)
        <> ": expected PersistByteString, received: "
        <> Text.pack (show x)

instance (C.SerialiseAsRawBytes a) => PersistFieldSql (PersistUsingRawBytes a) where
  sqlType _ = SqlBlob
