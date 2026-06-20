{-# LANGUAGE OverloadedStrings #-}

-- | Helper types to conform to JSON.
module Midgard.Node.Server.JSON.Types (
  NaturalJSON (..),
  TxOutJSON (..),
  TxJSON (..),
  TxOutRefJSON (..),
  txOutCBOR,
  txOutRefCBOR,
) where

import Cardano.Api qualified as C
import Cardano.Api.Parser.Text ((<?>))
import Cardano.Api.Parser.Text qualified as P
import Cardano.Ledger.Binary qualified as CBOR
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, FromJSONKey (fromJSONKey), FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON (toEncoding, toJSON), ToJSONKey (toJSONKey, toJSONKeyList), withText)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Functor (($>))
import Data.Functor.Contravariant ((>$<))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Text.Read (readMaybe)

-- | Like 'Natural', but encodes to string since JSON numbers cannot support large naturals.
newtype NaturalJSON = NaturalJSON Natural
  deriving newtype (Eq, Ord, Show)

instance FromJSON NaturalJSON where
  parseJSON =
    withText "Natural" $
      maybe
        (fail "Not a natural number")
        (pure . NaturalJSON)
        . readMaybe
        . Text.unpack

instance ToJSON NaturalJSON where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

-- | Tx CBOR expressed as a hex string.
newtype TxJSON = TxJSON {unTxJSON :: C.Tx C.ConwayEra}
  deriving newtype (Eq, Show)

instance ToJSON TxJSON where
  toJSON =
    toJSON
      . BS8.unpack
      . BS16.encode
      . C.serialiseToCBOR
      . unTxJSON

-- | TxIn CBOR expressed as a hex string.
newtype TxOutRefJSON = TxOutRefJSON {unTxOutRefJSON :: C.TxIn}
  deriving newtype (Eq, Show)

-- TODO: Make sure shelleyProtVer is the right version here.
-- Cross-check with the typescript serialization.
txOutRefCBOR :: TxOutRefJSON -> BS8.ByteString
txOutRefCBOR = CBOR.serialize' CBOR.shelleyProtVer . C.toShelleyTxIn . unTxOutRefJSON

instance ToJSON TxOutRefJSON where
  toJSON =
    toJSON
      . BS8.unpack
      . BS16.encode
      . txOutRefCBOR
  toEncoding =
    toEncoding
      . BS8.unpack
      . BS16.encode
      . txOutRefCBOR

-- | Transaction output CBOR expressed as a hex string.
newtype TxOutJSON = TxOutJSON
  { unTxOutJSON :: C.TxOut C.CtxUTxO C.ConwayEra
  }
  deriving newtype (Eq, Show)

-- TODO: Make sure shelleyProtVer is the right version here.
-- Cross-check with the typescript serialization.
txOutCBOR :: TxOutJSON -> BS8.ByteString
txOutCBOR =
  CBOR.serialize' CBOR.shelleyProtVer
    . C.toShelleyTxOut C.shelleyBasedEra
    . unTxOutJSON

instance ToJSON TxOutJSON where
  toJSON =
    toJSON
      . BS8.unpack
      . BS16.encode
      . txOutCBOR
  toEncoding =
    toEncoding
      . BS8.unpack
      . BS16.encode
      . txOutCBOR

-- | A UTxO expressed as a JSON record with its key properties.
data UTxOJSON = UTxOJSON
  { txHash :: C.TxId
  , outputIndex :: Natural
  , address :: C.Address C.ShelleyAddr
  , assets :: Map AssetJSON NaturalJSON
  , datum :: Maybe DatumJSON
  , datumHash :: Maybe (C.Hash C.ScriptData)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | PolicyID and AssetName both encoded in hex and concatenated without any separators.
newtype AssetJSON = AssetJSON {unAssetJSON :: C.AssetId}
  deriving stock (Eq, Ord, Show)

{- | Similar to: https://github.com/IntersectMBO/cardano-api/blob/34238163cd2497d781cf793a78f0c5f1d41e18f8/cardano-api/src/Cardano/Api/Value/Internal.hs#L222-L249
But no separator between policy id and asset name.
-}
parseAssetId :: P.Parser C.AssetId
parseAssetId =
  P.try parseAdaAssetId
    <|> parseNonAdaAssetId
      <?> "asset ID"
  where
    -- Parse the ADA asset ID.
    parseAdaAssetId :: P.Parser C.AssetId
    parseAdaAssetId = P.string "lovelace" $> C.AdaAssetId

    -- Parse a multi-asset ID.
    parseNonAdaAssetId :: P.Parser C.AssetId
    parseNonAdaAssetId = do
      polId <- C.parsePolicyId
      parseFullAssetId polId <|> parseAssetIdNoAssetName polId

    -- Parse a fully specified multi-asset ID with both a policy ID and asset
    -- name.
    parseFullAssetId :: C.PolicyId -> P.Parser C.AssetId
    parseFullAssetId polId = do
      aName <- C.parseAssetName <?> "hexadecimal asset name"
      pure (C.AssetId polId aName)

    -- Parse a multi-asset ID that specifies a policy ID, but no asset name.
    parseAssetIdNoAssetName :: C.PolicyId -> P.Parser C.AssetId
    parseAssetIdNoAssetName polId = pure $ C.AssetId polId (C.UnsafeAssetName "")

-- | Similar to the official renderAssetId in cardano-api but does not emit a separator.
renderAssetIdNoSep :: C.AssetId -> Text
renderAssetIdNoSep C.AdaAssetId = "lovelace"
renderAssetIdNoSep (C.AssetId (C.PolicyId scriptHash) assetName) =
  C.serialiseToRawBytesHexText scriptHash <> C.serialiseToRawBytesHexText assetName

instance FromJSON AssetJSON where
  parseJSON = withText "AssetJSON" $ fmap AssetJSON . P.runParserFail parseAssetId

instance FromJSONKey AssetJSON where
  fromJSONKey = FromJSONKeyTextParser $ fmap AssetJSON . P.runParserFail parseAssetId

instance ToJSONKey AssetJSON where
  toJSONKey = (renderAssetIdNoSep . unAssetJSON) >$< toJSONKey
  toJSONKeyList = map (renderAssetIdNoSep . unAssetJSON) >$< toJSONKeyList

-- | Datum represented as CBOR hex in JSON.
newtype DatumJSON = DatumJSON {unDatumJSON :: C.ScriptData}
  deriving newtype (Eq, Show)

instance FromJSON DatumJSON where
  parseJSON = withText "DatumJSON" $ \raw -> do
    hexStr <- either fail pure . BS16.decode . BS8.pack $ Text.unpack raw
    either (fail . show) (pure . DatumJSON) $ C.deserialiseFromCBOR C.AsScriptData hexStr

instance ToJSON DatumJSON where
  toJSON =
    toJSON
      . BS8.unpack
      . BS16.encode
      . C.serialiseToCBOR
      . unDatumJSON
  toEncoding =
    toEncoding
      . BS8.unpack
      . BS16.encode
      . C.serialiseToCBOR
      . unDatumJSON
