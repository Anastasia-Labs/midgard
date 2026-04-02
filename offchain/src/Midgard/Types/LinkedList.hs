{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.LinkedList (
  NodeKey (..),
  Element (..),
  ElementData (..),
  nodeKeyToAssetName,
  nodeKey,
  getNodeKey,
  nodeKeyFromAssetName,
  nodeKeyFromAssetName',
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import PlutusLedgerApi.Common (
  BuiltinByteString,
  FromData,
  ToData,
  UnsafeFromData,
  fromBuiltin,
  toBuiltin,
 )
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Blueprint (
  HasBlueprintDefinition (Unroll),
  HasBlueprintSchema (schema),
  HasSchemaDefinition,
  Schema (SchemaBytes),
  SchemaInfo (title),
  UnrollAll,
  emptyBytesSchema,
  emptySchemaInfo,
 )

import Ply (PlyArg)

newtype NodeKey = NodeKey BuiltinByteString
  deriving stock (Show)
  deriving newtype (ToData, FromData, UnsafeFromData)

nodeKey :: ByteString -> NodeKey
nodeKey = NodeKey . toBuiltin

getNodeKey :: NodeKey -> ByteString
getNodeKey (NodeKey bbs) = fromBuiltin bbs

-- | Produce a linked list asset name by prepending the prefix.
nodeKeyToAssetName :: ByteString -> NodeKey -> C.AssetName
nodeKeyToAssetName prefix (NodeKey plutusBs) = C.UnsafeAssetName $ prefix <> fromBuiltin plutusBs

-- | Obtain the key from a linked list asset name by dropping the prefix.
nodeKeyFromAssetName :: Int -> C.AssetName -> NodeKey
nodeKeyFromAssetName prefixLen = NodeKey . toBuiltin . nodeKeyFromAssetName' prefixLen

-- | Obtain the key from a linked list asset name by dropping the prefix (in raw bytestring form).
nodeKeyFromAssetName' :: Int -> C.AssetName -> ByteString
nodeKeyFromAssetName' prefixLen (C.UnsafeAssetName assetName) = BS.drop prefixLen assetName

instance HasBlueprintSchema NodeKey referenedTypes where
  schema = SchemaBytes emptySchemaInfo {title = Just "NodeKey"} emptyBytesSchema

instance HasBlueprintDefinition NodeKey where
  type Unroll NodeKey = '[NodeKey]

instance PlyArg NodeKey

data ElementData rootData nodeData = Root rootData | Node nodeData
  deriving stock (Eq, Show, Generic)

$( makeIsDataIndexed
     ''ElementData
     [ ('Root, 0)
     , ('Node, 1)
     ]
 )

instance (Typeable rootData, Typeable nodeData) => HasBlueprintDefinition (ElementData rootData nodeData) where
  type Unroll (ElementData rootData nodeData) = ElementData rootData nodeData : UnrollAll '[rootData, nodeData]

instance
  ( HasSchemaDefinition rootData referencedTypes
  , HasSchemaDefinition nodeData referencedTypes
  , HasBlueprintDefinition rootData
  , HasBlueprintDefinition nodeData
  ) =>
  HasBlueprintSchema (ElementData rootData nodeData) referencedTypes
  where
  schema = schema @(Either rootData nodeData)

data Element rootData nodeData = Element
  { elementData :: ElementData rootData nodeData
  , elementLink :: Maybe NodeKey
  }
  deriving stock (Generic, Show)

$(makeIsDataIndexed ''Element [('Element, 0)])

instance (Typeable rootData, Typeable nodeData) => HasBlueprintDefinition (Element rootData nodeData) where
  type Unroll (Element rootData nodeData) = Element rootData nodeData : UnrollAll '[ElementData rootData nodeData, Maybe NodeKey, NodeKey]

instance
  ( Typeable rootData
  , Typeable nodeData
  , HasSchemaDefinition (ElementData rootData nodeData) referencedTypes
  , HasSchemaDefinition (Maybe NodeKey) referencedTypes
  ) =>
  HasBlueprintSchema (Element rootData nodeData) referencedTypes
  where
  schema = schema @(ElementData rootData nodeData, Maybe NodeKey)
