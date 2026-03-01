{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.LinkedList (NodeKey (..), Element (..), ElementData (..)) where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import PlutusLedgerApi.Common (
  BuiltinByteString,
  FromData,
  ToData,
  UnsafeFromData,
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
  deriving newtype (ToData, FromData, UnsafeFromData)

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
     , ('Node, 0)
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
  deriving stock (Generic)

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
