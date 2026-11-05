{- |
Module      : LinkedList.Types
Description : Plutarch port of the type vocabulary of
              @aiken-design-patterns/linked-list.ak@ (v1.7.0).

An on-chain linked list is a set of UTxOs, each authenticated by an NFT under
one policy. The first element is the /root/; every other element is a /node/.
A node's NFT asset name is @node_key_prefix ++ node_key@, the root's is
@root_key@, and each element's datum carries a 'PLink' to its successor.

Most of the Aiken aliases here are transparent (@RootKey = AssetName@,
@NodeKey = ByteArray@, …), so they stay Haskell type synonyms — a newtype would
change nothing on the wire but would force conversions at every use site.
-}
module LinkedList.Types (
  -- * Key and payload aliases
  PRootKey,
  PNodeKey,
  PNodeKeyPrefix,
  PNodeKeyPrefixLength,
  PLovelaceChange,
  PRootData,
  PNodeData,
  PLink,

  -- * Element datums
  PElement (..),
  PElementData (..),
  PGenericElement,
  PGenericElementData,

  -- * Reader aliases
  ElementEval,
  RootEval,
  NodeEval,
  prunElementWith,
  prunRootWith,
  prunNodeWith,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTokenName)
import Plutarch.Prelude

{- | Aiken @RootKey = AssetName@ — the root NFT's asset name.

Must sit outside the node namespace: it must not begin with 'PNodeKeyPrefix',
or root and node identity can collide.
-}
type PRootKey = PTokenName

{- | Aiken @NodeKey = ByteArray@ — a node's key, /without/ the prefix.

Ordered operations compare these as bytestrings, so integer-like keys need a
fixed-width encoding if numeric order is intended: @255 < 256@, but
@0xff > 0x0100@ bytewise.
-}
type PNodeKey = PByteString

-- | Aiken @NodeKeyPrefix = ByteArray@ — the bytes prefixing every node NFT name.
type PNodeKeyPrefix = PByteString

{- | Aiken @NodeKeyPrefixLength = Int@.

Passed alongside the prefix rather than recomputed, to keep the length
computation off-chain. It must equal the prefix's actual length; nothing checks
that, and a wrong value silently mis-slices every node key.
-}
type PNodeKeyPrefixLength = PInteger

{- | Aiken @LovelaceChange = Lovelace@.

Always @continued_output_lovelace - spent_input_lovelace@: positive means the
operation added Ada to the continued element. The library imposes no policy on
this; operation callbacks receive it so contracts can enforce their own.
-}
type PLovelaceChange = PInteger

-- | Aiken @RootData = Data@ — the raw payload of a root datum.
type PRootData = PData

-- | Aiken @NodeData = Data@ — the raw payload of a node datum.
type PNodeData = PData

{- | Aiken @Link = Option<NodeKey>@ — the successor pointer.

'PDNothing' means the element is terminal. The stored key excludes the prefix;
the namespace comes from the operation context rather than being repeated in
every datum.
-}
type PLink = PMaybeData PByteString

{- | Aiken @Element<root_data, node_data>@ — the datum at every list UTxO.

@
pub type Element<root_data, node_data> {
  data: ElementData<root_data, node_data>,
  link: Link,
}
@

A single-constructor record, so @Constr 0@ on the wire — 'DeriveAsDataStruct',
not 'DeriveAsDataRec'.
-}
data PElement (s :: S) = PElement
  { pelement'data :: Term s (PAsData PElementData)
  , pelement'link :: Term s PLink
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PElement)

{- | Aiken @ElementData<root_data, node_data>@.

@
Root { data: root_data }
Node { data: node_data }
@

Constructor order fixes the on-chain tag: @Root@ is 0, @Node@ is 1. Element
authentication branches on this to decide whether to check the asset name
against the root key or against the node prefix, so a swapped order would let a
node authenticate as the root.

The payload stays raw 'PData': this library authenticates the structural role
and hands the application payload back undecoded, exactly as the Aiken
@GenericElement@ alias does.
-}
data PElementData (s :: S)
  = PRoot {pelementData'root :: Term s PData}
  | PNode {pelementData'node :: Term s PData}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PElementData)

-- | Aiken @GenericElement = Element<Data, Data>@.
type PGenericElement = PElement

-- | Aiken @GenericElementData = ElementData<Data, Data>@.
type PGenericElementData = PElementData

{- | Aiken @ElementEval<result>@ — a reader over the full list configuration.

@fn(PolicyId, RootKey, NodeKeyPrefix, NodeKeyPrefixLength) -> result@

Operations that can act on either a root or a node return this shape; the
validator finishes it once, at the end, with its own policy id and namespace
constants. That is the @|> finalize_linked_list(own_policy_id)@ idiom in the
Midgard validators.
-}
type ElementEval (s :: S) (a :: S -> Type) =
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s PNodeKeyPrefix ->
  Term s PNodeKeyPrefixLength ->
  Term s a

-- | Aiken @RootEval<result>@ — for root-only operations, which need no node namespace.
type RootEval (s :: S) (a :: S -> Type) =
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s a

-- | Aiken @NodeEval<result>@ — for node-only operations, which never inspect the root.
type NodeEval (s :: S) (a :: S -> Type) =
  Term s (PAsData PCurrencySymbol) ->
  Term s PNodeKeyPrefix ->
  Term s PNodeKeyPrefixLength ->
  Term s a

-- | Aiken @run_element_with@.
prunElementWith ::
  forall (s :: S) (a :: S -> Type).
  ElementEval s a ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s PNodeKeyPrefix ->
  Term s PNodeKeyPrefixLength ->
  Term s a
prunElementWith reader = reader

-- | Aiken @run_root_with@.
prunRootWith ::
  forall (s :: S) (a :: S -> Type).
  RootEval s a ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s a
prunRootWith reader = reader

-- | Aiken @run_node_with@.
prunNodeWith ::
  forall (s :: S) (a :: S -> Type).
  NodeEval s a ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PNodeKeyPrefix ->
  Term s PNodeKeyPrefixLength ->
  Term s a
prunNodeWith reader = reader
