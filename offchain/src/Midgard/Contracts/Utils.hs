module Midgard.Contracts.Utils (
  LinkedListInfo (..),
  nextOutIx,
  findOutputIndexWithAsset,
  findUtxoWithAsset,
  inlineDatumFromUTxO,
  findTxInNonMembership,
  findUtxoWithLink,
  pubKeyHashFromCardano,
) where

import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT)
import Data.ByteString (ByteString)
import Data.List (find, findIndex)
import Data.Map.Strict qualified as Map
import GHC.IsList (toList)

import Cardano.Api qualified as C
import Control.Lens (
  view,
  _2,
 )
import Convex.CardanoApi.Lenses qualified as L
import Convex.Scripts (fromHashableScriptData)
import Convex.Utxos (UtxoSet (UtxoSet), partition, selectUtxo)
import PlutusLedgerApi.Common (BuiltinData, FromData, toBuiltin)
import PlutusLedgerApi.Data.V3 (PubKeyHash (PubKeyHash))

import Midgard.Types.LinkedList (nodeKeyToAssetName)
import Midgard.Types.LinkedList qualified as LinkedList

-- | Index of the next output to be added into the tx.
nextOutIx :: C.TxBodyContent v era -> Int
nextOutIx = length . view L.txOuts

-- | Find the index of the first tx output that contains the given asset.
findOutputIndexWithAsset :: C.PolicyId -> C.AssetName -> C.TxBodyContent C.BuildTx era -> Int
findOutputIndexWithAsset policyId assetName txBody =
  case findIndex txOutHasAsset $ view L.txOuts txBody of
    Just ix -> ix
    Nothing -> error "missing required output asset"
  where
    txOutHasAsset (C.TxOut _ txOutValue _ _) =
      C.selectAsset (C.txOutValueToValue txOutValue) (C.AssetId policyId assetName) > 0

-- | Find a utxo in the utxo set that contains the given asset.
findUtxoWithAsset :: UtxoSet ctx a -> C.AssetId -> Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
findUtxoWithAsset utxoSet asset =
  let txOutHasToken :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool
      txOutHasToken (C.InAnyCardanoEra _ txOut, _) =
        (> 1) $
          flip C.selectAsset asset $
            C.txOutValueToValue $
              view (L._TxOut . _2) txOut
   in selectUtxo . fst $ partition txOutHasToken utxoSet

-- | Parse a linked list datum from an inline datum field.
inlineDatumFromUTxO :: (FromData a) => C.TxOut ctx era -> Maybe a
inlineDatumFromUTxO (C.TxOut _ _ rootDatum _) = case rootDatum of
  C.TxOutDatumInline _ d -> fromHashableScriptData d
  _ -> Nothing

-- | Global info about a particular UTxO linked list structure. Required for computing certain operations.
data LinkedListInfo = LinkedListInfo
  { ownerPolicyId :: C.PolicyId
  , rootAssetName :: C.AssetName
  -- ^ The asset name contained within the root node.
  , nodeAssetNamePrefix :: ByteString
  }

{- | From the given UTxO set, representing an ordered linked list structure, find the "boundary" UTxO that
proves non-membership of given key (in the form of asset name).
i.e Find the linked-list utxo who's datum contains a key that is _smaller_ than the given key,
but its link (next utxo) contains a key that is _larger_ than the given key, thereby proving
that no utxo with given key exists in the linked list.
The selected UTxO must also contain a token with the given policy ID.
-}
findTxInNonMembership :: UtxoSet ctx a -> C.AssetName -> ReaderT LinkedListInfo Maybe C.TxIn
findTxInNonMembership (UtxoSet utxoMap) assetToAdd = do
  LinkedListInfo {ownerPolicyId, rootAssetName, nodeAssetNamePrefix} <- ask
  let targetUtxos = Map.toList utxoMap
  fmap fst . lift . flip find targetUtxos $ \(_, (C.InAnyCardanoEra _ utxo, _)) ->
    case inlineDatumFromUTxO @(LinkedList.Element BuiltinData BuiltinData) utxo of
      -- Not a valid UTxO
      Nothing -> False
      Just LinkedList.Element {elementLink} ->
        let thisAssetNameM = listAssetFromUTxO ownerPolicyId utxo
            linkAssetNameM = nodeKeyToAssetName nodeAssetNamePrefix <$> elementLink
         in case (thisAssetNameM, linkAssetNameM) of
              -- Not a valid UTxO if it doesn't have its own key.
              (Nothing, _) -> False
              (Just thisAssetName, linkAssetNameM)
                | thisAssetName == rootAssetName -> isLarger linkAssetNameM
                | otherwise -> thisAssetName < assetToAdd && isLarger linkAssetNameM
  where
    -- \| Whether or not the link is "larger" (i.e beyond)
    isLarger Nothing = True
    isLarger (Just linkKey) = linkKey > assetToAdd

    -- \| Find the linked list node asset name contained within the value.
    listAssetFromUTxO :: C.PolicyId -> C.TxOut ctx era -> Maybe C.AssetName
    listAssetFromUTxO policyId (C.TxOut _ txOutValue _ _) = do
      -- Lookup any asset(s) for the owner policy.
      nodeAssets <- Map.lookup policyId . C.valueToPolicyAssets $ C.txOutValueToValue txOutValue
      -- There should be only one asset, yield its name.
      case toList nodeAssets of
        [(nodeAssets, _)] -> pure nodeAssets
        _ -> error "absurd: UTxO contains multiple assets under owner policy"

{- | From the given UTxO set, representing an ordered linked list structure, find the "anchor" UTxO that
links to the given key.
The selected UTxO must also contain a token with the given policy ID.
-}
findUtxoWithLink :: UtxoSet ctx a -> C.PolicyId -> ByteString -> Maybe (C.TxIn, C.InAnyCardanoEra (C.TxOut ctx))
findUtxoWithLink utxoSet policyId key = error "TODO: Implement once datum structures are finalized"

pubKeyHashFromCardano :: C.Hash C.PaymentKey -> PubKeyHash
pubKeyHashFromCardano = PubKeyHash . toBuiltin . C.serialiseToRawBytes
