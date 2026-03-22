module Midgard.Contracts.Utils (
  LinkedListInfo (..),
  nextOutIx,
  slotToBeginUTCTime,
  slotToEndUTCTime,
  utcTimeToEnclosingSlot,
  findOutputIndexWithAsset,
  findMintRedeemerIndex,
  findUTxOWithAsset,
  inlineDatumFromUTxO,
  findUTxONonMembership,
  findFinalUTxONode,
  findUTxOWithLink,
  listAssetNameFromUTxO,
  pubKeyHashFromCardano,
  findUTxOWithNodeData,
  mintPlutusRefWithRedeemerFinal,
  spendPlutusInlineDatumWithRedeemerFinal,
) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT)
import Data.ByteString (ByteString)
import Data.List (elemIndex, find, findIndex, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import GHC.IsList (toList)

import Cardano.Api qualified as C
import Control.Lens (
  view,
 )
import Convex.BuildTx (MonadBuildTx, addInputWithTxBody, addMintWithTxBody, buildRefScriptWitness, buildScriptWitness)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (queryEraHistory, querySystemStart))
import Convex.Scripts (fromHashableScriptData)
import Convex.Utils qualified as Convex
import Convex.Utxos (UtxoSet (UtxoSet))
import PlutusLedgerApi.Common (BuiltinData, FromData, ToData, fromBuiltin)

import Midgard.Types.LinkedList (NodeKey (NodeKey), nodeKey, nodeKeyToAssetName)
import Midgard.Types.LinkedList qualified as LinkedList

-- | Index of the next output to be added into the tx.
nextOutIx :: C.TxBodyContent v era -> Int
nextOutIx = length . view L.txOuts

-- | Convert slot to its beginning UTC time.
slotToBeginUTCTime :: (MonadError String m, MonadBlockchain era m) => C.SlotNo -> m UTCTime
slotToBeginUTCTime slotNo = do
  eraHistory <- queryEraHistory
  systemStart <- querySystemStart
  either throwError pure $ Convex.slotToUtcTime eraHistory systemStart slotNo

-- | Convert slot to its very last UTC time millisecond.
slotToEndUTCTime :: (MonadError String f, MonadBlockchain era f) => C.SlotNo -> f UTCTime
slotToEndUTCTime slotNo = addUTCTime (-oneMs) <$> slotToBeginUTCTime (slotNo + 1)
  where
    oneMs :: NominalDiffTime
    oneMs = 0.001

-- | Convert UTC time to its enclosing slot.
utcTimeToEnclosingSlot :: (MonadError String m, MonadBlockchain era m) => UTCTime -> m C.SlotNo
utcTimeToEnclosingSlot time = do
  eraHistory <- queryEraHistory
  systemStart <- querySystemStart
  (\(slotNo, _, _) -> slotNo) <$> either throwError pure (Convex.utcTimeToSlot eraHistory systemStart time)

-- | Find the index of the first tx output that contains the given asset.
findOutputIndexWithAsset :: C.PolicyId -> C.AssetName -> C.TxBodyContent C.BuildTx era -> Int
findOutputIndexWithAsset policyId assetName txBody =
  case findIndex txOutHasAsset $ view L.txOuts txBody of
    Just ix -> ix
    Nothing -> error "missing required output asset"
  where
    txOutHasAsset (C.TxOut _ txOutValue _ _) =
      C.selectAsset (C.txOutValueToValue txOutValue) (C.AssetId policyId assetName) > 0

{- | Find the index for minting a policy in the plutus-ledger-api txInfo redeemers assoc list.
This requires knowledge of _all_ policies that will be minted in the transaction.
The given target policyId must exist in the _all_ policies list.
-}
findMintRedeemerIndex :: [C.PolicyId] -> C.TxBodyContent C.BuildTx era -> C.PolicyId -> Int
findMintRedeemerIndex allPolicies txBody policyId = scriptSpendingRedeemers + mintRedeemerIx
  where
    mintRedeemerIx =
      case elemIndex policyId (sort allPolicies) of
        Just ix -> ix
        Nothing -> error "target policy id missing from policy list"
    scriptSpendingRedeemers =
      length . filter hasScriptSpendingWitness $ C.txIns txBody
    hasScriptSpendingWitness :: (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)) -> Bool
    hasScriptSpendingWitness (_, C.BuildTxWith (C.ScriptWitness _ _)) = True
    hasScriptSpendingWitness _ = False

-- | Find a utxo in the utxo set that contains the given asset.
findUTxOWithAsset :: UtxoSet ctx a -> C.AssetId -> Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
findUTxOWithAsset (UtxoSet utxos) asset =
  find
    ( \(_, (C.InAnyCardanoEra _ (C.TxOut _ val _ _), _)) ->
        C.selectAsset
          (C.txOutValueToValue val)
          asset
          >= 1
    )
    $ Map.toList utxos

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
findUTxONonMembership ::
  UtxoSet ctx a ->
  ByteString ->
  ReaderT LinkedListInfo Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
findUTxONonMembership (UtxoSet utxoMap) keyToAdd = do
  LinkedListInfo {ownerPolicyId, rootAssetName, nodeAssetNamePrefix} <- ask
  let assetToAdd = nodeKeyToAssetName nodeAssetNamePrefix $ nodeKey keyToAdd
  let targetUtxos = Map.toList utxoMap
  lift . flip find targetUtxos $ \(_, (C.InAnyCardanoEra _ utxo, _)) ->
    case inlineDatumFromUTxO @(LinkedList.Element BuiltinData BuiltinData) utxo of
      -- Not a valid UTxO
      Nothing -> False
      Just LinkedList.Element {elementLink} ->
        let thisAssetNameM = listAssetNameFromUTxO ownerPolicyId utxo
            linkAssetNameM = nodeKeyToAssetName nodeAssetNamePrefix <$> elementLink
         in case (thisAssetNameM, linkAssetNameM) of
              -- Not a valid UTxO if it doesn't have its own key.
              (Nothing, _) -> False
              (Just thisAssetName, linkAssetNameM)
                | thisAssetName == rootAssetName -> isLarger assetToAdd linkAssetNameM
                | otherwise -> thisAssetName < assetToAdd && isLarger assetToAdd linkAssetNameM
  where
    -- Whether or not the link is "larger" (i.e beyond)
    isLarger _ Nothing = True
    isLarger assetToAdd (Just linkKey) = linkKey > assetToAdd

-- | Find a utxo in the utxo set that contains the given node data.
findUTxOWithNodeData ::
  forall nodeData ctx a.
  (Eq nodeData, FromData nodeData) =>
  UtxoSet ctx a -> nodeData -> ReaderT LinkedListInfo Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
findUTxOWithNodeData (UtxoSet utxoMap) expectedNodeData = do
  LinkedListInfo {ownerPolicyId} <- ask
  let targetUtxos = Map.toList utxoMap
  lift . flip find targetUtxos $ \(_, (C.InAnyCardanoEra _ utxo, _)) ->
    case inlineDatumFromUTxO @(LinkedList.Element BuiltinData nodeData) utxo of
      -- Not a valid UTxO
      Just LinkedList.Element {elementData = LinkedList.Node actualNodeData} ->
        -- Must have a corresponding list asset.
        let isAuthentic = isJust $ listAssetNameFromUTxO ownerPolicyId utxo
         in isAuthentic && actualNodeData == expectedNodeData
      _ -> False

{- | From the given UTxO set, representing an ordered linked list structure, find the "anchor" UTxO that
links to the given key (in asset name form).
The selected UTxO must also contain a token with the given policy ID.
-}
findUTxOWithLink :: UtxoSet ctx a -> ByteString -> ReaderT LinkedListInfo Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
findUTxOWithLink (UtxoSet utxoMap) targetKey = do
  LinkedListInfo {ownerPolicyId} <- ask
  let targetUtxos = Map.toList utxoMap
  lift . flip find targetUtxos $ \(_, (C.InAnyCardanoEra _ utxo, _)) ->
    case inlineDatumFromUTxO @(LinkedList.Element BuiltinData BuiltinData) utxo of
      -- Not a valid UTxO
      Nothing -> False
      Just LinkedList.Element {elementLink} ->
        -- Must have a corresponding list asset.
        let isAuthentic = isJust $ listAssetNameFromUTxO ownerPolicyId utxo
         in isAuthentic && case elementLink of
              Nothing -> False
              Just (NodeKey linkKey) -> fromBuiltin linkKey == targetKey

-- | From the given UTxO set, find the authentic linked-list node with no outgoing link.
findFinalUTxONode :: UtxoSet ctx a -> ReaderT LinkedListInfo Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
findFinalUTxONode (UtxoSet utxoMap) = do
  LinkedListInfo {ownerPolicyId, rootAssetName} <- ask
  let targetUtxos = Map.toList utxoMap
  lift . flip find targetUtxos $ \(_, (C.InAnyCardanoEra _ utxo, _)) ->
    case inlineDatumFromUTxO @(LinkedList.Element BuiltinData BuiltinData) utxo of
      Nothing -> False
      Just LinkedList.Element {elementLink} ->
        case (listAssetNameFromUTxO ownerPolicyId utxo, elementLink) of
          (Just thisAssetName, Nothing) -> thisAssetName /= rootAssetName
          _ -> False

-- Find the linked list node asset name contained within the value.
listAssetNameFromUTxO :: C.PolicyId -> C.TxOut ctx era -> Maybe C.AssetName
listAssetNameFromUTxO policyId (C.TxOut _ txOutValue _ _) = do
  -- Lookup any asset(s) for the owner policy.
  nodeAssets <- Map.lookup policyId . C.valueToPolicyAssets $ C.txOutValueToValue txOutValue
  -- There should be only one asset, yield its name.
  case toList nodeAssets of
    [(nodeAssets, _)] -> pure nodeAssets
    _ -> error "absurd: UTxO contains multiple assets under owner policy"

-- | Like 'addMintWithTxBody' but tailored towards easy usage with ref minting.
mintPlutusRefWithRedeemerFinal ::
  (ToData redeemer, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang, MonadBuildTx era m, C.IsMaryBasedEra era) =>
  C.TxIn ->
  C.PlutusScriptVersion lang ->
  C.PolicyId ->
  C.AssetName ->
  C.Quantity ->
  (C.TxBodyContent C.BuildTx era -> redeemer) ->
  m ()
mintPlutusRefWithRedeemerFinal refIn scriptVersion policyId assetName quantity redeemerF =
  addMintWithTxBody policyId assetName quantity $
    buildRefScriptWitness refIn scriptVersion C.NoScriptDatumForMint . redeemerF

-- | Like 'addInputWithTxBody' but tailored towards easy usage with inline datum script spending.
spendPlutusInlineDatumWithRedeemerFinal ::
  (MonadBuildTx era m, ToData b, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang) =>
  C.PlutusScript lang ->
  C.TxIn ->
  (C.TxBodyContent C.BuildTx era -> b) ->
  m ()
spendPlutusInlineDatumWithRedeemerFinal plutusScript txIn redeemerF =
  addInputWithTxBody txIn $
    C.ScriptWitness C.ScriptWitnessForSpending
      . buildScriptWitness plutusScript C.InlineScriptDatum
      . redeemerF
