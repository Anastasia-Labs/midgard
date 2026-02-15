module Midgard.Contracts.Utils (findUtxoWithAsset, findTxInNonMembership) where

import Data.ByteString (ByteString)

import Cardano.Api qualified as C
import Control.Lens (
  view,
  _2,
 )
import Convex.CardanoApi.Lenses qualified as L
import Convex.Utxos (UtxoSet, partition, selectUtxo)

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

{- | From the given UTxO set, representing a linked list structure, find the "boundary" UTxO that
proves non-membership of given key.
i.e Find the linked-list utxo who's datum contains a key that is _smaller_ than the given key,
but its link (next utxo) contains a key that is _larger_ than the given key, thereby proving
that no utxo with given key exists in the linked list.
The selected UTxO must also contain a token with the given policy ID.
-}
findTxInNonMembership :: UtxoSet ctx a -> C.PolicyId -> ByteString -> Maybe C.TxIn
findTxInNonMembership utxoSet policyId key = error "TODO: Implement once datum structures are finalized"
