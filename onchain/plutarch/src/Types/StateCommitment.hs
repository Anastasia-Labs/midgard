{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.StateCommitment where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.V3 (
  PDatum,
  PDatumHash,
  PPosixTime (..),
  PPubKeyHash (..),
  PRedeemer,
  PScriptHash,
  PScriptPurpose,
  PTxId,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.MerkleTree.PatriciaForestry
import Plutarch.Prelude
import PlutusLedgerApi.Data.V1 (
  BuiltinByteString,
  FromData,
  POSIXTime,
  ScriptPurpose,
  ToData,
  TxInInfo,
  TxOut,
  TxOutRef,
  UnsafeFromData,
 )
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified

data Block = Block
  { txRoot :: MerklePatriciaForestry
  , depositEventRoot :: MerklePatriciaForestry
  , withdrawalEventRoot :: MerklePatriciaForestry
  , startTimestamp :: POSIXTime
  , endTimestamp :: POSIXTime
  , oldStateRoot :: MerklePatriciaForestry
  , newStateRoot :: MerklePatriciaForestry
  , transactionCount :: Integer
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Block

data PBlock (s :: S)
  = PBlock
  { pblock'txRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pblock'depositEventRoots :: Term s (PAsData PMerklePatriciaForestry)
  , pblock'withdrawalEventRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pblock'startTimestamp :: Term s (PAsData PPosixTime)
  , pblock'endTimestamp :: Term s (PAsData PPosixTime)
  , pblock'oldUtxoRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pblock'newUtxoRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pblock'transactionCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass
    ( SOP.Generic
    , PIsData
    , PEq
    , PShow
    )
  deriving (PlutusType) via (DeriveAsDataStruct PBlock)

deriving via
  DeriveDataPLiftable PBlock Block
  instance
    PLiftable PBlock

-- StateCommitment type
data StateCommitment = StateCommitment
  { publisher :: BuiltinByteString
  , block :: Block
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''StateCommitment

data PStateCommitment (s :: S)
  = PStateCommitment
  { pstateCommitment'publisher :: Term s (PAsData PPubKeyHash)
  , pstateCommitment'block :: Term s (PAsData PBlock)
  }
  deriving stock (Generic)
  deriving anyclass
    ( SOP.Generic
    , PIsData
    , PEq
    , PShow
    )
  deriving (PlutusType) via (DeriveAsDataStruct PStateCommitment)

deriving via
  DeriveDataPLiftable PStateCommitment StateCommitment
  instance
    PLiftable PStateCommitment

data MidgardScriptInfo
  = MintingScript V2.CurrencySymbol
  | SpendingScript V2.ScriptHash V2.TxOutRef
  | ObservingScript V2.ScriptHash
  deriving stock (Generic)

PlutusTx.unstableMakeIsData ''MidgardScriptInfo

data PMidgardScriptInfo (s :: S)
  = PMintingScript (Term s (PAsData Value.PCurrencySymbol))
  | PSpendingScript (Term s (PAsData PScriptHash)) (Term s (PAsData PTxOutRef))
  | PObservingScript (Term s (PAsData PScriptHash))
  deriving stock (Generic)
  deriving anyclass
    ( SOP.Generic
    , PIsData
    , PEq
    , PShow
    )
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardScriptInfo)

deriving via
  DeriveDataPLiftable PMidgardScriptInfo MidgardScriptInfo
  instance
    PLiftable PMidgardScriptInfo

data MidgardTxInfo = MidgardTxInfo
  { mtxInfoInputs :: [TxInInfo]
  , mtxInfoReferenceInputs :: [TxInInfo]
  , mtxInfoOutputs :: [V2.TxOut]
  , mtxInfoFee :: V2.Lovelace
  , mtxInfoMint :: V2.Value
  , mtxInfoMintCount :: Integer
  , mtxInfoObservers :: [V2.ScriptHash]
  , mtxInfoValidRange :: V2.POSIXTimeRange
  , mtxInfoSignatories :: [V2.PubKeyHash]
  , mtxInfoRedeemers :: V2.Map ScriptPurpose V2.Redeemer
  , mtxInfoData :: V2.Map V2.DatumHash V2.Datum
  , mtxInfoId :: V3.TxId
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTxInfo

-- TODO: should we have two transaction formats? txInfo and serializedTx?
data PMidgardTxInfo (s :: S)
  = PMidgardTxInfo
  { pmidguardTxInfo'inputs :: Term s (PAsData (PBuiltinList PTxInInfo))
  , pmidguardTxInfo'referenceInputs :: Term s (PAsData (PBuiltinList PTxInInfo))
  , pmidguardTxInfo'outputs :: Term s (PAsData (PBuiltinList PTxOut))
  , -- the transaction fee
    pmidguardTxInfo'fee :: Term s (PAsData Value.PLovelace)
  , -- value minted by the transaction
    pmidguardTxInfo'mint :: Term s (PAsData (Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees))
  , -- count of unique tokens minted by the transaction
    pmidguardTxInfo'mintCount :: Term s (PAsData PInteger)
  , -- Script hashes of all observer scripts executing in the transaction
    pmidguardTxInfo'observers :: Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
  , pmidguardTxInfo'validRange :: Term s (PAsData (Interval.PInterval PPosixTime))
  , pmidguardTxInfo'signatories :: Term s (PAsData (PBuiltinList (PAsData PPubKeyHash)))
  , pmidguardTxInfo'redeemers :: Term s (PAsData (PBuiltinList (PAsData (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)))))
  , pmidguardTxInfo'data :: Term s (PAsData (PBuiltinList (PAsData (PBuiltinPair (PAsData PDatumHash) (PAsData PDatum)))))
  , -- hash of the pending transaction
    -- TODO:
    -- probably have to change this to script data integrity hash
    pmidguardTxInfo'id :: Term s (PAsData PTxId)
  }
  deriving stock (Generic)
  deriving anyclass
    ( SOP.Generic
    , PIsData
    , PEq
    , PShow
    )
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTxInfo)

deriving via
  DeriveDataPLiftable PMidgardTxInfo MidgardTxInfo
  instance
    PLiftable PMidgardTxInfo

data PMidgardContext (s :: S)
  = PMidgardContext
  { pmidguardContext'txInfo :: Term s PMidgardTxInfo
  , pmidguardContext'redeemer :: Term s PRedeemer
  , pmidguardContext'scriptInfo :: Term s PMidgardScriptInfo
  }
  deriving stock (Generic)
  deriving anyclass
    ( SOP.Generic
    , PEq
    , PShow
    )
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardContext)

data MidgardTxBodyContent = MidgardTxBodyContent
  { txIns :: [TxOutRef]
  , txInsCollateral :: [TxOutRef]
  , txInsReference :: [TxOutRef]
  , txOuts :: [TxOut]
  , txTotalCollateral :: V2.Lovelace
  , txReturnCollateral :: TxOut
  , txFee :: V2.Lovelace
  , txValidityLowerBound :: Maybe V2.POSIXTime
  , txValidityUpperBound :: Maybe V2.POSIXTime
  , -- signed tx body
    txRequiredSigners :: [V2.PubKeyHash]
  , txMintValue :: V2.Value
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTxBodyContent

newtype PubKey = PubKey {getPubKey :: BuiltinByteString}
  deriving stock (Generic, Show)
  deriving newtype (FromData, UnsafeFromData, ToData)

newtype Signature = Signature {getSignature :: BuiltinByteString}
  deriving stock (Generic, Show)
  deriving newtype (FromData, UnsafeFromData, ToData)

data MidgardTxWitness = MidgardTxWitness
  { pkWits :: [(PubKey, Signature, V2.PubKeyHash)]
  , scriptWits :: [(V2.ScriptHash, V2.Redeemer)]
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTxWitness

data MidgardTx = MidgardTx
  { txBody :: MidgardTxBodyContent
  , txWitness :: MidgardTxWitness
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTx

{- | Check if a transaction hash is present in the given transaction root.

This function verifies whether a given transaction hash exists within the Merkle Patricia Forestry
represented by the transaction root. It uses the provided proof to perform the verification.

@param txroot The Merkle Patricia Forestry root of transactions
@param txhash The hash of the transaction to check
@param proof The Merkle proof for the transaction
@return True if the transaction hash is present, False otherwise
-}
phasTransactionHash :: Term s (PAsData PMerklePatriciaForestry :--> PByteString :--> PByteString :--> PProof :--> PBool)
phasTransactionHash = phoistAcyclic $ plam $ \txroot txHash txInfoHash proof ->
  phas # pfromData txroot # txHash # txInfoHash # proof

{- | Verify if a given hash matches the hash of a MidgardTxInfo.

This function computes the Blake2b-256 hash of the serialized MidgardTxInfo
and compares it with the provided hash.

@param hash The hash to compare against
@param transaction The MidgardTxInfo to hash and compare
@return True if the hashes match, False otherwise
-}
ptxHashMatches :: Term s (PByteString :--> PAsData PMidgardTxInfo :--> PBool)
ptxHashMatches = phoistAcyclic $ plam $ \hash transaction ->
  hash #== pblake2b_256 # (pserialiseData # pforgetData transaction)

-- TODO
-- The MPF structure of for tx_root should be:
-- key := TxHash
-- value := TxInfoHash
--
-- This means we need to create functions that convert between MidgardTx and MidgardTxInfo
-- and utility functions for fraud proofs on these structures.
