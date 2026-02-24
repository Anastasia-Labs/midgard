{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.StateCommitment where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.V3 (
  PPosixTime (..),
  PPubKeyHash (..),
  PRedeemer,
  PScriptHash,
  PTxId,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.MerkleTree.PatriciaForestry (
  MerklePatriciaForestry,
  PMerklePatriciaForestry,
  PProof,
  phas,
 )
import Plutarch.Prelude
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  BuiltinData,
  CurrencySymbol,
  FromData,
  Lovelace,
  MintValue,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  Redeemer,
  ScriptHash,
  ToData,
  TxId,
  TxInInfo,
  TxOut,
  TxOutRef,
  UnsafeFromData,
  Value,
 )
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
  = MintingScript CurrencySymbol
  | SpendingScript ScriptHash TxOutRef
  | ObservingScript ScriptHash
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
  , mtxInfoOutputs :: [TxOut]
  , mtxInfoFee :: Lovelace
  , mtxInfoValidRange :: POSIXTimeRange
  , mtxInfoObservers :: [ScriptHash]
  , mtxInfoSignatories :: [PubKeyHash]
  , mtxInfoMint :: MintValue
  , mtxInfoScriptIntegrityHash :: BuiltinData
  , mtxInfoAuxDataHash :: BuiltinData
  , mtxInfoId :: TxId
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
  , pmidguardTxInfo'validRange :: Term s (PAsData (Interval.PInterval PPosixTime))
  , -- Script hashes of all observer scripts executing in the transaction
    pmidguardTxInfo'observers :: Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
  , pmidguardTxInfo'signatories :: Term s (PAsData (PBuiltinList (PAsData PPubKeyHash)))
  , -- value minted by the transaction
    pmidguardTxInfo'mint :: Term s (PAsData (Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees))
  , pmidguardTxInfo'scriptIntegrityHash :: Term s PData
  , pmidguardTxInfo'auxDataHash :: Term s PData
  , pmidguardTxInfo'id :: Term s (PAsData PTxId)
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
  , txTotalCollateral :: Lovelace
  , txReturnCollateral :: TxOut
  , txFee :: Lovelace
  , txValidityLowerBound :: Maybe POSIXTime
  , txValidityUpperBound :: Maybe POSIXTime
  , -- signed tx body
    txRequiredSigners :: [PubKeyHash]
  , txMintValue :: Value
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
  { pkWits :: [(PubKey, Signature, PubKeyHash)]
  , scriptWits :: [(ScriptHash, Redeemer)]
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
