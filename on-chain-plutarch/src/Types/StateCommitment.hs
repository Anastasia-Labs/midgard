{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module Types.StateCommitment where 

import Plutarch.Prelude
import Plutarch.DataRepr (DerivePConstantViaData(DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import qualified PlutusTx
import MerkleTree.MerklePatriciaForestry (PMerklePatriciaForestry, MerklePatriciaForestry)
import PlutusLedgerApi.V2 (POSIXTime, BuiltinByteString, ScriptPurpose)
import Plutarch.LedgerApi.V3 (
  PScriptHash,
  PRedeemer,
  PDatum,
  PPubKeyHash(..),
  PPosixTime(..),
  PScriptPurpose,
  PTxId, 
  PTxOutRef, 
  PTxOut, 
  PDatumHash,
  PTxInInfo
  )
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Builtin (pserialiseData, pforgetData)
import Plutarch.Prelude
import MerkleTree.MerklePatriciaForestry
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 (TxInInfo)
import PlutusLedgerApi.V3.Tx qualified as V3
import PlutusTx.AssocMap (Map)

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
      ( Term
          s
          ( PDataRecord
              '[ "txRoot" ':= PMerklePatriciaForestry
               , "depositEventRoot" ':= PMerklePatriciaForestry
               , "withdrawalEventRoot" ':= PMerklePatriciaForestry
               , "startTimestamp" ':= PPosixTime
               , "endTimestamp" ':= PPosixTime
               , "oldUtxoRoot" ':= PMerklePatriciaForestry
               , "newUtxoRoot" ':= PMerklePatriciaForestry
               , "transactionCount" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PBlock where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PBlock where
  type PLifted PBlock = Block

deriving via
  (DerivePConstantViaData Block PBlock)
  instance
    (PConstantDecl Block)

-- StateCommitment type
data StateCommitment = StateCommitment
  { publisher :: BuiltinByteString
  , block :: Block
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''StateCommitment

data PStateCommitment (s :: S)
  = PStateCommitment
      ( Term
          s
          ( PDataRecord
              '[ "publisher" ':= PPubKeyHash
               , "block" ':= PBlock
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PStateCommitment where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PStateCommitment where
  type PLifted PStateCommitment = StateCommitment

deriving via
  (DerivePConstantViaData StateCommitment PStateCommitment)
  instance
    (PConstantDecl StateCommitment)

data MidgardScriptInfo
  = MintingScript V2.CurrencySymbol
  | SpendingScript V2.ScriptHash V2.TxOutRef
  | ObservingScript V2.ScriptHash
  deriving stock (Generic)

PlutusTx.unstableMakeIsData ''MidgardScriptInfo

data PMidgardScriptInfo (s :: S)
  = PMintingScript (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpendingScript (Term s (PDataRecord '["_0" ':= PScriptHash, "_1" ':= PTxOutRef]))
  | PObservingScript (Term s (PDataRecord '["_0" ':= PScriptHash]))
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PEq
    , PShow
    )

instance DerivePlutusType PMidgardScriptInfo where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PMidgardScriptInfo where
  type PLifted _ = MidgardScriptInfo

deriving via
  (DerivePConstantViaData MidgardScriptInfo PMidgardScriptInfo)
  instance
    PConstantDecl MidgardScriptInfo

data MidgardTxInfo = MidgardTxInfo
  { mtxInfoInputs                :: [TxInInfo]
  , mtxInfoReferenceInputs       :: [TxInInfo]
  , mtxInfoOutputs               :: [V2.TxOut]
  , mtxInfoFee                   :: V2.Lovelace
  , mtxInfoMint                  :: V2.Value
  , mtxInfoMintCount             :: Integer 
  , mtxInfoObservers             :: [V2.ScriptHash]
  , mtxInfoValidRange            :: V2.POSIXTimeRange
  , mtxInfoSignatories           :: [V2.PubKeyHash]
  , mtxInfoRedeemers             :: Map ScriptPurpose V2.Redeemer
  , mtxInfoData                  :: Map V2.DatumHash V2.Datum
  , mtxInfoId                    :: V3.TxId
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTxInfo

-- TODO: should we have two transaction formats? txInfo and serializedTx?
newtype PMidgardTxInfo (s :: S)
  = PMidgardTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "referenceInputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut)
               -- the transaction fee
               , "fee" ':= Value.PLovelace
               -- value minted by the transaction
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees 
               -- count of unique tokens minted by the transaction
               , "mintCount" ':= PInteger 
               -- Script hashes of all observer scripts executing in the transaction
               , "observers" ':= PBuiltinList (PAsData PScriptHash)
               , "validRange" ':= Interval.PInterval PPosixTime
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "redeemers" ':= PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) 
               , "data" ':= PBuiltinList (PBuiltinPair (PAsData PDatumHash) (PAsData PDatum))
               -- hash of the pending transaction
               -- TODO:
               -- probably have to change this to script data integrity hash 
               , "id" ':= PTxId
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PDataFields
    , PEq
    )

instance DerivePlutusType PMidgardTxInfo where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PMidgardTxInfo where
  type PLifted _ = MidgardTxInfo

deriving via
  (DerivePConstantViaData MidgardTxInfo PMidgardTxInfo)
  instance
    PConstantDecl MidgardTxInfo

data PMidgardContext (s :: S) = 
  PMidgardContext 
    ( Term
        s
        ( PDataRecord
            '[ "txInfo" ':= PMidgardTxInfo
             , "redeemer" ':= PRedeemer
             , "scriptInfo" ':= PMidgardScriptInfo
             ]
        )
    )
  deriving stock Generic 
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMidgardContext where
  type DPTStrat _ = PlutusTypeData

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
  -- signed tx body
  , txRequiredSigners :: [V2.PubKeyHash]
  , txMintValue :: V2.Value
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTxBodyContent

newtype PubKey = PubKey {getPubKey :: BuiltinByteString}
  deriving stock (Generic, Show)

newtype Signature = Signature {getSignature :: BuiltinByteString}
  deriving stock (Generic, Show)

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

-- | Check if a transaction hash is present in the given transaction root.
--
-- This function verifies whether a given transaction hash exists within the Merkle Patricia Forestry
-- represented by the transaction root. It uses the provided proof to perform the verification.
--
-- @param txroot The Merkle Patricia Forestry root of transactions
-- @param txhash The hash of the transaction to check
-- @param proof The Merkle proof for the transaction
-- @return True if the transaction hash is present, False otherwise
phasTransactionHash :: Term s (PMerklePatriciaForestry :--> PByteString :--> PByteString :--> PProof :--> PBool)
phasTransactionHash = phoistAcyclic $ plam $ \txroot txHash txInfoHash proof -> 
  phas # txroot # txHash # txInfoHash # proof 

-- | Verify if a given hash matches the hash of a MidgardTxInfo.
--
-- This function computes the Blake2b-256 hash of the serialized MidgardTxInfo
-- and compares it with the provided hash.
--
-- @param hash The hash to compare against
-- @param transaction The MidgardTxInfo to hash and compare
-- @return True if the hashes match, False otherwise 
ptxHashMatches :: Term s (PByteString :--> PAsData PMidgardTxInfo :--> PBool)
ptxHashMatches = phoistAcyclic $ plam $ \hash transaction -> 
  hash #== pblake2b_256 # (pserialiseData # (pforgetData transaction))
 
-- TODO
-- The MPF structure of for tx_root should be:
-- key := TxHash 
-- value := TxInfoHash
--
-- This means we need to create functions that convert between MidgardTx and MidgardTxInfo 
-- and utility functions for fraud proofs on these structures.