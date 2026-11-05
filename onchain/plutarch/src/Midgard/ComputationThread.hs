{- |
Module      : Midgard.ComputationThread
Description : Partial Plutarch port of @lib/midgard/computation-thread.ak@.

The whole module: the mint redeemer that 'Midgard.Validators.FraudProof' reads
to learn a proof ran to completion, plus the per-step datum and redeemer that
each fraud-proof step carries.
-}
module Midgard.ComputationThread (
  PMintRedeemer (..),
  PStepDatum (..),
  PStepRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PPubKeyHash, PScriptHash, PTokenName)
import Plutarch.Prelude

import Midgard.MpfProof.Types (PProof)

{- | Aiken @computation_thread.MintRedeemer@.

@
Init { first_step_output_index, fraud_category_id, fraud_category,
       fraud_category_membership_proof, fraud_proof_catalogue_ref_input_index,
       inclusion_proof_script_redeemer_index, hub_oracle_ref_input_index,
       fraudulent_block_ref_input_index }
Success { burning_token_asset_name }
BurnForCancellation { burning_token_asset_name }
@

Constructor order fixes the on-chain tag: @Init@ is 0, @Success@ 1,
@BurnForCancellation@ 2. 'Midgard.Validators.FraudProof' matches on @Success@,
so getting this order wrong would silently make it accept a cancellation.
-}
data PMintRedeemer (s :: S)
  = PInit
      { pctInit'firstStepOutputIndex :: Term s (PAsData PInteger)
      , pctInit'fraudCategoryId :: Term s (PAsData PByteString)
      , pctInit'fraudCategory :: Term s (PAsData PScriptHash)
      , pctInit'fraudCategoryMembershipProof :: Term s (PAsData PProof)
      , pctInit'fraudProofCatalogueRefInputIndex :: Term s (PAsData PInteger)
      , pctInit'inclusionProofScriptRedeemerIndex :: Term s (PAsData PInteger)
      , pctInit'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pctInit'fraudulentBlockRefInputIndex :: Term s (PAsData PInteger)
      }
  | PSuccess {pctSuccess'burningTokenAssetName :: Term s (PAsData PTokenName)}
  | PBurnForCancellation
      {pctBurnForCancellation'burningTokenAssetName :: Term s (PAsData PTokenName)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @computation_thread.StepDatum<a>@.

@fraud_prover@ is the key that will be paid the reward if the proof completes,
and it is fixed when the thread is created: the @Init@ branch requires the
transaction to be signed by it, so a thread cannot be opened on someone else's
behalf and then claimed.

@data@ is the step's own working state — 'PDNothing' at the first step, since
nothing has been computed yet. Aiken parameterises it; the payload stays raw
'PData' here because each fraud category gives it a different shape and this
module never looks inside.
-}
data PStepDatum (s :: S) = PStepDatum
  { pstep'fraudProver :: Term s (PAsData PPubKeyHash)
  , pstep'data :: Term s (PMaybeData PData)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStepDatum)

{- | Aiken @computation_thread.StepRedeemer<a>@.

@Cancel@ is 0 and @Continue@ 1. Cancelling abandons a thread and burns its
token; continuing carries the step's payload forward, which again stays raw
'PData' because it is the fraud category's business rather than this module's.
-}
data PStepRedeemer (s :: S)
  = PCancel
      { pcancel'inputIndex :: Term s (PAsData PInteger)
      , pcancel'computationThreadMintRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PContinue {pcontinue'data :: Term s PData}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStepRedeemer)
