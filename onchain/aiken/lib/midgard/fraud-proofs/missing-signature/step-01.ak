use aiken/crypto.{VerificationKeyHash}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardTxWitnessSetCompact}

pub type Datum =
  ct.StepDatum<Data>

pub type Args =
  TxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
