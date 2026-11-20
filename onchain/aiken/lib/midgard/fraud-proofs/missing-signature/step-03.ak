use aiken/crypto.{VerificationKey, VerificationKeyHash}
use midgard/computation_thread as ct
use midgard/ledger_state.{AddressWitsHash}

pub type State {
  missing_required_signer_hash: VerificationKeyHash,
  verified_addr_tx_wits_hash: AddressWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  missing_required_signer_vkey: VerificationKey,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
