use midgard/computation_thread as ct
use midgard/ledger_state.{AddressWitsHash, Signers, SignersHash}

pub type State {
  verified_required_signers_hash: SignersHash,
  verified_addr_tx_wits_hash: AddressWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  required_signers_preimage: Signers,
  bad_required_signer_hash_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
