use aiken/crypto.{VerificationKey}
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardAddressWitness}
use midgard/ledger_state.{AddressWitsHash}

pub type State {
  missing_required_signer_vkey: VerificationKey,
  verified_addr_tx_wits_hash: AddressWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// Canonical native address-witness collection preimage.
  addr_tx_wits_preimage: List<MidgardAddressWitness>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
