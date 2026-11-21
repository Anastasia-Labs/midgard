use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxId}

/// The thread carries the mis-tagged transaction's id and the validity scalar
/// its committed `transactions_root` leaf actually embeds. Both values are
/// read off the authenticated leaf view at step-01 — never off anything the
/// prover carried loose — so by the time they reach this step they are as
/// trustworthy as the header commitment.
///
/// The scalar is forwarded rather than merely asserted at step-01 so the
/// decisive soundness check (`!= 0`, i.e. `== 1` under the codec's `{0, 1}`
/// domain) sits on the finalize path, where the permanent fraud-proof token is
/// minted (l2-tx-mistag plan §3.1, D5).
pub type State {
  bad_tx_id: MidgardTxId,
  committed_validity_code: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
