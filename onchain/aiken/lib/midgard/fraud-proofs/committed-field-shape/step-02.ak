use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxId}

/// The accusation, as step-01 derived it and step-02 adjudicates it.
///
/// `(bad_tx_id, field_index)` is §12.1's **fault address** — the transaction and
/// the slot the claim names — and `verdict` is the proof: slot `field_index`'s
/// §7.4/§5.4 shape verdict over the bytes that slot committed, taken by step-01
/// after the §8.8 door hashed them against §4's positionally-extracted
/// commitment. Every member is derived from authenticated data rather than
/// accepted from the prover, so step-02 adjudicates committed values only.
///
/// **Structurally identical to §12.7's step-02 state, and deliberately not the
/// same type.** The two verdict code spaces are different — §12.7's runs 0..10
/// over §5.1's grammar, this one 0..3 over slot `i`'s shape — so one type would
/// let a §12.7 code satisfy this family's bounds check and mean something else
/// while doing it. The families are held apart by the script hashes their two
/// step-01s pin and by these two types, not by convention.
///
/// **No preimage bytes travel.** §12.1's witness minimality holds here for the
/// same reason it holds for a fault statement: the bytes were authenticated in
/// the transaction that read them, the verdict is what they proved, and a second
/// transaction re-supplying them would be re-supplying a field the first one
/// already paid for. A prover that fabricates a verdict fails at step-01, where
/// the output state is recomputed rather than read.
pub type State {
  bad_tx_id: MidgardTxId,
  field_index: Int,
  verdict: Int,
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
