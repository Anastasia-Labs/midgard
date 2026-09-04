//// ## `value-not-preserved` step-04 — the decisive inequality
////
//// The final step, and therefore the last place the family can refuse. The
//// carried state is a completed conservation fold for one claimed asset of
//// one operator-accepted transaction; the conviction condition is that the
//// fold really ended unbalanced, **in the claimed direction**:
////
////   * ADA claim: `final_delta = Σ_in lovelace − Σ_out lovelace − fee`,
////   * token claim `u`: `final_delta = Σ_in qty_u + mint_u − Σ_out qty_u`,
////
//// and the machine's stage five accepts the transaction only if every such
//// delta is zero. `final_delta != 0` therefore reproduces
//// `reject_value_not_preserved` for the claimed unit — an accepted
//// transaction whose fold ends non-zero is one the machine would have
//// rejected, and a block that accepted it is fraudulent.
////
//// A **balanced** fold (`final_delta == 0`) refuses here even though every
//// earlier step verified: this includes the transaction that never touches
//// the claimed asset (a 0 == 0 fold must not convict), the wrong-asset claim
//// against a transaction unbalanced in a *different* asset, and the
//// minted-and-fully-paid-out token. A direction mismatch also refuses: the
//// claim names one specific fault, and a permanent token must record the
//// fault that was actually proven.

use midgard/computation_thread as ct
use midgard/fraud_proofs/value_not_preserved/step_01.{
  ClaimedAssetDeflated, ClaimedAssetInflated, ClaimedAssetV1,
  ClaimedImbalanceDirectionV1,
}
use midgard/ledger_state.{MidgardTxId}

// ## Thread state

/// The completed fold.
pub type State {
  bad_tx_id: MidgardTxId,
  claimed_asset: ClaimedAssetV1,
  claimed_direction: ClaimedImbalanceDirectionV1,
  /// `inflow − outflow` of the claimed asset over the whole transaction.
  final_delta: Int,
}

pub type Datum =
  ct.StepDatum<State>

/// The conviction condition: the completed fold is unbalanced in the claimed
/// direction. `Inflated` is outputs-plus-fee exceeding sources
/// (`final_delta < 0` — value created from nothing); `Deflated` is sources
/// exceeding outputs (`final_delta > 0` — value destroyed). `final_delta == 0`
/// satisfies neither arm, so a balanced transaction is never convictable.
pub fn value_not_preserved_fault_is_established_v1(state: State) -> Bool {
  when state.claimed_direction is {
    ClaimedAssetInflated -> state.final_delta < 0
    ClaimedAssetDeflated -> state.final_delta > 0
  }
}

// ## Step-04 redeemer

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
