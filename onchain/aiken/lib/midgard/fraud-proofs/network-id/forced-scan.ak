//// Q35 `network-id`, forced (wrongful-rejection) direction: the resumable
//// §10 walk that proves **no** output of the rejected forced transaction
//// names a foreign network.
////
//// The forced leaf's verdict is `ForcedTxInvalid { reason: NetworkIdMismatch }`,
//// so conviction is a *negation*: the optional body network field agrees with
//// the deployment and every output address does too. The body half is decided
//// once, at the forced door, which refuses an honest mismatch outright. The
//// output half is what this thread state tracks, because a forced
//// transaction's `outputs` field reaches the raw-carriage bound (15,148 bytes,
//// ~352 minimal outputs) and tier-3 certified carriage goes beyond it — far
//// past what one transaction can scan against the 20% memory reserve. A
//// single-transaction scan would leave those shapes unprovable, which is an
//// operator escape hatch: reject any large forced transaction as
//// `NetworkIdMismatch` and never be convicted.

use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// Everything the scan trusts, written once by the forced door from the
/// authenticated forced leaf and then carried unchanged through every batch.
///
/// Nothing here is ever re-derived by this validator: `bad_tx_id` anchors each
/// re-opening of the outputs field, `committed_tx_network_id` and
/// `expected_network_id` are the values the terminal step's state must carry,
/// and `forced_source_key` is the authenticated forced-root key that fixes the
/// wrongful-rejection polarity downstream.
pub type BoundV1 {
  /// The disputed forced transaction, as the block's counted
  /// `forced_transactions_root` committed it.
  bad_tx_id: MidgardTxId,
  /// The body's optional network field, read off the verified compact
  /// structure by the forced door. `255` spells Cardano's absent field.
  committed_tx_network_id: Int,
  /// The deployment network. A `forced_step` parameter, never a redeemer
  /// value.
  expected_network_id: Int,
  /// `cbor.serialise(membership.key)` of the authenticated forced-root leaf.
  forced_source_key: ByteArray,
}

/// Thread state. Constructor order is wire format.
///
/// `Ready` is written by the forced door; `Open` (tiers 1–2) or the
/// `StartGrammar`/`ResumeGrammar`/`FinishGrammar` sequence (tier 3, §10.7)
/// turns it into `Scanning`, and `Advance` batches from there until the walk
/// completes and hands the thread to step 02.
pub type State {
  /// Constr 0. Nothing opened yet.
  Ready { bound: BoundV1 }
  /// Constr 1. Tier-3 envelope certification in progress; `checkpoint_hash`
  /// is a `field_grammar_checkpoint_hash`.
  Grammar { bound: BoundV1, checkpoint_hash: ByteArray }
  /// Constr 2. Semantic walk in progress; `checkpoint_hash` is a
  /// `field_walk_checkpoint_hash`.
  Scanning { bound: BoundV1, checkpoint_hash: ByteArray }
}

/// Redeemer actions. Constructor order is wire format.
pub type ActionV1 {
  /// Constr 0. Tiers 1–2 only: pays the whole-view envelope walk once and
  /// derives the semantic walk's item-zero position from it.
  Open { input_index: Int, output_index: Int, opening: FieldOpeningV1 }
  /// Constr 1. Tier 3 (or any tier whose whole-view walk is unaffordable):
  /// begin the provisional envelope certification.
  StartGrammar {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    item_budget: Int,
  }
  /// Constr 2. Continue envelope certification from the committed digest.
  ResumeGrammar {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  /// Constr 3. Turn a terminal grammar certificate into the semantic walk's
  /// item-zero position.
  FinishGrammar {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
  }
  /// Constr 4. Fold at most `item_budget` outputs, refusing the moment one
  /// names a foreign network — that is the operator's rejection being honest.
  Advance {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

/// Largest semantic batch a single `Advance` accepts. Each output costs a
/// §5.1 head read, a slice and an address-only decode. Measured marginals
/// (aiken v1.1.23+5adf783): ≈122k memory per output over a whole (tier 1–2)
/// view and ≈205k over a chunked (tier 3) view, so a full batch fits the 20%
/// reserve only under tiers 1–2; the off-chain driver folds 48 per
/// transaction over certified carriage. The cap bounds work, it does not
/// promise a batch fits, and an over-budget batch fails on execution units
/// rather than on this check.
pub const scan_batch: Int = 64

/// Largest grammar batch a single `StartGrammar`/`ResumeGrammar` accepts.
/// Envelope heads only — no payload is interpreted — at ≈125k memory per
/// item over chunked carriage, so the payable batch under the 20% reserve is
/// 64 and the off-chain driver certifies 64 per transaction; the cap is the
/// wire-pinned upper bound, not the batch the driver chooses.
pub const grammar_batch: Int = 128
