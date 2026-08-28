//// ## `value-not-preserved` step-02 — spent-input accumulation
////
//// One self-loop iteration per spend input of the challenged transaction,
//// adding the claimed asset's quantity in that input's **pre-state ledger
//// value** to the running delta. The value is never the prover's: it is the
//// `LedgerOutputCommitmentV1` descriptor the header's `prev_utxos_root`
//// commits at the input's out-ref key, fetched through the *same*
//// ledger-membership witness path the transition-trace machinery's
//// `apply_l2_spends` fetches spent-input values through
//// (`midgard/fraud_proofs/transition_trace/proof.verify_ledger_membership`,
//// key `encode_midgard_tx_input(input)`), and the claimed asset's quantity
//// inside the descriptor is read exactly the way the validation machine's
//// ValueAndMint stage two reads it — by walking **all** authenticated asset
//// leaves `0 .. asset_count-1` and summing the ones whose `(policy, name)`
//// is the claimed unit. No ordering or absence assumption is made about the
//// frontier: absence of the claimed unit is established by having seen every
//// leaf, which is the machine's own reading restricted to one unit.
////
//// ### Conviction-safety of pre-state values
////
//// An out-ref's value is immutable: the key is the producing transaction's id
//// plus an output index, ids are content hashes, and an id can enter the
//// ledger at most once — so a key present in `prev_utxos_root` can never be
//// re-inserted in-block with a different value. For an **accepted**
//// transaction the machine resolved each spent input from its running root;
//// whenever this family's fold completes over `prev_utxos_root` witnesses,
//// either those are the very values the machine resolved (an honest running
//// root serves the same descriptor bytes), or the block already committed
//// another fault (a double-spend of an input deleted earlier in-block —
//// accepted-input-set fraud in its own right). In both branches a completed
//// fold that ends unbalanced convicts a block that really is fraudulent, so
//// conviction is sound. What pre-state witnesses do **not** cover is a
//// challenged transaction that spends an output *created earlier in the same
//// block*: such an input has no `prev_utxos_root` membership, the fold cannot
//// complete, and the thread can only be cancelled — a completeness gap
//// (documented in the family plan), never a wrong conviction.
////
//// ### Duplicate spend inputs
////
//// The fold counts each occurrence of an input exactly as a naive walk of
//// field 0 does — twice-listed inputs contribute twice. Input-set uniqueness
//// is a *separate* family's rule; here it suffices that an **accepted**
//// transaction with duplicate inputs is already fraudulent (the machine's
//// input-set stage rejects duplicates), so any conviction reached through a
//// duplicate-bearing fold is a conviction of a genuinely fraudulent block.
//// The dedup-balanced completeness corner belongs to that other family.

use aiken/collection/list
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_types
use midgard/fraud_proofs/transition_trace/proof.{verify_ledger_membership}
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetV1, ClaimedImbalanceDirectionV1, TokenAsset,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_state.{MidgardTxId}
use midgard/validation_merkle_v1.{FrontierPeak}

// ## Thread state

/// The fold's state while spent inputs are being accumulated.
pub type State {
  /// §3 anchor of the challenged transaction — a complete anchor for body
  /// fields 0/2/5, the only fields this family opens.
  bad_tx_id: MidgardTxId,
  /// The claim, frozen at step-01.
  claimed_asset: ClaimedAssetV1,
  claimed_direction: ClaimedImbalanceDirectionV1,
  /// The committed fee, read off the root-committed compact structure at
  /// step-01. Only an ADA claim consumes it (at step-03).
  committed_fee: Int,
  /// The challenged block's pre-state ledger root
  /// (`header.prev_utxos_root`), read off the authenticated header at
  /// step-01. Every spent-input value witness authenticates against it.
  prev_utxos_root: ByteArray,
  /// Number of spend inputs already folded. The fold is strictly in field
  /// order: iteration `k` reads `spend_input_at(view, k)`.
  input_cursor: Int,
  /// Running `inflow − outflow` of the claimed asset. Step-02 only adds
  /// (inputs are inflow); outputs, mint and fee are step-03's.
  claimed_delta: Int,
}

pub type Datum =
  ct.StepDatum<State>

// ## Spent-input value evidence

/// One authenticated asset leaf of a spent input's descriptor, in leaf-index
/// order: opening `i` of a witness is leaf `i` of the descriptor's asset
/// frontier.
pub type AssetLeafOpeningV1 {
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  siblings: List<ByteArray>,
}

/// The pre-state value witness for one spend input.
pub type SpentInputValueWitnessV1 {
  /// The exact `LedgerOutputCommitmentV1` bytes `prev_utxos_root` commits at
  /// the input's out-ref key.
  descriptor_cbor: ByteArray,
  /// MPF membership proof of `(encode_midgard_tx_input(input),
  /// descriptor_cbor)` under `prev_utxos_root`.
  ledger_membership_proof: Proof,
  /// The descriptor's asset frontier peaks. Consumed (and checked against
  /// `asset_frontier_commitment`) only for a token claim.
  asset_peaks: List<FrontierPeak>,
  /// Every asset leaf of the descriptor, in index order — the full walk
  /// `0 .. asset_count-1`, which is what makes a zero contribution an
  /// *established absence* rather than an unproven one. Empty for an ADA
  /// claim, whose value is the descriptor's own `lovelace` field.
  asset_openings: List<AssetLeafOpeningV1>,
}

/// The claimed asset's quantity in one spent input's pre-state ledger value.
///
/// Fails closed unless
///   1. the descriptor bytes are a member of `prev_utxos_root` at the input's
///      out-ref key (the single ledger-membership witness path),
///   2. the bytes decode as a well-formed descriptor (total + canonical
///      re-encode check), and
///   3. for a token claim, the witness opens **every** asset leaf the
///      descriptor commits, each authenticated at its index against the
///      descriptor's `asset_frontier_commitment`.
///
/// The returned quantity is then exactly the machine's stage-two contribution
/// of this input, restricted to the claimed unit.
pub fn spent_input_claimed_quantity_v1(
  prev_utxos_root: ByteArray,
  spend_input: MidgardTxInput,
  witness: SpentInputValueWitnessV1,
  claimed_asset: ClaimedAssetV1,
) -> Int {
  // 1. The descriptor is the committed pre-state value, not the prover's.
  expect
    verify_ledger_membership(
      prev_utxos_root,
      encode_midgard_tx_input(spend_input),
      witness.descriptor_cbor,
      witness.ledger_membership_proof,
    )

  // 2. Total + canonical decoding of the committed bytes.
  let descriptor = ledger_output_commitment_v1.decode(witness.descriptor_cbor)

  when claimed_asset is {
    // 3a. The lovelace value is a scalar field of the descriptor itself; no
    //     leaf walk exists to run. The unread walk arguments must be empty so
    //     nothing rides along unverified.
    AdaAsset -> {
      expect witness.asset_openings == []
      expect witness.asset_peaks == []
      descriptor.lovelace
    }
    // 3b. The full authenticated leaf walk, summing the claimed unit.
    TokenAsset { .. } -> {
      expect list.length(witness.asset_openings) == descriptor.asset_count
      sum_claimed_leaves(
        descriptor,
        claimed_asset,
        witness.asset_peaks,
        witness.asset_openings,
        0,
        0,
      )
    }
  }
}

fn sum_claimed_leaves(
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
  claimed_asset: ClaimedAssetV1,
  peaks: List<FrontierPeak>,
  openings: List<AssetLeafOpeningV1>,
  index: Int,
  accumulated: Int,
) -> Int {
  when openings is {
    [] -> accumulated
    [opening, ..rest] -> {
      // Leaf `index` really is `(policy, name, quantity)`: the leaf hash
      // binds all three, and the index-position binding of
      // `verify_membership` means an opening cannot stand in for another
      // leaf. This is `ledger_output_commitment_v1.verify_asset_membership`,
      // the same authentication the validation machine's stage two applies
      // per leaf.
      expect
        ledger_output_commitment_v1.verify_asset_membership(
          descriptor,
          index,
          opening.policy_id,
          opening.asset_name,
          opening.quantity,
          peaks,
          opening.siblings,
        )
      let contribution = when claimed_asset is {
        AdaAsset -> 0
        TokenAsset { policy_id, asset_name } ->
          if and {
            opening.policy_id == policy_id,
            opening.asset_name == asset_name,
          } {
            opening.quantity
          } else {
            0
          }
      }
      sum_claimed_leaves(
        descriptor,
        claimed_asset,
        peaks,
        rest,
        index + 1,
        accumulated + contribution,
      )
    }
  }
}

// ## Step-02 redeemer

pub type FoldArgs {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for the challenged transaction's field-0
  /// preimage.
  spend_inputs_opening: FieldOpeningV1,
  /// The pre-state value witness for the spend input at `state.input_cursor`.
  value_witness: SpentInputValueWitnessV1,
}

pub type Args {
  /// Fold the next spend input (`state.input_cursor`) and self-loop with the
  /// cursor advanced.
  FoldInput(FoldArgs)
  /// The cursor has reached the field's authenticated item count: forward the
  /// accumulated delta to step-03.
  FinishInputs {
    input_index: Int,
    output_index: Int,
    /// Re-opened so the *authenticated* item count — not a prover assertion —
    /// is what the cursor is compared against.
    spend_inputs_opening: FieldOpeningV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
