//// ## `value-not-preserved` step-01 — claim shape and thread types
////
//// ### The rule this family adjudicates
////
//// The validation machine's ValueAndMint stages accumulate, per accepted L2
//// transaction, a lovelace delta and a per-asset delta map — spent inputs add
//// (descriptor lovelace, authenticated asset-leaf quantities), outputs
//// subtract, the mint field adds — and stage five rejects the transaction
//// unless `lovelace_delta - body.fee == 0` and every per-asset delta is zero
//// (`reject_value_not_preserved`). A block whose operator committed and
//// **accepted** a transaction that violates this conservation rule has stolen
//// or destroyed funds, and this family converts that machine-covered rule into
//// a standalone L1 fault proof (execution-plan W-C14 single-party conversion).
////
//// ### The single-asset claim (execution-plan D-C1, resolved here)
////
//// D-C1 asked for a bounded-size on-chain summation strategy for multi-asset
//// value sums. The resolution is the owner-mandated **single-asset claim**:
//// the prover names one specific asset — ADA, or one `(policy id, asset name)`
//// unit — plus the direction of the imbalance, and the step chain accumulates
//// only that asset's balance. The chain never folds the whole value map; it
//// verifies one per-asset fold that restricts the machine's accumulator
//// semantics to the claimed unit:
////
////   * **ADA claim**: `Σ spent-input lovelace − Σ output lovelace − fee == 0`
////     is conservation. ADA is unmintable (a mint policy id is 28 bytes, the
////     ADA policy is empty — structural-na-q24) and output lovelace is
////     non-negative by canonical decoding (structural-na-q25), so the ADA
////     case needs no mint reading at all.
////   * **Token claim** for unit `u`:
////     `Σ spent-input qty_u + mint_u − Σ output qty_u == 0` is conservation.
////
//// The machine convicts when *any* asset's delta is non-zero; naming which
//// one (and in which direction) makes the on-chain check a single bounded
//// fold, with the search for the unbalanced asset done off-chain by the
//// prover.
////
//// ### Step-01's job, and the acceptance gate
////
//// Bind the challenged transaction out of the challenged block's counted
//// `transactions_root` (the one blessed binding path,
//// `midgard/fraud_proofs/common.pass_native_tx_to_next_step`), and refuse
//// unless the transaction was operator-**accepted**. An invalid transaction
//// honestly recorded as a no-op (`validity_code != 0`) is *correct* block
//// production — the machine applies no value transition for it — so it must
//// never reach the fold, let alone convict.
////
//// The `validity_code` read here is sound for the same reason as the
//// transition-trace family's `unanchored_validity_code_of(anchored) == 0`
//// call site (see `midgard/fraud_proofs/field_opening_v1`): the compact bytes
//// this step reads the code from are the exact bytes the block's counted
//// `transactions_root` committed as the leaf value — `plutarch_phas_raw`
//// checked those very bytes for membership — so the code is committed by the
//// **root**, not supplied by the prover. Step-01 is the only step of this
//// family that reads it, and downstream steps re-anchor fields 0/2/5 by the
//// transaction id alone, which §3 makes a complete anchor for body fields.
////
//// Step-01 freezes into thread state everything downstream steps need from
//// the header and the compact structure: the transaction id, the claim, the
//// committed fee, and the header's `prev_utxos_root` — the block's pre-state
//// ledger root that step-02's spent-input value witnesses authenticate
//// against.

use aiken/primitive/bytearray
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}

// ## Catalogue binding

/// Catalogue identifier of the `valueNotPreserved` category.
///
/// A category id is the `fraud_proof_catalogue.id_byte_count`-byte big-endian
/// index of the category's position in the append-only
/// `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`
/// (`demo/midgard-sdk/src/fraud-proof/catalogue.ts`), the same derivation
/// `transition_trace`'s `#"00000004"` follows.
///
/// The category is **not registered yet**: catalogue registration is
/// parent-owned integration work. Id `0x19` (25) is the reserved
/// pre-registration id for this family; until registration it appears only in
/// emulator wiring (an `extraCategories` sidecar), never in the canonical
/// order and never through the deployment manifest.
pub const value_not_preserved_fraud_category_id: ByteArray = #"00000019"

/// A value-not-preserved computation-thread token's asset name is this
/// family's category id followed by the 28-byte hash of the header under
/// challenge.
pub fn fraud_category_is_value_not_preserved_v1(
  computation_thread_token_asset_name: ByteArray,
) -> Bool {
  and {
    bytearray.length(computation_thread_token_asset_name) == fraud_proof_catalogue.id_byte_count + 28,
    bytearray.take(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    ) == value_not_preserved_fraud_category_id,
  }
}

// ## The single-asset claim

/// The one asset a thread accuses the transaction of not conserving.
///
/// Constructor order is wire format: `AdaAsset` is Constr 0, `TokenAsset`
/// Constr 1.
pub type ClaimedAssetV1 {
  /// The distinguished lovelace claim. Its conservation equation folds
  /// descriptor/output lovelace and the committed fee, and — by
  /// structural-na-q24/q25 — needs neither the mint field nor a negative
  /// output guard.
  AdaAsset
  /// One native-asset unit. `policy_id` is 28 bytes and `asset_name` at most
  /// 32, exactly the domain `ledger_output_commitment_v1.asset_leaf_hash` and
  /// the canonical mint item decoder enforce, so a claim outside that domain
  /// could never match a committed leaf or mint entry and is refused at the
  /// door.
  TokenAsset { policy_id: ByteArray, asset_name: ByteArray }
}

/// Which way the claimed asset's conservation fails. The final delta is
/// `inflow − outflow` (spent inputs plus mint, minus outputs, minus the fee
/// for an ADA claim), so:
///
///   * `ClaimedAssetInflated` — outputs (plus fee) exceed the sources: the
///     transaction creates value out of nothing (`delta < 0`). Fund theft.
///   * `ClaimedAssetDeflated` — sources exceed outputs: the transaction
///     destroys value (`delta > 0`).
///
/// Constructor order is wire format: `ClaimedAssetInflated` is Constr 0,
/// `ClaimedAssetDeflated` Constr 1.
pub type ClaimedImbalanceDirectionV1 {
  ClaimedAssetInflated
  ClaimedAssetDeflated
}

/// The claim's domain check, applied once at step-01: everything downstream
/// steps compare against leaves and mint entries is inside the domain those
/// commitments are defined over.
pub fn claimed_asset_is_well_formed_v1(claimed_asset: ClaimedAssetV1) -> Bool {
  when claimed_asset is {
    AdaAsset -> True
    TokenAsset { policy_id, asset_name } -> and {
        bytearray.length(policy_id) == 28,
        bytearray.length(asset_name) <= 32,
      }
  }
}

/// Whether one committed unit is the claimed one. `unit` is the concatenated
/// `policy_id ‖ asset_name` spelling `MidgardValue.assets` carries; the split
/// at byte 28 is the same one `ledger_output_descriptor_v1`'s asset fold
/// applies, so a claim and a committed unit agree exactly when their leaves
/// would.
pub fn unit_matches_claim_v1(
  claimed_asset: ClaimedAssetV1,
  unit: ByteArray,
) -> Bool {
  when claimed_asset is {
    AdaAsset -> False
    TokenAsset { policy_id, asset_name } -> and {
        bytearray.take(unit, 28) == policy_id,
        bytearray.drop(unit, 28) == asset_name,
      }
  }
}

// ## Step-01 thread types

pub type Args {
  /// The blessed binding of the challenged transaction to the challenged
  /// block: counted `transactions_root` plus `plutarch_phas_raw` membership,
  /// through `common.pass_native_tx_to_next_step` and no other path.
  tx_inclusion: NativeTxInclusionArgs,
  /// The single asset this thread accuses the transaction of not conserving.
  claimed_asset: ClaimedAssetV1,
  /// The direction of the claimed imbalance.
  claimed_direction: ClaimedImbalanceDirectionV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
