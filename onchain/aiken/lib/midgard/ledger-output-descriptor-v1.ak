use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_bytes, encode_definite_map_header,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_merkle_v1

pub const version: Int = 1

/// Accumulator for the single left-to-right pass over the canonical flat asset
/// list. `size` accrues exactly the bytes `ledger_output_scan_v1` accrues while
/// it walks the same assets in the output preimage: one policy head plus one
/// quantity-map head per policy group, and one name plus one quantity per
/// asset.
type AssetFoldV1 {
  count: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  policy_count: Int,
  size: Int,
}

/// Length of the leading run of `assets` under `policy_id`. The canonical
/// value orders policies strictly ascending, so one policy occupies exactly
/// one contiguous run and this is that run's length.
fn policy_run_length(
  policy_id: ByteArray,
  assets: List<Pair<ByteArray, Int>>,
) -> Int {
  when assets is {
    [] -> 0
    [Pair(unit, _), ..rest] ->
      if bytearray.take(unit, 28) == policy_id {
        1 + policy_run_length(policy_id, rest)
      } else {
        0
      }
  }
}

fn fold_assets(
  assets: List<Pair<ByteArray, Int>>,
  previous_policy: ByteArray,
  accumulator: AssetFoldV1,
) -> AssetFoldV1 {
  when assets is {
    [] -> accumulator
    [Pair(unit, quantity), ..rest] -> {
      let policy_id = bytearray.take(unit, 28)
      let asset_name = bytearray.drop(unit, 28)
      // A 28-byte policy id never equals the `#""` seed, so the first asset
      // always opens a group.
      let opens_group = policy_id != previous_policy
      let group_bytes = if opens_group {
        bytearray.length(encode_definite_bytes(policy_id)) + bytearray.length(
          encode_definite_map_header(policy_run_length(policy_id, assets)),
        )
      } else {
        0
      }
      fold_assets(
        rest,
        policy_id,
        AssetFoldV1 {
          count: accumulator.count + 1,
          peaks: validation_merkle_v1.append_leaf(
            accumulator.count,
            accumulator.peaks,
            ledger_output_commitment_v1.asset_leaf_hash(
              policy_id,
              asset_name,
              quantity,
            ),
          ),
          policy_count: accumulator.policy_count + if opens_group {
            1
          } else {
            0
          },
          size: accumulator.size + group_bytes + bytearray.length(
            encode_definite_bytes(asset_name),
          ) + bytearray.length(cbor.serialise(quantity)),
        },
      )
    }
  }
}

/// The CBOR length of the *Cardano* spelling of this value — a bare coin when
/// the output carries no multi-asset, and the `[coin, multiasset]` pair
/// otherwise. Byte-for-byte the same arithmetic `ledger_output_scan_v1`
/// performs incrementally over the output preimage (`step_value_header`,
/// `step_policy_header`, `step_asset`), stated here as one closed form over
/// the already-decoded value.
fn cardano_value_size(lovelace: Int, assets: AssetFoldV1) -> Int {
  if assets.count == 0 {
    bytearray.length(cbor.serialise(lovelace))
  } else {
    1 + bytearray.length(cbor.serialise(lovelace)) + bytearray.length(
      encode_definite_map_header(assets.policy_count),
    ) + assets.size
  }
}

fn reference_script_facts(
  output_index: Int,
  output: MidgardTxOutput,
) -> (Int, ByteArray, Int, ByteArray) {
  when output.script_ref is {
    None -> (-1, #"", 0, #"")
    Some(script) -> {
      let script_cbor = encode_midgard_versioned_script(script)
      (
        script_proof_v1.language_tag(script.language),
        script_proof_v1.versioned_script_hash(script),
        bytearray.length(script_cbor),
        bounded_item_v1.from_bytes(
          ledger_output_commitment_v1.output_field_index,
          output_index,
          script_cbor,
        ),
      )
    }
  }
}

/// Builds the compact ledger MPF value of one canonical output.
///
/// Spec §5.2/§5.3 and §5.5.3 make `LedgerOutputCommitmentV1` the *only* value
/// that ever enters `utxos_root`; §5.5.3 says so exclusively ("recomputed with
/// that descriptor as the MPT value, not with the full output bytes"). Every
/// producer commits descriptors — the node, the validation machine, the
/// watcher's replay and the DA reconstruction — so any on-chain rule that
/// re-derives a ledger root has to insert this, not `encode_midgard_tx_output`.
///
/// This is the one-shot twin of two things that already exist and must not
/// drift from it:
///
///   * off-chain, `buildCanonicalMidgardLedgerOutputMaterialV1`
///     (`demo/midgard-validation/src/ledger-output-descriptor.ts`), the encoder
///     every committed descriptor in DA and persistence comes from; and
///   * on-chain, the staged `ledger_output_proof_v1` admission machine, whose
///     `descriptor_is_exact_v1` proves the same facts from authenticated output
///     chunks across many transactions.
///
/// The staged machine exists because the *validation machine* meets outputs it
/// cannot afford to hold whole. A one-step transition-trace rule is the
/// opposite case: it already holds the exact output bytes, authenticated
/// through the §8.8 field door or projected from an authenticated L1 deposit
/// event, so the descriptor is a pure function of bytes it has. Deriving it
/// here — rather than accepting a prover-supplied descriptor — is what keeps
/// those rules sound: a challenger who could name the inserted value could
/// name a post-root and slash an honest operator.
///
/// Total: `None` for every output whose descriptor does not exist, which is
/// exactly the set the off-chain encoder also refuses — non-canonical output
/// bytes, a canonical-but-unmaterialisable datum (§6.2 bignum/tag-102 forms),
/// an out-of-domain output index, or a Cardano value wider than
/// `max_cardano_value_cbor_bytes`. Callers `expect` the `Some`, so a fraud
/// proof over such an output fails closed rather than minting on a value the
/// ledger never held.
pub fn build_v1(
  output_index: Int,
  output_cbor: ByteArray,
) -> Option<ledger_output_commitment_v1.LedgerOutputCommitmentV1> {
  if or {
    output_index < 0,
    output_index > ledger_output_commitment_v1.max_output_index,
  } {
    // Checked before anything is derived: `bounded_item_v1` salts its chunk
    // hashes with the item index and aborts outside the domain, so an
    // out-of-domain index has to be turned away rather than folded.
    None
  } else {
    when ledger_output_v1.decode_canonical_output(output_cbor) is {
      None -> None
      Some(output) ->
        when script_context_v1.ledger_output_summaries_v1(output_cbor) is {
          None -> None
          Some((cardano_tx_out, midgard_tx_out, cardano_spend_datum)) -> {
            let assets = fold_assets(
              output.value.assets,
              #"",
              AssetFoldV1 {
                count: 0,
                peaks: validation_merkle_v1.empty_frontier(),
                policy_count: 0,
                size: 0,
              },
            )
            let
                (
                  reference_script_language,
                  reference_script_hash,
                  reference_script_total_length,
                  reference_script_item_commitment,
                )
              = reference_script_facts(output_index, output)
            let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
              version: ledger_output_commitment_v1.version,
              output_index,
              total_length: bytearray.length(output_cbor),
              item_commitment: ledger_output_commitment_v1.output_item_commitment(
                output_index,
                output_cbor,
              ),
              address: encode_midgard_address(output.address),
              lovelace: output.value.lovelace,
              asset_count: assets.count,
              asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
                assets.count,
                assets.peaks,
              ),
              cardano_value_size: cardano_value_size(
                output.value.lovelace,
                assets,
              ),
              reference_script_language,
              reference_script_hash,
              reference_script_total_length,
              reference_script_item_commitment,
              cardano_tx_out,
              midgard_tx_out,
              cardano_spend_datum,
            }
            if ledger_output_commitment_v1.descriptor_is_well_formed(descriptor) {
              Some(descriptor)
            } else {
              None
            }
          }
        }
    }
  }
}

/// The exact bytes this output occupies in `utxos_root`.
pub fn ledger_value_v1(
  output_index: Int,
  output_cbor: ByteArray,
) -> Option<ByteArray> {
  when build_v1(output_index, output_cbor) is {
    None -> None
    Some(descriptor) -> Some(ledger_output_commitment_v1.encode(descriptor))
  }
}
