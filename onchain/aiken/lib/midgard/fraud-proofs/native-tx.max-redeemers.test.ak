// The `maximum_cardano_*` constants in this module are GENERATED; the tests
// around them are written by hand.
//
//   producer: pnpm --dir demo/midgard-validation run fixtures:ordered-collection-boundary-aiken:sync
//
// Their source is the genuine signed-Cardano field-8 boundary found by
// `demo/midgard-validation/tests/ordered-collection-redeemer-boundary.test.ts`,
// which is the only implementation of that search; the producer runs the suite and
// rebinds these constants from the vector it publishes (#588).

use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  encode_redeemer_witness_preimage,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, SpendRedeemer,
}
use midgard/native_tx_field_access_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_trace_v1

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-redeemer-boundary.test.ts.
const maximum_cardano_spend_redeemer_count = 296

/// Section 5.1 bytes of field 8 completed before the terminal item begins.
const maximum_cardano_terminal_encoded_length_before_item = 5330

/// The redeemer datum and execution units every item of the field-8 maximum
/// fixture carries. Declared by the TypeScript boundary fixture, which chose
/// them to land the transaction exactly on the Cardano execution envelope.
const maximum_cardano_spend_redeemer_cbor = #"d87980"

const maximum_cardano_spend_redeemer_ex_memory = 1601

const maximum_cardano_spend_redeemer_ex_steps = 316149

const maximum_cardano_spend_redeemer_preimage_bytes = 5349

const maximum_cardano_spend_redeemer_preimage_hash =
  #"48c9573caee7147b9cb991e575dd5e203cf09c52dda6dc6701976f1f72e81918"

const maximum_cardano_spend_redeemer_collection_commitment =
  #"48c9573caee7147b9cb991e575dd5e203cf09c52dda6dc6701976f1f72e81918"

const maximum_cardano_transaction_id =
  #"f5f996b9433cd5b58e08f0f6a65de8ab93b93c7d71a9d965166871deb0f72605"

const maximum_cardano_transaction_commitment =
  #"910c82e3f83b53c9c30132bdc2cf9f6ffe375405de035c5293efd1fcf343a66f"

const maximum_cardano_compact_cbor =
  #"84018c5820d2de13bbfffe80e6db054d1577456ea75b28dea66b6aaed899d3dd4c54faecab582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820a7774afe7dd2957973cf90d95c0f9d3a56bb72356a4d2e3cd57e11654505c3801a000de2ec2020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820e650a24c14c0e6a48877805b4185f8ff2ee711e964e6aa63ce05c29ddeb1bd26582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820fe9cbd2aa7dde86cac94b8047c6ba073e1ea7ffbcbf6a939f83b6eb8466c922600"

const maximum_cardano_witness_set_compact_cbor =
  #"835820e09b6f4823d7127b1759a6ee84e54f18ca19c061ed6fbeea403e5a3040bba98d582065fddcafe70161066b6a2a5cb713ec4fb52a1e2d94c729ac7b9c8e26e5b0c0d7582048c9573caee7147b9cb991e575dd5e203cf09c52dda6dc6701976f1f72e81918"

const maximum_cardano_field_preimage_lengths_cbor =
  #"89192e6b0118300101011718681914e5"

const maximum_cardano_validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

const maximum_cardano_terminal_pre_work_root =
  #"eb6af1445c43188db21af5507139f8f6d49625b949782421d97e017b7f2bd049"

const maximum_cardano_terminal_post_work_root =
  #"c517e8eb697fd4f265fcbd06f6799cbd6e13a2c5aad5048bc18c60fded4769ae"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn maximum_cardano_spend_redeemers_from(
  pointer_index: Int,
) -> List<MidgardRedeemerWitness> {
  if pointer_index > maximum_cardano_spend_redeemer_count {
    []
  } else {
    [
      MidgardRedeemerWitness {
        purpose: SpendRedeemer,
        index: pointer_index,
        redeemer_cbor: maximum_cardano_spend_redeemer_cbor,
        execution_units: MidgardExecutionUnits {
          memory: maximum_cardano_spend_redeemer_ex_memory,
          steps: maximum_cardano_spend_redeemer_ex_steps,
        },
      },
      ..maximum_cardano_spend_redeemers_from(pointer_index + 1)
    ]
  }
}

fn redeemer_pointer_sequence_is_exact(
  redeemers: List<MidgardRedeemerWitness>,
  expected_pointer_index: Int,
) -> Bool {
  when redeemers is {
    [] -> expected_pointer_index == maximum_cardano_spend_redeemer_count + 1
    [redeemer, ..rest] -> and {
        redeemer.index == expected_pointer_index,
        redeemer_pointer_sequence_is_exact(rest, expected_pointer_index + 1),
      }
  }
}

test maximum_cardano_spend_redeemer_field_matches_typescript_terminal_commitment() {
  let redeemers = maximum_cardano_spend_redeemers_from(1)
  let preimage_cbor = encode_redeemer_witness_preimage(redeemers)
  let decoded = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    preimage_cbor,
  )
  let item_cbors = redeemers |> list.map(encode_midgard_redeemer_witness)
  expect Some(first) = list.at(decoded, 0)
  expect Some(last) = list.last(decoded)

  and {
    list.length(decoded) == maximum_cardano_spend_redeemer_count,
    decoded == redeemers,
    redeemer_pointer_sequence_is_exact(decoded, 1),
    first.index == 1,
    last.index == maximum_cardano_spend_redeemer_count,
    list.all(
      decoded,
      fn(redeemer) {
        and {
          redeemer.purpose == SpendRedeemer,
          redeemer.redeemer_cbor == maximum_cardano_spend_redeemer_cbor,
          redeemer.execution_units.memory == maximum_cardano_spend_redeemer_ex_memory,
          redeemer.execution_units.steps == maximum_cardano_spend_redeemer_ex_steps,
        }
      },
    ),
    bytearray.length(preimage_cbor) == maximum_cardano_spend_redeemer_preimage_bytes,
    blake2b_256(preimage_cbor) == maximum_cardano_spend_redeemer_preimage_hash,
    native_tx_field_access_v1.field_commitment_from_items(item_cbors) == maximum_cardano_spend_redeemer_collection_commitment,
  }
}

test maximum_cardano_spend_redeemer_terminal_fold_matches_typescript() {
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    maximum_cardano_compact_cbor,
    maximum_cardano_witness_set_compact_cbor,
    maximum_cardano_field_preimage_lengths_cbor,
    maximum_cardano_validation_context_cbor,
    8,
    295,
    0,
    maximum_cardano_spend_redeemer_count,
    maximum_cardano_terminal_encoded_length_before_item,
  )
  let compact_binding_witness_cbor = vm_witness.encode_compact_binding_witness(
    maximum_cardano_transaction_id,
    maximum_cardano_transaction_commitment,
    maximum_cardano_compact_cbor,
    maximum_cardano_witness_set_compact_cbor,
    maximum_cardano_field_preimage_lengths_cbor,
    maximum_cardano_validation_context_cbor,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: maximum_cardano_transaction_id,
    transaction_id: maximum_cardano_transaction_id,
    transaction_commitment: maximum_cardano_transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      maximum_cardano_validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: maximum_cardano_transaction_commitment,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      40,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CompactBinding,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      41,
      compact_binding_witness_cbor,
    ),
  }
  and {
    pre.work_root == maximum_cardano_terminal_pre_work_root,
    post.work_root == maximum_cardano_terminal_post_work_root,
    vm_one_step.verify_canonical_decode_one_step_v1(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: vm_machine_types.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: post,
        },
        // §8.1 tier-1 carriage: the field-8 preimage itself, rebuilt from the
        // generated `maximum_cardano_*` constants by this module's own §5.1
        // encoder. Nothing per-item is supplied — the door hashes the preimage
        // once against the compact structure's `redeemer_tx_wits_hash` (which
        // `maximum_cardano_spend_redeemer_preimage_hash` pins) and item 295 is
        // then a slice.
        auxiliary: vm_machine_types.TransactionFieldChunkWitness {
          field_index: 8,
          item_index: 295,
          carriage: native_tx_field_access_v1.Inline {
            preimage: encode_redeemer_witness_preimage(
              maximum_cardano_spend_redeemers_from(1),
            ),
          },
        },
      },
      field_door.MachineFieldDoorV1 {
        reference_inputs: [],
        certificate_policy_id: #"",
      },
    ),
  }
}
