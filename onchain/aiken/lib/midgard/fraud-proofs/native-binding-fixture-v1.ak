//// Test-fixture support for the native-V1 counted-root binding selectors of
//// the fault-proof step validators (GOAL_SPEC.md Q00).
////
//// This module is referenced by tests embedded in the
//// `validators/fraud-proofs/**` step modules, and — for its §8 tier-3 carriage
//// helpers alone (`certified_carriage_v1` /
//// `certified_reference_inputs_v1`) — by `native-tx.max-inline-datum.test.ak`,
//// whose 16,225-byte field-2 boundary has no admissible carriage below tier 3. It builds a real single-tx
//// native-V1 block fixture (canonical compact CBOR, genuine MPF transactions
//// root, counted-root committed header) plus the L1 transactions that drive a
//// computation-thread `Continue` or finalize step against it. It is not
//// reachable from any on-chain validator entry point.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{merkle_16}
use aiken_design_patterns/linked_list
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Redeemer,
  ScriptPurpose, Transaction, Withdraw, placeholder,
}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/chunked_inclusion_v1
use midgard/fraud_proofs/common.{
  NativeTxInclusionArgs, NativeTxInclusionCarriage,
}
use midgard/fraud_proofs/field_opening_v1
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/hub_oracle
use midgard/ledger_state
use midgard/mpf_chunked_proof_v1
use midgard/native_tx_carriage_v1 as native_carriage
use midgard/native_tx_field_access_v1 as field_access
use midgard/state_queue
use midgard/transition_trace

// ## Deterministic identities

pub const hub_oracle_script_hash =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

pub const state_queue_policy_id =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

pub const computation_thread_policy_id =
  #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

pub const step_script_hash =
  #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

pub const next_step_script_hash =
  #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

pub const fraud_proof_policy_id =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

pub const fraud_prover_key_hash =
  #"11111111111111111111111111111111111111111111111111111111"

pub const fraud_category_id = #"00000001"

const h32_a =
  #"1010101010101010101010101010101010101010101010101010101010101010"

const h32_b =
  #"2020202020202020202020202020202020202020202020202020202020202020"

const h28_x = #"30303030303030303030303030303030303030303030303030303030"

pub fn own_out_ref() -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: 0 }
}

pub fn fraud_proof_token_address() -> Address {
  address.from_script(fraud_proof_policy_id)
}

// ## Canonical native-V1 transactions

/// A representative, fully canonical native-V1 transaction as committed by a
/// well-formed block: one spend input, one reference input, one output, a fee,
/// one required signer, and one address witness whose key hashes to the
/// required signer.
pub fn valid_native_tx_v1() -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  native_types.MidgardTransaction {
    version: native_types.native_tx_version_v1,
    validity: native_types.TxIsValid,
    body: native_types.MidgardTransactionBody {
      inputs: [native_types.MidgardTxInput { tx_id: h32_b, output_index: 0 }],
      reference_inputs: [
        native_types.MidgardTxInput { tx_id: h32_b, output_index: 1 },
      ],
      outputs: [sample_native_output()],
      fee: 1_000_000,
      validity_interval_start: env.posix_time_none,
      validity_interval_end: env.posix_time_none,
      required_observers: [],
      required_signers: [
        utils.get_verification_key_hash(sample_witness_verification_key()),
      ],
      mint: empty_mint,
      script_integrity_hash: h32_a,
      auxiliary_data_hash: h32_a,
      network_id: 0,
    },
    witness_set: native_types.MidgardTransactionWitnessSet {
      addr_tx_wits: [sample_address_witness()],
      script_tx_wits: [sample_native_script_witness()],
      redeemer_tx_wits: [],
    },
  }
}

pub fn sample_native_output() -> native_types.MidgardTxOutput {
  native_types.MidgardTxOutput {
    address: native_types.MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: native_types.PubKeyCredential(h28_x),
      stake_credential: None,
    },
    value: native_types.MidgardValue { lovelace: 5_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

pub fn sample_witness_verification_key() -> ByteArray {
  #"4040404040404040404040404040404040404040404040404040404040404040"
}

pub fn sample_witness_signature() -> ByteArray {
  #"5050505050505050505050505050505050505050505050505050505050505050"
    |> builtin.append_bytearray(
        #"6060606060606060606060606060606060606060606060606060606060606060",
      )
}

pub fn sample_address_witness() -> native_types.MidgardAddressWitness {
  native_types.MidgardAddressWitness {
    verification_key: sample_witness_verification_key(),
    signature: sample_witness_signature(),
  }
}

pub fn sample_native_script_bytes() -> ByteArray {
  #"8200581c303030303030303030303030303030303030303030303030303030"
}

pub fn sample_native_script_witness() -> native_types.MidgardVersionedScript {
  native_types.MidgardVersionedScript {
    language: native_types.NativeCardanoScript,
    script_bytes: sample_native_script_bytes(),
  }
}

// ## Single-transaction native-V1 block fixture

pub type NativeBlockFixtureV1 {
  tx: native_types.MidgardTransaction,
  tx_id: ByteArray,
  compact: native_types.NativeTxCompact,
  compact_cbor: ByteArray,
  witness_set_compact: native_types.NativeTxWitnessSetCompact,
  transactions_phas_root: ByteArray,
  header: ledger_state.HeaderV1,
  header_hash: ByteArray,
}

/// Commits `tx` as the single transaction of a native-V1 block: the raw MPF
/// transactions root holds exactly `(tx_id, compact_cbor)` and the header
/// commits it under the `TransactionsV1RootDomain` counted root.
pub fn native_block_fixture_v1(
  tx: native_types.MidgardTransaction,
) -> NativeBlockFixtureV1 {
  native_block_fixture_with_withdrawals_v1(tx, env.empty_merkle_tree_root, 0)
}

pub fn native_block_fixture_with_withdrawals_v1(
  tx: native_types.MidgardTransaction,
  withdrawals_phas_root: ByteArray,
  withdrawal_count: Int,
) -> NativeBlockFixtureV1 {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let compact_cbor = native_compact.encode_native_tx_compact_v1(compact)
  let tx_id = native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
  let witness_set_compact = native_transaction.midgard_transaction_witness_set_to_compact(
    tx.witness_set,
  )
  let transactions_phas_root = mpf.root(
    mpf.insert(mpf.empty, tx_id, compact_cbor, []),
  )
  let withdrawals_root = transition_trace.commit_counted_root(
    transition_trace.WithdrawalsRootDomain,
    withdrawals_phas_root,
    withdrawal_count,
  )
  let header = ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      transactions_phas_root,
      1,
    ),
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count,
    forced_transaction_count: 0,
    l2_transaction_count: 1,
    deposit_count: 0,
    total_event_count: withdrawal_count + 1,
    transition_step_count: withdrawal_count + 1,
    validation_trace_count: 1,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_x,
    operator_vkey: h28_x,
    protocol_version: ledger_state.protocol_version_v1,
  }
  let header_data: Data = header
  let header_hash = utils.serialise_and_hash_28(header_data)
  NativeBlockFixtureV1 {
    tx,
    tx_id,
    compact,
    compact_cbor,
    witness_set_compact,
    transactions_phas_root,
    header,
    header_hash,
  }
}

/// Re-commits a block fixture so its single `transactions_root` leaf is the
/// arbitrary pair `(committed_key, committed_leaf_value)`, then re-derives the
/// counted root, the header and the header hash.
///
/// The key is deliberately **not** required to be the value's canonical
/// native-V1 transaction id: this is the committed shape the
/// `da-hash-preimage` family adjudicates (GOAL_SPEC.md Q44). `tx_id` and
/// `compact_cbor` on the returned fixture carry the committed pair, so
/// `native_inclusion_args_v1` produces the matching prover arguments.
pub fn committed_leaf_block_fixture_v1(
  base: NativeBlockFixtureV1,
  committed_key: ByteArray,
  committed_leaf_value: ByteArray,
) -> NativeBlockFixtureV1 {
  let transactions_phas_root = mpf.root(
    mpf.insert(mpf.empty, committed_key, committed_leaf_value, []),
  )
  let header = ledger_state.HeaderV1 {
    ..base.header,
    transactions_root: transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      transactions_phas_root,
      1,
    ),
  }
  let header_data: Data = header
  NativeBlockFixtureV1 {
    ..base,
    tx_id: committed_key,
    compact_cbor: committed_leaf_value,
    transactions_phas_root,
    header,
    header_hash: utils.serialise_and_hash_28(header_data),
  }
}

/// Single-transaction block whose only committed leaf holds the canonical
/// compact bytes of `tx` under the foreign key `committed_key` — the
/// `da-hash-preimage` violation.
pub fn miskeyed_block_fixture_v1(
  tx: native_types.MidgardTransaction,
  committed_key: ByteArray,
) -> NativeBlockFixtureV1 {
  let base = native_block_fixture_v1(tx)
  committed_leaf_block_fixture_v1(base, committed_key, base.compact_cbor)
}

pub fn ct_token_asset_name(fixture: NativeBlockFixtureV1) -> ByteArray {
  fraud_category_id |> builtin.append_bytearray(fixture.header_hash)
}

/// The compact structure the same block would have committed had `tx`'s witness
/// set been `witness_set` — genuine body, prover-chosen tail.
///
/// This is the forgery §3's two-level split makes possible and the transaction
/// id cannot see. The id preimage is `encode_native_tx_body_compact(body)`
/// alone, so replacing the compact structure's trailing `witness_set_hash`
/// leaves `tx_id` **exactly** as it was: the returned fixture's
/// `compact_cbor` re-derives to the committed transaction's id, and any check
/// that only re-derives the id accepts it. `header`/`transactions_phas_root` are
/// deliberately left committing the *honest* bytes, because a finalize step does
/// not re-open the block — the whole point is that the forgery never has to
/// survive an inclusion proof.
///
/// The useful choice of `witness_set` is the empty one
/// (`empty_witness_set_compact`): it makes the §2.5 absence rules over fields 6
/// and 7 vacuously true, which is a slashing proof against an honest operator.
pub fn forged_witness_set_block_v1(
  base: NativeBlockFixtureV1,
  witness_set: native_types.NativeTxWitnessSetCompact,
) -> NativeBlockFixtureV1 {
  let compact = native_types.NativeTxCompact {
    ..base.compact,
    witness_set_hash: blake2b_256(
      native_compact.encode_native_tx_witness_set_compact(witness_set),
    ),
  }
  NativeBlockFixtureV1 {
    ..base,
    compact,
    compact_cbor: native_compact.encode_native_tx_compact_v1(compact),
    witness_set_compact: witness_set,
  }
}

/// The canonical three-hash compact form of an arbitrary witness set — the
/// prover's side of a forgery, built exactly as an honest block builder would
/// build it.
pub fn witness_set_compact_v1(
  witness_set: native_types.MidgardTransactionWitnessSet,
) -> native_types.NativeTxWitnessSetCompact {
  native_transaction.midgard_transaction_witness_set_to_compact(witness_set)
}

/// A compact witness set whose three §2.5 collection hashes are taken over
/// items the caller supplies **already encoded**, canonical or not.
///
/// `witness_set_compact_v1` goes through the typed `MidgardTransactionWitnessSet`
/// and can therefore only ever commit canonical items. A selector that has to
/// commit a *non*-canonical one — to show that a step's item-level §6.1 check is
/// load-bearing rather than decorative — must bypass the type, and this is the
/// only place that does.
pub fn witness_set_compact_of_items(
  addr_tx_wits: List<ByteArray>,
  script_tx_wits: List<ByteArray>,
  redeemer_tx_wits: List<ByteArray>,
) -> native_types.NativeTxWitnessSetCompact {
  native_types.NativeTxWitnessSetCompact {
    addr_tx_wits_hash: blake2b_256(
      field_access.encode_field_preimage(addr_tx_wits),
    ),
    script_tx_wits_hash: blake2b_256(
      field_access.encode_field_preimage(script_tx_wits),
    ),
    redeemer_tx_wits_hash: blake2b_256(
      field_access.encode_field_preimage(redeemer_tx_wits),
    ),
  }
}

/// A single-transaction block whose committed transaction commits **exactly
/// `preimage`** at §2.5 slot `field_index` — bytes the caller chooses, a §5.1
/// envelope or not.
///
/// This is the committed shape the `canonical-decodability` family adjudicates
/// (`docs/spec/midgard-tx.md` §12.7), and no honest producer can build it:
/// `encode_field_preimage` writes the count it measures and
/// `midgard_transaction_to_compact` only ever commits what it encoded, so a
/// fixture for a non-envelope commitment has to be assembled below the typed
/// layer. `witness_set_compact_of_items` is the same bypass one level in — it
/// commits non-canonical *items* inside a well-formed envelope — and the two
/// together are the only places in the tree that commit bytes the encoders
/// would not produce.
///
/// Everything else about the block stays honest. The compact structure is
/// re-encoded, the transaction id is re-derived from the tampered body, the
/// witness-set hash is re-derived from the tampered witness set, and the leaf is
/// re-committed under its own id — so the block passes every native family's
/// codec precondition and is faulted for the field alone. The returned fixture's
/// `tx`/`compact`/`witness_set_compact` are updated in step, so a caller reading
/// them to build a claim reads the committed values.
pub fn committed_field_preimage_block_fixture_v1(
  base: NativeBlockFixtureV1,
  field_index: Int,
  preimage: ByteArray,
) -> NativeBlockFixtureV1 {
  let Pair(body, witness_set) = committed_field_structures(
    base.compact.body,
    base.witness_set_compact,
    field_index,
    field_access.field_commitment(preimage),
  )
  let compact = native_types.NativeTxCompact {
    ..base.compact,
    body,
    witness_set_hash: blake2b_256(
      native_compact.encode_native_tx_witness_set_compact(witness_set),
    ),
  }
  let compact_cbor = native_compact.encode_native_tx_compact_v1(compact)
  let tx_id = native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(body),
  )
  let recommitted = committed_leaf_block_fixture_v1(base, tx_id, compact_cbor)
  NativeBlockFixtureV1 {
    ..recommitted,
    compact,
    witness_set_compact: witness_set,
  }
}

/// §2.5's positional table on the producer side: replace slot `field_index`'s
/// commitment in whichever of the two structures owns it, and leave the other
/// untouched.
fn committed_field_structures(
  body: native_types.NativeTxBodyCompact,
  witness_set: native_types.NativeTxWitnessSetCompact,
  field_index: Int,
  commitment: ByteArray,
) -> Pair<native_types.NativeTxBodyCompact,
native_types.NativeTxWitnessSetCompact> {
  when field_index is {
    0 ->
      Pair(
        native_types.NativeTxBodyCompact {
          ..body,
          spend_inputs_hash: commitment,
        },
        witness_set,
      )
    1 ->
      Pair(
        native_types.NativeTxBodyCompact {
          ..body,
          reference_inputs_hash: commitment,
        },
        witness_set,
      )
    2 ->
      Pair(
        native_types.NativeTxBodyCompact { ..body, outputs_hash: commitment },
        witness_set,
      )
    3 ->
      Pair(
        native_types.NativeTxBodyCompact {
          ..body,
          required_observers_hash: commitment,
        },
        witness_set,
      )
    4 ->
      Pair(
        native_types.NativeTxBodyCompact {
          ..body,
          required_signers_hash: commitment,
        },
        witness_set,
      )
    5 ->
      Pair(
        native_types.NativeTxBodyCompact { ..body, mint_hash: commitment },
        witness_set,
      )
    6 ->
      Pair(
        body,
        native_types.NativeTxWitnessSetCompact {
          ..witness_set,
          script_tx_wits_hash: commitment,
        },
      )
    7 ->
      Pair(
        body,
        native_types.NativeTxWitnessSetCompact {
          ..witness_set,
          addr_tx_wits_hash: commitment,
        },
      )
    _ -> {
      expect field_index == 8
      Pair(
        body,
        native_types.NativeTxWitnessSetCompact {
          ..witness_set,
          redeemer_tx_wits_hash: commitment,
        },
      )
    }
  }
}

/// The canonical compact witness set of a transaction that witnesses nothing:
/// all three §2.5 collections empty. Every `field_item_count` over it is 0, so
/// every absence claim over fields 6–8 holds vacuously — which is what makes it
/// the forgery a missing-signature or missing-script prover wants.
pub fn empty_witness_set_compact() -> native_types.NativeTxWitnessSetCompact {
  witness_set_compact_v1(
    native_types.MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  )
}

// ## Shared family-selector fixtures
//
// Every `validators/fraud-proofs/**` selector needs the same two things: a
// §8 certificate policy to parameterise its validator with, and a tier-1
// carriage of one field's preimage. They lived as a per-file constant and a
// per-file `inline_*_opening` before #575's review; one copy each is enough,
// and a single copy is the only way a change to the §8 opening shape reaches
// every selector at once.

/// The §8 field-preimage certificate policy the selectors parameterise their
/// validators with.
///
/// Tier-1 (`Inline`) carriage never consults a certificate, so for every
/// selector but the tier-3 ones below this value's job is to be *unusable*: no
/// reference input in their L1 transactions carries it, and a selector that
/// silently fell through to a tier-2 or tier-3 path would therefore fail rather
/// than pass against an unchecked policy. The tier-3 fixtures below deliberately
/// mint under it, because their whole claim is about what a *well-formed*
/// certificate can and cannot authorise.
pub const test_certificate_policy_id: assets.PolicyId =
  #"77777777777777777777777777777777777777777777777777777777"

/// Tier-1 carriage of a **body** field's preimage (§2.5 fields 0–5), over items
/// the caller has already canonically encoded.
pub fn inline_body_opening(
  compact_cbor: ByteArray,
  encoded_items: List<ByteArray>,
) -> field_opening_v1.FieldOpeningV1 {
  field_opening_v1.BodyFieldOpening {
    native_tx_compact_cbor: compact_cbor,
    carriage: field_access.Inline {
      preimage: field_access.encode_field_preimage(encoded_items),
    },
  }
}

/// Tier-1 carriage of a **witness-set** field's preimage (§2.5 fields 6–8),
/// paired with the witness set the door checks against the anchored
/// `witness_set_hash`.
pub fn inline_witness_opening(
  compact_cbor: ByteArray,
  witness_set: native_types.NativeTxWitnessSetCompact,
  encoded_items: List<ByteArray>,
) -> field_opening_v1.FieldOpeningV1 {
  field_opening_v1.WitnessFieldOpening {
    native_tx_compact_cbor: compact_cbor,
    witness_set,
    carriage: field_access.Inline {
      preimage: field_access.encode_field_preimage(encoded_items),
    },
  }
}

// ## Tier-3 (§8.4 Certified) carriage fixtures
//
// A §8.6 certificate is the *only* thing binding a tier-3 preimage to the
// disputed transaction — the door has no preimage to hash, which is what tier 3
// is for. These builders produce a certificate that is honest in every
// mechanical respect: real byte length, real per-chunk digests, the mint-welded
// `field_hash` of the preimage's own bytes (#606), minted under the policy the
// selector parameterises its validator with, carrying the constant §8.6 asset
// name over a datum naming the committed transaction's `(tx_id, field_index)`.
// Whether the *field* it certifies is the one that transaction committed is
// the door's question since #606: the datum's `field_hash` must equal the
// commitment the door derives from the anchored structures, so a certificate
// over bytes the transaction never committed is refused at the door.

const carriage_holder_script_hash =
  #"91919191919191919191919191919191919191919191919191919191"

/// The §8.4 chunk carriage of `preimage`, as reference inputs at slots
/// `1 ..= n`. §8.5: inert bytes at an arbitrary address, no token, no ordering
/// claim beyond the index the redeemer names.
fn certified_chunk_input(index: Int, chunk: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: blake2b_256(cbor.serialise(index)),
      output_index: index + 1,
    },
    output: Output {
      address: address.from_script(carriage_holder_script_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(builtin.b_data(chunk)),
      reference_script: None,
    },
  }
}

/// Certificate at slot 0, chunks at slots `1 ..= n` — the layout
/// `certified_carriage_v1` names.
pub fn certified_reference_inputs_v1(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> List<Input> {
  let certificate: Data = native_carriage.field_preimage_certificate_v1(
    fraud_prover_key_hash,
    tx_id,
    field_index,
    preimage,
  )
  let certificate_input = Input {
    output_reference: OutputReference { transaction_id: tx_id, output_index: 0 },
    output: Output {
      address: address.from_script(test_certificate_policy_id),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            test_certificate_policy_id,
            field_access.field_preimage_certificate_asset_name,
            1,
          ),
      datum: InlineDatum(certificate),
      reference_script: None,
    },
  }
  [
    certificate_input,
    ..native_carriage.split_field_preimage(preimage)
      |> list.indexed_map(certified_chunk_input)
  ]
}

/// The carriage naming the layout `certified_reference_inputs_v1` builds.
pub fn certified_carriage_v1(
  preimage: ByteArray,
) -> field_access.FieldCarriageV1 {
  certified_carriage_from_v1(0, preimage)
}

/// The same layout, shifted so the certificate sits at `first_index` and the
/// chunks follow it.
///
/// A **finalize** step's L1 transaction has no block reference inputs, so the
/// carriage starts at slot 0 and `certified_carriage_v1` is the whole story. A
/// `Continue` step's does: `continue_step_l1_tx_v1` puts the hub oracle at 0 and
/// the state-queue node at 1, exactly where the inclusion args say they are, so
/// carriage appended to it starts at 2. Naming the offset rather than hard-coding
/// either layout is what stops a selector from silently pointing its chunk
/// indices at the block evidence.
pub fn certified_carriage_from_v1(
  first_index: Int,
  preimage: ByteArray,
) -> field_access.FieldCarriageV1 {
  let chunk_count = list.length(native_carriage.split_field_preimage(preimage))
  field_access.Certified {
    cert_ref_input_index: first_index,
    chunk_ref_input_indices: list.range(
      first_index + 1,
      first_index + chunk_count,
    ),
  }
}

/// `block` repeated `times` over, built by doubling.
///
/// A left fold of `concat` is quadratic in the result's length and does not fit
/// the test budget at the widths §5.4 admits; doubling is O(log times) concats
/// over geometrically growing arguments.
pub fn repeated_bytes_v1(block: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else if times == 1 {
    block
  } else {
    let half = repeated_bytes_v1(block, times / 2)
    let doubled = builtin.append_bytearray(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      builtin.append_bytearray(doubled, block)
    }
  }
}

/// A committed preimage of exactly `total_length` bytes that is **not** a §5.1
/// envelope, and is ungrammatical for the reason only the end of a walk can
/// see: one well-formed item followed by one byte §5.1 admits nowhere.
///
/// The shape is deliberate. Of the ways to leave §5.1, trailing content is the
/// only one that costs the *whole* walk before the verdict is known — every
/// other refusal stops at the byte that causes it — so this is the worst
/// admissible shape for a fixed-item-count field and the one a cost claim about
/// this family has to be taken at. Holding the item count at one keeps the
/// per-item term out of the measurement; the per-item term is priced separately.
pub fn ungrammatical_trailing_preimage_v1(total_length: Int) -> ByteArray {
  // `81` + `59 LLLL` + L payload + one trailing byte. The 3-byte item header is
  // §5.1's minimal form only for L > 255, which is what the bound enforces.
  let payload_length = total_length - 5
  expect payload_length > 0xff
  builtin.append_bytearray(
    field_access.encode_field_preimage(
      [repeated_bytes_v1(#"00", payload_length)],
    ),
    #"41",
  )
}

/// Tier-3 carriage of a **witness-set** field (§2.5 fields 6–8), paired with
/// the witness set the door checks against the anchored `witness_set_hash`.
pub fn certified_witness_opening(
  compact_cbor: ByteArray,
  witness_set: native_types.NativeTxWitnessSetCompact,
  preimage: ByteArray,
) -> field_opening_v1.FieldOpeningV1 {
  field_opening_v1.WitnessFieldOpening {
    native_tx_compact_cbor: compact_cbor,
    witness_set,
    carriage: certified_carriage_v1(preimage),
  }
}

/// Tier-3 carriage of a **body** field (§2.5 fields 0–5).
pub fn certified_body_opening(
  compact_cbor: ByteArray,
  preimage: ByteArray,
) -> field_opening_v1.FieldOpeningV1 {
  field_opening_v1.BodyFieldOpening {
    native_tx_compact_cbor: compact_cbor,
    carriage: certified_carriage_v1(preimage),
  }
}

/// The prover-side inclusion arguments matching `continue_step_l1_tx_v1`.
pub fn native_inclusion_args_v1(
  fixture: NativeBlockFixtureV1,
) -> NativeTxInclusionArgs {
  NativeTxInclusionArgs {
    input_index: 0,
    output_index: 0,
    hub_ref_input_index: 0,
    state_queue_node_ref_input_index: 1,
    native_tx_id: fixture.tx_id,
    native_tx_compact_cbor: fixture.compact_cbor,
    transactions_phas_root: fixture.transactions_phas_root,
    tx_membership_proof: [],
    inclusion_proof_script_withdraw_redeemer_index: 0,
  }
}

/// The prover-side inclusion carriage matching `continue_step_l1_tx_v1`: the
/// proof rides in this transaction's redeemers, which is the route every
/// pre-#545 selector exercises.
pub fn native_inclusion_carriage_v1(
  fixture: NativeBlockFixtureV1,
) -> NativeTxInclusionCarriage {
  common.RedeemerCarriedInclusion(native_inclusion_args_v1(fixture))
}

// ## Published-chunk carriage fixtures (issue #545)
//
// The on-chain library has no prover, so a proof deep enough to matter is
// synthesised here and the block's header is then made to commit exactly the
// root that proof proves. Neighbour digests are arbitrary but deterministic;
// an all-`Branch` ladder is the adversarial shape, because a ground sibling at
// every level forces MPF's widest step.

fn branch_neighbors(seed: Int) -> ByteArray {
  let a = blake2b_256(cbor.serialise(seed))
  let b = blake2b_256(a)
  let c = blake2b_256(b)
  let d = blake2b_256(c)
  a
    |> builtin.append_bytearray(b)
    |> builtin.append_bytearray(c)
    |> builtin.append_bytearray(d)
}

/// A deterministic ladder of `count` all-`Branch` proof steps — one forced
/// branch level each, the worst admissible shape per level.
pub fn adversarial_branch_steps(count: Int) -> mpf.Proof {
  if count <= 0 {
    []
  } else {
    list.push(adversarial_branch_steps(count - 1), branch_step(count - 1))
  }
}

fn branch_step(seed: Int) -> mpf.ProofStep {
  mpf.Branch { skip: 0, neighbors: branch_neighbors(seed) }
}

/// Independent reconstruction of the root an all-`Branch` membership proof of
/// `(key, value)` proves. Independent because it mirrors the MPF fold directly
/// rather than calling the verifier the selectors are testing.
pub fn adversarial_membership_root(
  key: ByteArray,
  value: ByteArray,
  steps: mpf.Proof,
) -> ByteArray {
  fold_membership_root(blake2b_256(key), blake2b_256(value), 0, steps)
}

fn fold_membership_root(
  path: ByteArray,
  value_hash: ByteArray,
  cursor: Int,
  steps: mpf.Proof,
) -> ByteArray {
  when steps is {
    [] -> combine(suffix(path, cursor), value_hash)
    [mpf.Branch { skip, neighbors }, ..rest] -> {
      let next_cursor = cursor + 1 + skip
      let child_root = fold_membership_root(path, value_hash, next_cursor, rest)
      combine(
        nibbles(path, cursor, next_cursor - 1),
        merkle_16(
          nibble(path, next_cursor - 1),
          child_root,
          builtin.slice_bytearray(0, 32, neighbors),
          builtin.slice_bytearray(32, 32, neighbors),
          builtin.slice_bytearray(64, 32, neighbors),
          builtin.slice_bytearray(96, 32, neighbors),
        ),
      )
    }
    // The ladder is all- by construction; the other step shapes are
    // enumerated rather than wildcarded so no unenumerated case can be
    // accepted here (family-scaffold rule ).
    [mpf.Fork { .. }, ..] -> merkling.null_hash
    [mpf.Leaf { .. }, ..] -> merkling.null_hash
  }
}

/// Re-commits a block fixture so its header commits `transactions_phas_root`
/// under `l2_transaction_count`, leaving the committed transaction untouched.
///
/// This is how a selector reaches a trie deep enough that its membership proof
/// cannot travel inside the step transaction: the counted-root authentication
/// the step performs is genuine, and only the depth is synthetic.
pub fn deep_transactions_root_block_fixture_v1(
  base: NativeBlockFixtureV1,
  transactions_phas_root: ByteArray,
  l2_transaction_count: Int,
) -> NativeBlockFixtureV1 {
  let header = ledger_state.HeaderV1 {
    ..base.header,
    transactions_root: transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      transactions_phas_root,
      l2_transaction_count,
    ),
    l2_transaction_count,
  }
  let header_data: Data = header
  NativeBlockFixtureV1 {
    ..base,
    transactions_phas_root,
    header,
    header_hash: utils.serialise_and_hash_28(header_data),
  }
}

/// Publication UTxOs for `steps`, packed to the per-chunk step bound. A chunk
/// is inert data at an arbitrary address: no token, no owner, no ordering
/// claim.
pub fn proof_chunk_reference_inputs(steps: mpf.Proof) -> List<Input> {
  do_proof_chunk_reference_inputs(steps, 0)
}

fn do_proof_chunk_reference_inputs(steps: mpf.Proof, index: Int) -> List<Input> {
  if steps == [] {
    []
  } else {
    let head_steps = list.take(steps, chunk_step_bound)
    let tail_steps = list.drop(steps, chunk_step_bound)
    let chunk_datum: Data = mpf_chunked_proof_v1.ProofChunkDatum {
      proof_steps: head_steps,
    }
    list.push(
      do_proof_chunk_reference_inputs(tail_steps, index + 1),
      Input {
        output_reference: OutputReference {
          transaction_id: blake2b_256(cbor.serialise(index)),
          output_index: index,
        },
        output: Output {
          address: address.from_script(chunk_holder_script_hash),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(chunk_datum),
          reference_script: None,
        },
      },
    )
  }
}

const chunk_step_bound: Int =
  mpf_chunked_proof_v1.maximum_chunk_proof_step_count

const chunk_holder_script_hash =
  #"90909090909090909090909090909090909090909090909090909090"

/// The published-chunk inclusion carriage matching
/// `continue_step_chunked_l1_tx_v1`: the block's hub-oracle and state-queue
/// reference inputs occupy slots 0 and 1, so the chunks start at slot 2.
pub fn published_chunk_inclusion_carriage_v1(
  fixture: NativeBlockFixtureV1,
  chunk_count: Int,
) -> NativeTxInclusionCarriage {
  common.PublishedChunkInclusion(
    common.PublishedChunkInclusionArgs {
      input_index: 0,
      output_index: 0,
      hub_ref_input_index: 0,
      state_queue_node_ref_input_index: 1,
      native_tx_id: fixture.tx_id,
      native_tx_compact_cbor: fixture.compact_cbor,
      transactions_phas_root: fixture.transactions_phas_root,
      ordered_chunk_reference_input_indices: chunk_indices_v1(chunk_count),
    },
  )
}

/// Withdraw redeemer entry mirroring the merkelized `mpf_chunked_verify`
/// invocation for one published-chunk claim.
pub fn chunked_verify_withdraw_redeemer_entry(
  claim: chunked_inclusion_v1.ChunkedProofClaim,
) -> Pair<ScriptPurpose, Redeemer> {
  let claim_data: Data = claim
  Pair(Withdraw(Script(env.mpf_chunked_verify_validator_hash)), claim_data)
}

/// The membership claim a published-chunk step must find in the merkelized
/// verifier's redeemer: the block's authenticated raw root, the committed
/// transaction's id and value digest, and the chunk order the step named.
pub fn chunked_membership_claim_v1(
  fixture: NativeBlockFixtureV1,
  chunk_count: Int,
) -> chunked_inclusion_v1.ChunkedProofClaim {
  chunked_inclusion_v1.ChunkedProofClaim {
    mode: chunked_inclusion_v1.Membership,
    merkle_root: fixture.transactions_phas_root,
    key_bytes: fixture.tx_id,
    value_hash: blake2b_256(fixture.compact_cbor),
    ordered_chunk_reference_input_indices: chunk_indices_v1(chunk_count),
  }
}

fn chunk_indices_v1(chunk_count: Int) -> List<Int> {
  list.map(list.range(0, chunk_count - 1), fn(index) { index + 2 })
}

/// L1 transaction that continues a first-step computation thread against the
/// fixture block with the membership proof carried by published chunks: same
/// thread input/output and block reference inputs, the merkelized verifier's
/// claim in place of the `phas` membership redeemer, and the chunk UTxOs
/// appended as reference inputs.
pub fn continue_step_chunked_l1_tx_v1(
  fixture: NativeBlockFixtureV1,
  output_state_data: Data,
  chunks: List<Input>,
  claim: chunked_inclusion_v1.ChunkedProofClaim,
) -> Transaction {
  let asset_name = ct_token_asset_name(fixture)
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [next_step_output(asset_name, output_state_data)],
    reference_inputs: list.concat(
      [hub_reference_input(), state_queue_reference_input(fixture)],
      chunks,
    ),
    redeemers: [chunked_verify_withdraw_redeemer_entry(claim)],
  }
}

pub fn step_datum_without_state() -> ct.StepDatum<Data> {
  ct.StepDatum { fraud_prover: fraud_prover_key_hash, data: None }
}

// ## L1 transaction assembly

pub fn ct_token_value(asset_name: ByteArray) {
  assets.from_lovelace(2_000_000)
    |> assets.add(computation_thread_policy_id, asset_name, 1)
}

pub fn ct_thread_input(asset_name: ByteArray) -> Input {
  step_input_with_value(ct_token_value(asset_name))
}

/// Thread-step input at the step script address holding an arbitrary value
/// (used by token-coupling negatives).
pub fn step_input_with_value(value: assets.Value) -> Input {
  Input {
    output_reference: own_out_ref(),
    output: Output {
      address: address.from_script(step_script_hash),
      value,
      datum: NoDatum,
      reference_script: None,
    },
  }
}

pub fn next_step_output(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Output {
  step_output_with_value(ct_token_value(asset_name), output_state_data)
}

/// Next-step output carrying an arbitrary value with a well-formed
/// `StepDatum` (used by token-coupling negatives).
pub fn step_output_with_value(
  value: assets.Value,
  output_state_data: Data,
) -> Output {
  let output_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fraud_prover_key_hash,
    data: Some(output_state_data),
  }
  let output_datum_data: Data = output_datum
  Output {
    address: address.from_script(next_step_script_hash),
    value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  }
}

pub fn hub_reference_input() -> Input {
  hub_reference_input_with_nft_policy(hub_oracle_script_hash)
}

/// Hub-oracle reference input whose authenticating NFT is minted under an
/// arbitrary policy (identity-substitution negatives).
pub fn hub_reference_input_with_nft_policy(nft_policy: ByteArray) -> Input {
  let dummy_addr = address.from_script(h28_x)
  let hub_datum = hub_oracle.Datum {
    registered_operators: h28_x,
    active_operators: h28_x,
    retired_operators: h28_x,
    scheduler: h28_x,
    state_queue: state_queue_policy_id,
    fraud_proof_catalogue: h28_x,
    fraud_proof: fraud_proof_policy_id,
    deposit: h28_x,
    withdrawal: h28_x,
    tx_order: h28_x,
    settlement: h28_x,
    payout: h28_x,
    registered_operators_addr: dummy_addr,
    active_operators_addr: dummy_addr,
    retired_operators_addr: dummy_addr,
    scheduler_addr: dummy_addr,
    state_queue_addr: dummy_addr,
    fraud_proof_catalogue_addr: dummy_addr,
    fraud_proof_addr: fraud_proof_token_address(),
    deposit_addr: dummy_addr,
    withdrawal_addr: dummy_addr,
    tx_order_addr: dummy_addr,
    settlement_addr: dummy_addr,
    reserve_addr: dummy_addr,
    payout_addr: dummy_addr,
    reserve_observer: h28_x,
  }
  let hub_datum_data: Data = hub_datum
  Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 0 },
    output: Output {
      address: address.from_script(hub_oracle_script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(nft_policy, hub_oracle.asset_name, 1),
      datum: InlineDatum(hub_datum_data),
      reference_script: None,
    },
  }
}

pub fn state_queue_reference_input(fixture: NativeBlockFixtureV1) -> Input {
  state_queue_reference_input_with_policy(fixture, state_queue_policy_id)
}

/// State-queue node reference input whose authenticating NFT is minted under
/// an arbitrary policy (block-registry substitution negatives).
pub fn state_queue_reference_input_with_policy(
  fixture: NativeBlockFixtureV1,
  nft_policy: ByteArray,
) -> Input {
  let node = state_queue.StateQueueNode {
    header: fixture.header,
    da_attestation: state_queue.no_da_attestation,
  }
  let element: linked_list.Element<Data, state_queue.StateQueueNode> = linked_list.Element {
    data: linked_list.Node { data: node },
    link: None,
  }
  let element_data: Data = element
  Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 1 },
    output: Output {
      address: address.from_script(nft_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            nft_policy,
            state_queue.block_asset_name_prefix
              |> builtin.append_bytearray(fixture.header_hash),
            1,
          ),
      datum: InlineDatum(element_data),
      reference_script: None,
    },
  }
}

/// Deployed fault-proof catalogue identity used by init fixtures.
pub const catalogue_script_hash =
  #"77777777777777777777777777777777777777777777777777777777"

pub fn catalogue_reference_input(catalogue_root: ByteArray) -> Input {
  catalogue_reference_input_with_nft_policy(
    catalogue_root,
    catalogue_script_hash,
  )
}

/// Catalogue reference input whose authenticating NFT is minted under an
/// arbitrary policy (catalogue substitution negatives).
pub fn catalogue_reference_input_with_nft_policy(
  catalogue_root: ByteArray,
  nft_policy: ByteArray,
) -> Input {
  let catalogue_datum_data: Data = catalogue_root
  Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 2 },
    output: Output {
      address: address.from_script(catalogue_script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(nft_policy, fraud_proof_catalogue.asset_name, 1),
      datum: InlineDatum(catalogue_datum_data),
      reference_script: None,
    },
  }
}

/// L1 transaction that cancels a computation thread: spends the thread input
/// and invokes the thread policy's `BurnForCancellation` redeemer for
/// `burn_asset_name`, optionally signed by the fraud prover.
pub fn cancel_step_l1_tx_v1(
  asset_name: ByteArray,
  burn_asset_name: ByteArray,
  signed_by_prover: Bool,
) -> Transaction {
  let mint_redeemer: Data = ct.BurnForCancellation {
    burning_token_asset_name: burn_asset_name,
  }
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    redeemers: [Pair(Mint(computation_thread_policy_id), mint_redeemer)],
    extra_signatories: if signed_by_prover {
      [fraud_prover_key_hash]
    } else {
      []
    },
  }
}

/// Withdraw redeemer entry mirroring the delegated Plutarch `phas` script
/// invocation for the given raw-root membership claim.
pub fn phas_withdraw_redeemer_entry(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> Pair<ScriptPurpose, Redeemer> {
  let membership_proof: mpf.Proof = []
  let proof_data: Data = membership_proof
  let payload: List<Data> = [
    builtin.b_data(merkle_root),
    builtin.b_data(key_bytes),
    builtin.b_data(value_bytes),
    proof_data,
  ]
  Pair(
    Withdraw(Script(env.plutarch_phas_validator_hash)),
    builtin.list_data(payload),
  )
}

/// L1 transaction that continues a first-step computation thread against the
/// fixture block: computation-thread input/output, hub-oracle and state-queue
/// reference inputs, and the delegated membership withdraw redeemer.
pub fn continue_step_l1_tx_v1(
  fixture: NativeBlockFixtureV1,
  output_state_data: Data,
  membership_root: ByteArray,
  membership_key: ByteArray,
  membership_value: ByteArray,
) -> Transaction {
  let asset_name = ct_token_asset_name(fixture)
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [next_step_output(asset_name, output_state_data)],
    reference_inputs: [
      hub_reference_input(),
      state_queue_reference_input(fixture),
    ],
    redeemers: [
      phas_withdraw_redeemer_entry(
        membership_root,
        membership_key,
        membership_value,
      ),
    ],
  }
}

/// `continue_step_l1_tx_v1` with §8 carriage reference inputs **appended** after
/// the two the inclusion args name.
///
/// Appended rather than prepended because `native_inclusion_args_v1` pins the
/// hub oracle at slot 0 and the state-queue node at slot 1; a carriage layout
/// that took those slots would point the door at the block evidence. The
/// matching redeemer-side offset is `certified_carriage_from_v1(2, …)`.
pub fn continue_step_l1_tx_with_carriage_v1(
  fixture: NativeBlockFixtureV1,
  output_state_data: Data,
  carriage_reference_inputs: List<Input>,
) -> Transaction {
  let asset_name = ct_token_asset_name(fixture)
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [next_step_output(asset_name, output_state_data)],
    reference_inputs: list.concat(
      [hub_reference_input(), state_queue_reference_input(fixture)],
      carriage_reference_inputs,
    ),
    redeemers: [
      phas_withdraw_redeemer_entry(
        fixture.transactions_phas_root,
        fixture.tx_id,
        fixture.compact_cbor,
      ),
    ],
  }
}

/// L1 transaction that continues a mid-pipeline computation thread (no block
/// reference inputs and no delegated membership redeemer).
pub fn continue_mid_step_l1_tx_v1(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [next_step_output(asset_name, output_state_data)],
  }
}

/// `finalize_step_l1_tx_v1` with carriage reference inputs attached — the shape
/// a step that opens a field under tier 2 or tier 3 has to be run against,
/// because the carriage lives in the L1 transaction's `reference_inputs` and
/// the redeemer names slots into it.
pub fn finalize_step_l1_tx_with_reference_inputs_v1(
  asset_name: ByteArray,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction { ..finalize_step_l1_tx_v1(asset_name), reference_inputs }
}

/// L1 transaction that finalizes a computation thread into a fraud-proof NFT
/// at the always-fails fraud-proof address.
pub fn finalize_step_l1_tx_v1(asset_name: ByteArray) -> Transaction {
  let fraud_proof_datum: Data = fraud_proof.Datum {
    fraud_prover: fraud_prover_key_hash,
  }
  let mint_redeemer: Data = fraud_proof.MintRedeemer {
    computation_thread_token_asset_name: asset_name,
    computation_thread_mint_redeemer_index: 0,
  }
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [
      Output {
        address: fraud_proof_token_address(),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(fraud_proof_policy_id, asset_name, 1),
        datum: InlineDatum(fraud_proof_datum),
        reference_script: None,
      },
    ],
    redeemers: [Pair(Mint(fraud_proof_policy_id), mint_redeemer)],
  }
}
