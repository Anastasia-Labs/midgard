use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_signature/forced_signer.{State}
use midgard/fraud_proofs/missing_signature/forced_step.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1} as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  FeeBelowMinimum, ForcedTxInvalid, ForcedTxValid, RequiredSignerUnsigned,
}
use midgard/transition_trace

validator main(
  forced_signer_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        header,
        membership,
        direction,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_name, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(1) = input_state
        let leaf = membership.value
        let header_data: Data = header
        expect and {
            direction == 1,
            utils.serialise_and_hash_28(header_data) == bytearray.drop(
              thread_name,
              fraud_proof_catalogue.id_byte_count,
            ),
            transition_trace.verify_root_membership_with_bytes(
              membership,
              transition_trace.ForcedTransactionsV1RootDomain,
              header.forced_transactions_root,
              header.forced_transaction_count,
              cbor.serialise(membership.key),
              cbor.serialise(leaf),
            ),
          }
        expect ForcedTxInvalid{reason: RequiredSignerUnsigned { signer_index }}
          = leaf.verdict
        let Pair(verified, _) = verify_native_tx_proof_source_v1(
          leaf.tx_id,
          leaf.source.compact_cbor,
          leaf.source.witness_set_compact_cbor,
          leaf.source.field_preimage_lengths_cbor,
        )
        expect verified.tx_compact.validity_code == 1
        let expected: Data = State {
          verified_tx_id: leaf.tx_id,
          verified_witness_set_hash: verified.tx_compact.witness_set_hash,
          forced_source_key: cbor.serialise(membership.key),
          signer_index,
        }
        and {
          output_hash == forced_signer_validator_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

const forced_order_key_transaction_id =
  #"7777777777777777777777777777777777777777777777777777777777777777"

const other_transaction_id =
  #"8888888888888888888888888888888888888888888888888888888888888888"

fn forced_order_key() -> ledger_state.TxOrderId {
  OutputReference {
    transaction_id: forced_order_key_transaction_id,
    output_index: 0,
  }
}

/// The forced-inclusion transaction the operator recorded as rejected. Its
/// compact `validity_code` is therefore 1 — the polarity the door demands.
fn rejected_forced_tx(network_id: Int) -> native_types.MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..base,
    body: native_types.MidgardTransactionBody { ..base.body, network_id },
    validity: native_types.TxIsInvalid,
  }
}

fn accepted_forced_tx(network_id: Int) -> native_types.MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..base,
    body: native_types.MidgardTransactionBody { ..base.body, network_id },
    validity: native_types.TxIsValid,
  }
}

fn proof_source_of(
  tx: native_types.MidgardTransaction,
) -> ledger_state.NativeTxProofSourceV1 {
  let source = native_transaction.midgard_transaction_proof_source_v1(tx)
  ledger_state.NativeTxProofSourceV1 {
    compact_cbor: source.compact_cbor,
    witness_set_compact_cbor: source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: source.field_preimage_lengths_cbor,
  }
}

fn forced_leaf(
  tx: native_types.MidgardTransaction,
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> ledger_state.ForcedInclusionTxV1 {
  ledger_state.ForcedInclusionTxV1 {
    tx_id: fixture.native_block_fixture_v1(tx).tx_id,
    source: proof_source_of(tx),
    verdict,
  }
}

fn forced_membership(
  key: ledger_state.TxOrderId,
  leaf: ledger_state.ForcedInclusionTxV1,
  count: Int,
) -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(leaf), []),
  )
  transition_trace.RootMembershipProof {
    domain: transition_trace.ForcedTransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      phas_root,
      count,
    ),
    phas_root,
    count,
    key,
    value: leaf,
    proof: [],
  }
}

fn forced_header(root: ByteArray, count: Int) -> ledger_state.HeaderV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  ledger_state.HeaderV1 {
    ..base.header,
    forced_transactions_root: root,
    forced_transaction_count: count,
  }
}

fn thread_name(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  bytearray.concat(
    fixture.fraud_category_id,
    utils.serialise_and_hash_28(header_data),
  )
}

fn expected_forced_state(
  leaf: ledger_state.ForcedInclusionTxV1,
  key: ledger_state.TxOrderId,
  _network: Int,
) -> State {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    leaf.tx_id,
    leaf.source.compact_cbor,
    leaf.source.witness_set_compact_cbor,
    leaf.source.field_preimage_lengths_cbor,
  )
  State {
    verified_tx_id: leaf.tx_id,
    verified_witness_set_hash: verified.tx_compact.witness_set_hash,
    forced_source_key: cbor.serialise(key),
    signer_index: 0,
  }
}

fn run_forced_step(
  successor_script_hash: ScriptHash,
  input_state: Option<Int>,
  header: ledger_state.HeaderV1,
  thread_name: ByteArray,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
  input_index: Int,
  output_index: Int,
  output_state: Data,
) -> Bool {
  main.spend(
    successor_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: input_state,
      },
    ),
    ct.Continue(
      Args { input_index, output_index, header, membership, direction },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(thread_name, output_state),
  )
}

/// The canonical wrongful-rejection binding: a forced leaf the operator
/// rejected for `NetworkIdMismatch`, whose transaction actually carries the
/// deployment network.
fn run_canonical_forced_bind(leaf_network_id: Int) -> Bool {
  let tx = rejected_forced_tx(leaf_network_id)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, leaf_network_id)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_binds_a_rejected_forced_leaf() {
  run_canonical_forced_bind(0)
}

test missing_signature_forced_step_rejects_direction_zero() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    0,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_direction_two() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    2,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_substituted_header() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  // The thread NFT names a different block than the redeemer-carried header.
  let other_header = ledger_state.HeaderV1 {
    ..header,
    block_slot: header.block_slot + 1,
  }
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    other_header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_substituted_forced_root() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(env.empty_merkle_tree_root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_substituted_forced_count() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 2)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

/// A leaf committed under a foreign order key cannot be opened at the key the
/// redeemer names, because the value bytes are keyed by `membership.key`.
test missing_signature_forced_step_rejects_a_substituted_order_key() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let forged_key = OutputReference {
    transaction_id: other_transaction_id,
    output_index: 0,
  }
  let state: Data = expected_forced_state(leaf, forged_key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    transition_trace.RootMembershipProof { ..membership, key: forged_key },
    1,
    0,
    0,
    state,
  )
}

/// Substituted leaf bytes: the committed `tx_id` no longer re-derives from the
/// committed source, so `verify_native_tx_proof_source_v1` refuses.
test missing_signature_forced_step_rejects_a_substituted_leaf_tx_id() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let base = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let leaf = ledger_state.ForcedInclusionTxV1 {
    ..base,
    tx_id: other_transaction_id,
  }
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

/// Substituted source: an authentic id paired with another transaction's
/// proof-source triple.
test missing_signature_forced_step_rejects_a_substituted_leaf_source() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let base = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let leaf = ledger_state.ForcedInclusionTxV1 {
    ..base,
    source: proof_source_of(rejected_forced_tx(1)),
  }
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

/// Substituted witness bytes: the compact structure's `witness_set_hash` no
/// longer covers the carried witness-set preimage.
test missing_signature_forced_step_rejects_substituted_witness_set_bytes() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let base = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let leaf = ledger_state.ForcedInclusionTxV1 {
    ..base,
    source: ledger_state.NativeTxProofSourceV1 {
      ..base.source,
      witness_set_compact_cbor: native_compact.encode_native_tx_witness_set_compact(
        fixture.empty_witness_set_compact(),
      ),
    },
  }
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

/// Malformed checkpoint: a non-canonically encoded field-preimage-length
/// vector cannot re-encode to itself.
test missing_signature_forced_step_rejects_a_malformed_field_length_vector() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let base = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let leaf = ledger_state.ForcedInclusionTxV1 {
    ..base,
    source: ledger_state.NativeTxProofSourceV1 {
      ..base.source,
      field_preimage_lengths_cbor: bytearray.concat(
        base.source.field_preimage_lengths_cbor,
        #"00",
      ),
    },
  }
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

/// An accepted source (`validity_code == 0`) has no rejection to contradict.
test missing_signature_forced_step_rejects_an_accepted_source_validity_code() fail {
  let tx = accepted_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_an_accepting_verdict() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(tx, ForcedTxValid)
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_foreign_rejection_reason() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(tx, ForcedTxInvalid { reason: FeeBelowMinimum })
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_missing_handoff_marker() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    None,
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_foreign_handoff_marker() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(2),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_a_wrong_successor() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    0,
    state,
  )
}

test missing_signature_forced_step_rejects_an_out_of_range_output_index() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    0,
    1,
    state,
  )
}

test missing_signature_forced_step_rejects_an_out_of_range_input_index() fail {
  let tx = rejected_forced_tx(0)
  let key = forced_order_key()
  let leaf = forced_leaf(
    tx,
    ForcedTxInvalid { reason: RequiredSignerUnsigned { signer_index: 0 } },
  )
  let membership = forced_membership(key, leaf, 1)
  let header = forced_header(membership.root, 1)
  let state: Data = expected_forced_state(leaf, key, 0)
  run_forced_step(
    fixture.next_step_script_hash,
    Some(1),
    header,
    thread_name(header),
    membership,
    1,
    1,
    0,
    state,
  )
}
