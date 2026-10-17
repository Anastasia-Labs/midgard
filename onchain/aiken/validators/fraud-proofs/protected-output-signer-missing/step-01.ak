use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/compact.{
  verify_forced_tx_proof_source_v1, verify_native_tx_compact_cbor_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
  PubKeyCredential, TxIsInvalid,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule.{bind_output_v1}
use midgard/fraud_proofs/protected_output_signer_missing/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_02
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  ForcedTxInvalid, ForcedTxValid, ProtectedOutputSignerMissing,
  SpendInputSignerMissing,
}
use midgard/transition_trace

validator main(
  step_02_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { source: AcceptedSource { inclusion }, output_index }) -> {
        let
            _own,
            _name,
            _prover,
            _prior,
            next_hash,
            next_data,
            _header,
            _id,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let bound = bind_output_v1(
          substrate.bind_accepted_subject_v1(verified),
          output_index,
        )
        let expected: Data = step_02.State {
          bound,
          witness_set_hash: verified.tx_compact.witness_set_hash,
        }
        and {
          next_hash == step_02_hash,
          next_data == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource {
          input_index,
          output_index: next_index,
          header,
          membership,
          direction,
        },
        output_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, name, _prover, _prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          next_index,
          own_ref,
          inputs,
          outputs,
        )
        let subject = substrate.bind_forced_subject_to_thread_v1(
          name,
          header,
          membership,
          direction,
        )
        let Pair(verified, _witness_set) = verify_forced_tx_proof_source_v1(
          membership.value.tx_id,
          membership.value.submitted_source.compact_cbor,
          membership.value.submitted_source.witness_set_compact_cbor,
          membership.value.submitted_source.field_preimage_lengths_cbor,
        )
        let expected: Data = step_02.State {
          bound: bind_output_v1(subject, output_index),
          witness_set_hash: verified.tx_compact.witness_set_hash,
        }
        and {
          next_hash == step_02_hash,
          next_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

// ### Selectors
//
// The bind step proves nothing about the witness set: it authenticates the
// source (counted `transactions_root` membership, or the forced leaf under the
// header's counted `forced_transactions_root`), fixes the direction, and — for
// a wrongful rejection — binds the redeemer's output coordinate to exactly
// the leaf's `ProtectedOutputSignerMissing { output_index }`.

const protected_output_index: Int = 0

fn protected_output() -> MidgardTxOutput {
  MidgardTxOutput {
    ..fixture.sample_native_output(),
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: PubKeyCredential(
        #"30303030303030303030303030303030303030303030303030303030",
      ),
      stake_credential: None,
    },
  }
}

fn accepted_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [protected_output()] },
  }
}

/// The forced leaf the operator rejected: its compact `validity_code` is 1,
/// the polarity the forced door demands for a wrongful rejection.
fn rejected_tx() -> MidgardTransaction {
  MidgardTransaction { ..accepted_tx(), validity: TxIsInvalid }
}

fn expected_accepted_state(block: fixture.NativeBlockFixtureV1) -> Data {
  let verified = verify_native_tx_compact_cbor_v1(
    block.tx_id,
    block.compact_cbor,
  )
  let state: Data = step_02.State {
    bound: bind_output_v1(
      substrate.bind_accepted_subject_v1(verified),
      protected_output_index,
    ),
    witness_set_hash: verified.tx_compact.witness_set_hash,
  }
  state
}

fn run_accepted(
  block: fixture.NativeBlockFixtureV1,
  carriage_block: fixture.NativeBlockFixtureV1,
  output_index: Int,
  l1_tx: Transaction,
) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.native_inclusion_carriage_v1(carriage_block),
        },
        output_index,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

fn accepted_l1_tx(block: fixture.NativeBlockFixtureV1) -> Transaction {
  fixture.continue_step_l1_tx_v1(
    block,
    expected_accepted_state(block),
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
}

/// The accepted direction binds a committed transaction and its output
/// coordinate, forwarding the counted-root witness-set anchor.
test protected_output_step_01_binds_an_accepted_transaction() {
  let block = fixture.native_block_fixture_v1(accepted_tx())
  run_accepted(block, block, protected_output_index, accepted_l1_tx(block))
}

/// Transaction membership: a raw transactions root the header did not commit
/// cannot bind, whatever proof rides with it.
test protected_output_step_01_refuses_a_foreign_transactions_root() fail {
  let block = fixture.native_block_fixture_v1(accepted_tx())
  let forged = fixture.NativeBlockFixtureV1 {
    ..block,
    transactions_phas_root: #"4040404040404040404040404040404040404040404040404040404040404040",
  }
  run_accepted(block, forged, protected_output_index, accepted_l1_tx(block))
}

/// A negative output coordinate is refused before anything is opened.
test protected_output_step_01_refuses_a_negative_output_coordinate() fail {
  let block = fixture.native_block_fixture_v1(accepted_tx())
  run_accepted(block, block, -1, accepted_l1_tx(block))
}

/// The output coordinate written to thread state is the redeemer's: a datum
/// naming another coordinate is not the successor this step derives.
test protected_output_step_01_refuses_a_substituted_bound_coordinate() fail {
  let block = fixture.native_block_fixture_v1(accepted_tx())
  run_accepted(block, block, 1, accepted_l1_tx(block))
}

/// The bound state may only continue at step 02.
test protected_output_step_01_refuses_a_wrong_successor() fail {
  let block = fixture.native_block_fixture_v1(accepted_tx())
  let l1_tx = Transaction {
    ..accepted_l1_tx(block),
    outputs: fixture.continue_same_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      expected_accepted_state(block),
    ).outputs,
  }
  run_accepted(block, block, protected_output_index, l1_tx)
}

// #### Forced leaf

fn forced_membership(
  leaf: ledger_state.ForcedInclusionTxV1,
) -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  tf.forced_leaf_membership_v1(tf.forced_order_key_v1(), leaf)
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
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
  output_index: Int,
) -> Data {
  let leaf = membership.value
  let Pair(verified, _) = verify_forced_tx_proof_source_v1(
    leaf.tx_id,
    leaf.submitted_source.compact_cbor,
    leaf.submitted_source.witness_set_compact_cbor,
    leaf.submitted_source.field_preimage_lengths_cbor,
  )
  let state: Data = step_02.State {
    bound: bind_output_v1(
      substrate.bind_forced_subject_v1(header, membership, direction),
      output_index,
    ),
    witness_set_hash: verified.tx_compact.witness_set_hash,
  }
  state
}

fn run_forced(
  header: ledger_state.HeaderV1,
  name: ByteArray,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
  output_index: Int,
  output_state: Data,
) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: ForcedSource {
          input_index: 0,
          output_index: 0,
          header,
          membership,
          direction,
        },
        output_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(name, output_state),
  )
}

fn rejected_leaf(output_index: Int) -> ledger_state.ForcedInclusionTxV1 {
  tf.forced_leaf_v1(
    rejected_tx(),
    ForcedTxInvalid { reason: ProtectedOutputSignerMissing { output_index } },
  )
}

/// The canonical wrongful-rejection binding: the leaf rejects for exactly
/// this family's reason at the coordinate the redeemer names.
fn run_canonical_forced(
  leaf: ledger_state.ForcedInclusionTxV1,
  redeemer_output_index: Int,
) -> Bool {
  let membership = forced_membership(leaf)
  let header = forced_header(membership.root, 1)
  run_forced(
    header,
    thread_name(header),
    membership,
    substrate.direction_wrongful_rejection,
    redeemer_output_index,
    expected_forced_state(
      header,
      membership,
      substrate.direction_wrongful_rejection,
      redeemer_output_index,
    ),
  )
}

test protected_output_step_01_binds_a_rejected_forced_leaf() {
  run_canonical_forced(
    rejected_leaf(protected_output_index),
    protected_output_index,
  )
}

/// Reason coordinate: the leaf rejects output 0, the redeemer claims output 1.
test protected_output_step_01_refuses_a_substituted_reason_coordinate() fail {
  run_canonical_forced(rejected_leaf(protected_output_index), 1)
}

/// Reason constructor: a leaf rejected for the sibling spend-input reason is
/// not this family's subject, even at the same coordinate.
test protected_output_step_01_refuses_a_foreign_rejection_reason() fail {
  run_canonical_forced(
    tf.forced_leaf_v1(
      rejected_tx(),
      ForcedTxInvalid {
        reason: SpendInputSignerMissing { input_index: protected_output_index },
      },
    ),
    protected_output_index,
  )
}

/// Direction polarity: a rejecting leaf cannot open a wrongful-acceptance
/// thread.
test protected_output_step_01_refuses_direction_zero_on_a_rejecting_leaf() fail {
  let membership = forced_membership(rejected_leaf(protected_output_index))
  let header = forced_header(membership.root, 1)
  run_forced(
    header,
    thread_name(header),
    membership,
    substrate.direction_wrongful_acceptance,
    protected_output_index,
    expected_forced_state(
      header,
      membership,
      substrate.direction_wrongful_rejection,
      protected_output_index,
    ),
  )
}

/// An accepting leaf carries no reason to contradict.
test protected_output_step_01_refuses_a_wrongful_rejection_on_an_accepting_leaf() fail {
  let leaf = tf.forced_leaf_v1(accepted_tx(), ForcedTxValid)
  let membership = forced_membership(leaf)
  let header = forced_header(membership.root, 1)
  run_forced(
    header,
    thread_name(header),
    membership,
    substrate.direction_wrongful_rejection,
    protected_output_index,
    expected_forced_state(
      header,
      membership,
      substrate.direction_wrongful_acceptance,
      protected_output_index,
    ),
  )
}

/// Forced leaf: a header committing another forced root does not hold this
/// leaf.
test protected_output_step_01_refuses_a_substituted_forced_root() fail {
  let membership = forced_membership(rejected_leaf(protected_output_index))
  let header = forced_header(
    #"5151515151515151515151515151515151515151515151515151515151515151",
    1,
  )
  run_forced(
    header,
    thread_name(header),
    membership,
    substrate.direction_wrongful_rejection,
    protected_output_index,
    expected_forced_state(
      forced_header(membership.root, 1),
      membership,
      substrate.direction_wrongful_rejection,
      protected_output_index,
    ),
  )
}

/// The counted root binds the count as well as the raw root.
test protected_output_step_01_refuses_a_substituted_forced_count() fail {
  let membership = forced_membership(rejected_leaf(protected_output_index))
  let header = forced_header(membership.root, 2)
  run_forced(
    header,
    thread_name(header),
    membership,
    substrate.direction_wrongful_rejection,
    protected_output_index,
    expected_forced_state(
      forced_header(membership.root, 1),
      membership,
      substrate.direction_wrongful_rejection,
      protected_output_index,
    ),
  )
}

/// The header must be the one the thread NFT names.
test protected_output_step_01_refuses_a_header_the_thread_did_not_name() fail {
  let membership = forced_membership(rejected_leaf(protected_output_index))
  let header = forced_header(membership.root, 1)
  run_forced(
    header,
    thread_name(forced_header(membership.root, 2)),
    membership,
    substrate.direction_wrongful_rejection,
    protected_output_index,
    expected_forced_state(
      header,
      membership,
      substrate.direction_wrongful_rejection,
      protected_output_index,
    ),
  )
}

/// Leaf source: proof-source bytes of another transaction under the leaf's
/// transaction id do not re-derive to it.
test protected_output_step_01_refuses_a_substituted_leaf_source() fail {
  let honest = rejected_leaf(protected_output_index)
  let foreign = tf.forced_leaf_v1(
    MidgardTransaction {
      ..rejected_tx(),
      body: MidgardTransactionBody { ..rejected_tx().body, fee: 2_000_000 },
    },
    ForcedTxInvalid {
      reason: ProtectedOutputSignerMissing {
        output_index: protected_output_index,
      },
    },
  )
  let substituted = ledger_state.ForcedInclusionTxV1 {
    ..honest,
    submitted_source: foreign.submitted_source,
  }
  let membership = forced_membership(substituted)
  let header = forced_header(membership.root, 1)
  run_forced(
    header,
    thread_name(header),
    membership,
    substrate.direction_wrongful_rejection,
    protected_output_index,
    expected_forced_state(
      forced_header(forced_membership(honest).root, 1),
      forced_membership(honest),
      substrate.direction_wrongful_rejection,
      protected_output_index,
    ),
  )
}

/// The forced binding may only continue at step 02.
test protected_output_step_01_refuses_a_forced_wrong_successor() fail {
  let membership = forced_membership(rejected_leaf(protected_output_index))
  let header = forced_header(membership.root, 1)
  let name = thread_name(header)
  let state = expected_forced_state(
    header,
    membership,
    substrate.direction_wrongful_rejection,
    protected_output_index,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: ForcedSource {
          input_index: 0,
          output_index: 0,
          header,
          membership,
          direction: substrate.direction_wrongful_rejection,
        },
        output_index: protected_output_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_same_step_l1_tx_v1(name, state),
  )
}
