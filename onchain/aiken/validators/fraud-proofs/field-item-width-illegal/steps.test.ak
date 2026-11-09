//// Source-bound FIWI selectors traverse the real three spending handlers.
//// Terminal expectations are independent of the state derived by step 02.

use aiken/builtin
use aiken/primitive/bytearray
use cardano/address
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use fraud_proofs/field_item_width_illegal/step_01 as validator_01
use fraud_proofs/field_item_width_illegal/step_02 as validator_02
use fraud_proofs/field_item_width_illegal/step_03 as validator_03
use midgard/computation_thread as ct
use midgard/field_grammar_certificate_v1.{FieldGrammarCertificateV1}
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/field_item_width_illegal/rule
use midgard/fraud_proofs/field_item_width_illegal/step_01
use midgard/fraud_proofs/field_item_width_illegal/step_02
use midgard/fraud_proofs/field_item_width_illegal/step_03
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyFieldOpening, FieldOpeningV1, WitnessAnchor,
  WitnessFieldOpening, opened_whole_field_view,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as forced_fixture
use midgard/fraud_proofs/native_tx/compact
use midgard/fraud_proofs/native_tx/preimages
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{TxCompactView}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/native_tx_field_access_v1.{Inline}
use midgard/rejection_reason_v1.{FieldItemWidthIllegal, ForcedTxInvalid}

// Definite CBOR encoded independently of Midgard; Ed25519 signatures use
// public fixed seeds 01/02. Validation's signature-order.test.ts replays the
// same bytes through admission and the actual accepted signature trace.
const descending_tx =
  #"84018c582981582682582011111111111111111111111111111111111111111111111111111111111111111900004180582c815829a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53008358cf8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b3945840aa08f5f3164fbe3da499efa9bc02d9ccb302c34e05b0089e82f2ffb30cbbb4732e9beaf9ce196bb62959de0f05e7c73de24f8d5d266758a74956d011b88e1a0f4180418000"

const duplicate_tx =
  #"84018c582981582682582011111111111111111111111111111111111111111111111111111111111111111900004180582c815829a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53008358cf8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e014180418000"

const signed_tx_id =
  #"4735658298f785608b21ec2f2de1b7b11e0722b3c981f6abb306f8b1dc2b6479"

const descending_field =
  #"8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b3945840aa08f5f3164fbe3da499efa9bc02d9ccb302c34e05b0089e82f2ffb30cbbb4732e9beaf9ce196bb62959de0f05e7c73de24f8d5d266758a74956d011b88e1a0f"

const duplicate_field =
  #"8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e01"

fn next_datum(output: Output) -> Data {
  expect InlineDatum(datum) = output.datum
  datum
}

fn with_previous_output(tx: Transaction, previous: Output) -> Transaction {
  Transaction {
    ..tx,
    inputs: [
      Input { output_reference: fixture.own_out_ref(), output: previous },
    ],
  }
}

// The input is the actual serialized output whose equality step 01 checked.
fn finish_chain(
  first_output: Output,
  asset_name: ByteArray,
  opening: Option<FieldOpeningV1>,
) -> Bool {
  expect bound_datum: step_02.Datum = next_datum(first_output)
  expect Some(bound) = bound_datum.data
  let authenticated = if rule.coordinate_is_supported_v1(
    bound.field_index,
    bound.item_index,
  ) {
    expect Some(field_opening) = opening
    let anchor = if bound.field_index == 7 {
      WitnessAnchor {
        tx_id: bound.subject.transaction_id,
        source_kind: bound.subject.source_kind,
        witness_set_hash: bound.compact.witness_set_hash,
      }
    } else {
      BodyAnchor {
        tx_id: bound.subject.transaction_id,
        source_kind: bound.subject.source_kind,
      }
    }
    rule.authenticate_item_legality_v1(
      bound,
      opened_whole_field_view(
        field_opening,
        anchor,
        bound.field_index,
        [],
        fixture.test_certificate_policy_id,
      ),
    )
  } else {
    rule.authenticate_static_domain_v1(bound)
  }
  let authenticated_data: Data = authenticated
  let second_output = Output {
    ..fixture.next_step_output(asset_name, authenticated_data),
    address: address.from_script(fixture.step_script_hash),
  }
  let second_tx = Transaction {
    ..with_previous_output(
      fixture.continue_mid_step_l1_tx_v1(asset_name, authenticated_data),
      first_output,
    ),
    outputs: [second_output],
  }
  expect
    validator_02.main.spend(
      fixture.step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      grammar.test_grammar_certificate_policy_id,
      Some(bound_datum),
      ct.Continue(
        step_02.Args {
          input_index: 0,
          output_index: 0,
          opening,
          grammar_ref_input_index: None,
        },
      ),
      fixture.own_out_ref(),
      second_tx,
    )
  expect authenticated_datum: step_03.Datum = next_datum(second_output)
  validator_03.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(authenticated_datum),
    ct.Continue(
      step_03.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    with_previous_output(
      fixture.finalize_step_l1_tx_v1(asset_name),
      second_output,
    ),
  )
}

fn accepted_chain(duplicate: Bool, malformed: Bool, substitution: Int) -> Bool {
  let tx = native_transaction.decode_midgard_transaction_v1(
    if duplicate {
      duplicate_tx
    } else {
      descending_tx
    },
  )
  let base = fixture.native_block_fixture_v1(tx)
  expect base.tx_id == signed_tx_id
  expect [first, second] = tx.witness_set.addr_tx_wits
  expect
    builtin.verify_ed25519_signature(
      first.verification_key,
      base.tx_id,
      first.signature,
    )
  expect
    builtin.verify_ed25519_signature(
      second.verification_key,
      base.tx_id,
      second.signature,
    )
  let original = if duplicate {
    duplicate_field
  } else {
    descending_field
  }
  let field = if malformed {
    bytearray.take(original, 105)
      |> bytearray.concat(#"64")
      |> bytearray.concat(bytearray.drop(original, 106))
  } else {
    original
  }
  let block = fixture.committed_field_preimage_block_fixture_v1(base, 7, field)
  let subject = substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: block.tx_id,
    source_key: #"",
    rejection_reason: None,
  }
  let bound = rule.bind_coordinate_v1(
    subject,
    7,
    1,
    TxCompactView {
      body: block.compact.body,
      witness_set_hash: block.compact.witness_set_hash,
      validity_code: Some(block.compact.validity_code),
    },
  )
  let bound_data: Data = bound
  let first_tx = fixture.continue_step_l1_tx_v1(
    block,
    bound_data,
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  expect
    validator_01.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.hub_oracle_script_hash,
      Some(
        ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None },
      ),
      ct.Continue(
        step_01.Args {
          source: step_01.AcceptedSource {
            inclusion: fixture.native_inclusion_carriage_v1(block),
          },
          field_index: 7,
          item_index: 1,
        },
      ),
      fixture.own_out_ref(),
      first_tx,
    )
  expect [first_output] = first_tx.outputs
  let alternative_field = bytearray.take(field, bytearray.length(field) - 1)
    |> bytearray.concat(#"00")
  let opening_block = if substitution == 2 {
    fixture.committed_field_preimage_block_fixture_v1(
      base,
      7,
      alternative_field,
    )
  } else {
    block
  }
  finish_chain(
    first_output,
    fixture.ct_token_asset_name(block),
    Some(
      WitnessFieldOpening {
        native_tx_compact_cbor: opening_block.compact_cbor,
        witness_set: opening_block.witness_set_compact,
        carriage: Inline {
          preimage: if substitution == 0 {
            field
          } else {
            alternative_field
          },
        },
      },
    ),
  )
}

fn forced_claim_chain(field_index: Int, item_index: Int) -> Bool {
  let tx = fixture.valid_native_tx_v1()
  let leaf = forced_fixture.forced_leaf_v1(
    tx,
    ForcedTxInvalid {
      reason: FieldItemWidthIllegal { field_index, item_index },
    },
  )
  let membership = forced_fixture.forced_leaf_membership_v1(
    forced_fixture.forced_order_key_v1(),
    leaf,
  )
  let claim = forced_fixture.thread_claim_v1(
    ledger_state.ForcedTransactionEventKey {
      tx_order_id: forced_fixture.forced_order_key_v1(),
    },
    ledger_state.ForcedTransaction,
    #"5555555555555555555555555555555555555555555555555555555555555555",
    Some(forced_fixture.forced_counted_root_v1(membership.phas_root, 1)),
  )
  let asset_name = forced_fixture.header_asset_name_v1(claim.header)
  let subject = substrate.open_subject_v1(
    asset_name,
    substrate.ForcedSource { header: claim.header, membership },
  )
  let forced = compact.decode_forced_tx_compact_v1(
    leaf.submitted_source.compact_cbor,
  )
  let bound = rule.bind_coordinate_v1(
    subject,
    field_index,
    item_index,
    TxCompactView {
      body: forced.body,
      witness_set_hash: forced.witness_set_hash,
      validity_code: None,
    },
  )
  let bound_data: Data = bound
  let first_tx = fixture.continue_mid_step_l1_tx_v1(asset_name, bound_data)
  expect
    validator_01.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.hub_oracle_script_hash,
      Some(
        ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None },
      ),
      ct.Continue(
        step_01.Args {
          source: step_01.ForcedSource {
            input_index: 0,
            output_index: 0,
            header: claim.header,
            membership,
          },
          field_index,
          item_index,
        },
      ),
      fixture.own_out_ref(),
      first_tx,
    )
  expect [first_output] = first_tx.outputs
  let opening = if rule.coordinate_is_supported_v1(field_index, item_index) {
    Some(
      BodyFieldOpening {
        native_tx_compact_cbor: leaf.submitted_source.compact_cbor,
        carriage: Inline {
          preimage: preimages.encode_hash28_list_preimage(
            tx.body.required_signers,
          ),
        },
      },
    )
  } else {
    None
  }
  finish_chain(first_output, asset_name, opening)
}

test fiwi_terminal_convicts_committed_descending_signed_witnesses() {
  accepted_chain(False, False, 0)
}

test fiwi_terminal_convicts_committed_duplicate_signed_witnesses() {
  accepted_chain(True, False, 0)
}

test fiwi_terminal_convicts_a_committed_malformed_witness_wrapper() {
  accepted_chain(False, True, 0)
}

test fiwi_terminal_refutes_a_legal_fixed_item_claim() {
  forced_claim_chain(4, 0)
}

test fiwi_terminal_refutes_an_exact_negative_item_without_a_field() {
  forced_claim_chain(4, -1)
}

test fiwi_terminal_refutes_an_exact_negative_field_without_a_field() {
  forced_claim_chain(-1, 0)
}

test fiwi_terminal_refutes_an_exact_huge_field_without_a_field() {
  forced_claim_chain(1208925819614629174706176, 0)
}

test fiwi_terminal_authenticates_count_for_a_huge_nonnegative_item() {
  forced_claim_chain(4, 1208925819614629174706176)
}

// Force the call, then return True: these fail tests can pass only by aborting,
// not merely because a downstream honest terminal returned False.
test fiwi_chain_rejects_substituted_field_bytes() fail {
  expect _ = accepted_chain(False, False, 1)
  True
}

test fiwi_chain_rejects_a_self_consistent_substituted_witness_set() fail {
  expect _ = accepted_chain(False, False, 2)
  True
}

// ## Step 02 through the field grammar certificate
//
// 761 items fill field 2 or 5 under tier 3. The coordinate sits at 751,
// fifteen items past its waypoint, so the width read pays the most header
// reads a certified read can.

const certified_count = 761

const certified_index = 751

type Step02Case {
  datum: Option<step_02.Datum>,
  redeemer: step_02.SpendRedeemer,
  tx: Transaction,
}

/// Step 02 over field `field_index` holding `item` at `certified_index`
/// among 41-byte items, at coordinate `index`, read through the certificate
/// `certificate(tx_id, preimage)` builds. `observation` is the state the
/// step must write.
fn certified_step_02(
  field_index: Int,
  item: ByteArray,
  index: Int,
  certificate: fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1,
  observation: rule.ItemObservationV1,
) -> Step02Case {
  let preimage = grammar.field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 41),
    certified_count,
    certified_index,
    item,
  )
  step_02_over(
    field_index,
    preimage,
    index,
    observation,
    fn(block) {
      Route {
        opening: fixture.certified_body_opening(block.compact_cbor, preimage),
        grammar_ref_input_index: Some(
          grammar.grammar_ref_input_index_v1(preimage),
        ),
        reference_inputs: grammar.certified_grammar_reference_inputs_v1(
          block.tx_id,
          field_index,
          preimage,
          certificate(block.tx_id, preimage),
        ),
      }
    },
  )
}

/// How a case opens the field: the opening, the grammar certificate's slot
/// and the L1 reference inputs, for the block committing it.
type Route {
  opening: FieldOpeningV1,
  grammar_ref_input_index: Option<Int>,
  reference_inputs: List<Input>,
}

/// Step 02 over field `field_index` committing `preimage`, at coordinate
/// `index`, opened by `route`. `observation` is the state the step must
/// write.
fn step_02_over(
  field_index: Int,
  preimage: ByteArray,
  index: Int,
  observation: rule.ItemObservationV1,
  route: fn(fixture.NativeBlockFixtureV1) -> Route,
) -> Step02Case {
  let block = fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    field_index,
    preimage,
  )
  let subject = substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: block.tx_id,
    source_key: #"",
    rejection_reason: None,
  }
  let bound = rule.BoundCoordinateV1 {
    subject,
    field_index,
    item_index: index,
    compact: TxCompactView {
      body: block.compact.body,
      witness_set_hash: block.compact.witness_set_hash,
      validity_code: Some(block.compact.validity_code),
    },
  }
  let next_state: Data = rule.AuthenticatedItemV1 {
    subject,
    field_index,
    item_index: index,
    observation,
  }
  let route = route(block)
  Step02Case {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      step_02.Args {
        input_index: 0,
        output_index: 0,
        opening: Some(route.opening),
        grammar_ref_input_index: route.grammar_ref_input_index,
      },
    ),
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        fixture.ct_token_asset_name(block),
        next_state,
      ),
      reference_inputs: route.reference_inputs,
    },
  }
}

fn run_certified_step_02(case: Step02Case) -> Bool {
  validator_02.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    grammar.test_grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

fn certificate_of(
  field_index: Int,
) -> fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1 {
  fn(tx_id, preimage) {
    grammar.complete_field_grammar_certificate_v1(
      tx_id,
      field_index,
      preimage,
      certified_count,
    )
  }
}

/// The empty mint item: a width field 5 never admits.
fn illegal_mint_case() -> Step02Case {
  certified_step_02(
    5,
    #"",
    certified_index,
    certificate_of(5),
    rule.ItemLegality { item_is_legal: False },
  )
}

test fiwi_step_02_certified_route_fixture_only_at_the_tier3_maximum() {
  illegal_mint_case().datum != None
}

test fiwi_step_02_certified_route_at_the_tier3_maximum_cost() {
  run_certified_step_02(illegal_mint_case())
}

test fiwi_step_02_certified_route_observes_a_legal_output_width() {
  run_certified_step_02(
    certified_step_02(
      2,
      fixture.repeated_bytes_v1(#"b2", 41),
      certified_index,
      certificate_of(2),
      rule.ItemLegality { item_is_legal: True },
    ),
  )
}

test fiwi_step_02_certified_route_refuses_a_legal_reading_of_an_illegal_width() fail {
  run_certified_step_02(
    certified_step_02(
      5,
      #"",
      certified_index,
      certificate_of(5),
      rule.ItemLegality { item_is_legal: True },
    ),
  )
}

/// The certified count also serves a chunked field: the coordinate one past
/// the last item is out of the field's domain.
test fiwi_step_02_certified_route_observes_an_index_past_the_count() {
  run_certified_step_02(
    certified_step_02(
      5,
      #"",
      certified_count,
      certificate_of(5),
      rule.ItemOutOfDomain,
    ),
  )
}

test fiwi_step_02_certified_route_refuses_another_fields_certificate() fail {
  run_certified_step_02(
    certified_step_02(
      5,
      #"",
      certified_index,
      certificate_of(2),
      rule.ItemLegality { item_is_legal: False },
    ),
  )
}

test fiwi_step_02_certified_route_refuses_an_in_progress_certificate() fail {
  run_certified_step_02(
    certified_step_02(
      5,
      #"",
      certified_index,
      fn(tx_id, preimage) {
        grammar.in_progress_field_grammar_certificate_v1(
          tx_id,
          5,
          preimage,
          certified_count,
        )
      },
      rule.ItemLegality { item_is_legal: False },
    ),
  )
}

/// A fixed-stride field (4, the required signers) has no certified route: its
/// item's legality is the item's shape and order, which the certified width
/// read does not decide. Two equal signers, each of a legal width, read
/// through a `Complete` certificate of the field, are refused.
test fiwi_step_02_certified_route_refuses_a_fixed_stride_field() fail {
  let signer = fixture.repeated_bytes_v1(#"c3", 28)
  let preimage = grammar.field_preimage_with_item_v1(signer, 2, 1, signer)
  run_certified_step_02(
    step_02_over(
      4,
      preimage,
      1,
      rule.ItemLegality { item_is_legal: True },
      fn(block) {
        Route {
          opening: BodyFieldOpening {
            native_tx_compact_cbor: block.compact_cbor,
            carriage: Inline { preimage },
          },
          grammar_ref_input_index: Some(0),
          reference_inputs: [
            grammar.field_grammar_certificate_input_v1(
              grammar.complete_field_grammar_certificate_v1(
                block.tx_id,
                4,
                preimage,
                2,
              ),
            ),
          ],
        }
      },
    ),
  )
}

// ### The walked route at its limit
//
// The walk decides the coordinate's domain by counting the field, which it
// can for a field carried inline or in one raw UTxO. Its dearest case is
// `grammar.walked_item_limit` items under tier 1, the coordinate last.

fn walked_limit_case() -> Step02Case {
  let preimage = grammar.walked_limit_inline_preimage_v1(#"")
  step_02_over(
    5,
    preimage,
    grammar.walked_item_limit - 1,
    rule.ItemLegality { item_is_legal: False },
    fn(block) {
      Route {
        opening: BodyFieldOpening {
          native_tx_compact_cbor: block.compact_cbor,
          carriage: Inline { preimage },
        },
        grammar_ref_input_index: None,
        reference_inputs: [],
      }
    },
  )
}

test fiwi_step_02_walked_route_fixture_only_at_the_walked_limit() {
  walked_limit_case().datum != None
}

test fiwi_step_02_walked_route_at_the_walked_limit_cost() {
  run_certified_step_02(walked_limit_case())
}
