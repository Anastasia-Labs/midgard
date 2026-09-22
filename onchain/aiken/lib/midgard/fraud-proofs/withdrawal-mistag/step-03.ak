use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256, verify_ed25519_signature}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{NoDatum, OutputReference}
use env
use midgard/common/types.{H32, Proof}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/types as native_tx_types
use midgard/fraud_proofs/structured_data_carriage as carriage
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/fraud_proofs/withdrawal_mistag/step_02
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/mpf_proof_v1
use midgard/validation_merkle_v1

const withdrawal_signature_domain_v1 = #"4d6964676172645769746864726177616c5631"

pub const maximum_withdrawal_asset_count_v1 = 100

pub type WithdrawalBodyHash =
  H32<ledger_state.WithdrawalBody>

pub type LedgerEvidenceV1 {
  PresentLedgerOutput { descriptor_cbor: ByteArray, membership_proof: Proof }
  AbsentLedgerOutput { non_membership_proof: Proof }
}

pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  withdrawal_id: ledger_state.WithdrawalId,
  withdrawal_body_hash: WithdrawalBodyHash,
  claimed_valid: Bool,
  output_present: Bool,
  owner_signature_valid: Bool,
  cardano_value_size: Int,
  output_lovelace: Int,
  output_asset_count: Int,
  output_asset_frontier_commitment: ByteArray,
}

pub type Payload {
  withdrawal_info: ledger_state.WithdrawalInfo,
  evidence: LedgerEvidenceV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  payload: carriage.Evidence,
}

pub type Datum =
  ct.StepDatum<step_02.State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

type AuthenticatedOutputProjectionV1 {
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  cardano_value_size: Int,
  asset_count: Int,
  address_bytes: ByteArray,
  lovelace: Int,
  asset_frontier_commitment: ByteArray,
}

/// Decode only the descriptor facts this proof consumes. The descriptor bytes
/// themselves are authenticated as the exact MPF value before this projection
/// is trusted; rebuilding the full descriptor here would pull the complete
/// output-admission machine into this one fraud-proof step.
fn authenticated_output_projection_v1(
  descriptor_cbor: ByteArray,
) -> AuthenticatedOutputProjectionV1 {
  expect Some(data) = cbor.deserialise(descriptor_cbor)
  expect
      [
        version_data, output_index_data, total_length_data, item_commitment_data,
        address_data, lovelace_data, asset_count_data, asset_frontier_data,
        cardano_value_size_data, _, _, _, _, _, _, _,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == ledger_output_commitment_v1.version
  let projection = AuthenticatedOutputProjectionV1 {
    output_index: builtin.un_i_data(output_index_data),
    total_length: builtin.un_i_data(total_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    cardano_value_size: builtin.un_i_data(cardano_value_size_data),
    asset_count: builtin.un_i_data(asset_count_data),
    address_bytes: builtin.un_b_data(address_data),
    lovelace: builtin.un_i_data(lovelace_data),
    asset_frontier_commitment: builtin.un_b_data(asset_frontier_data),
  }
  expect projection.output_index >= 0
  expect projection.output_index <= ledger_output_commitment_v1.max_output_index
  expect projection.total_length >= 0
  expect bytearray.length(projection.item_commitment) == 32
  expect projection.asset_count >= 0
  expect projection.cardano_value_size >= 0
  expect
    projection.cardano_value_size <= ledger_output_commitment_v1.max_cardano_value_cbor_bytes
  projection
}

fn payment_owner(address_bytes: ByteArray) -> Option<ByteArray> {
  let address_length = bytearray.length(address_bytes)
  let address_type = bytearray.at(address_bytes, 0) / 16
  expect
    if address_length == 29 {
      address_type == 6 || address_type == 7
    } else {
      address_length == 57 && address_type >= 0 && address_type <= 3
    }
  if address_type == 1 || address_type == 3 || address_type == 7 {
    None
  } else {
    Some(bytearray.slice(address_bytes, 1, 28))
  }
}

// Native CBOR orders keys by encoded length; Cardano Value Data uses byte order.
// Merge runs keep mixed-width names bounded instead of quadratic insertion.
fn merge_asset_pairs(
  left: List<Pair<ByteArray, a>>,
  right: List<Pair<ByteArray, a>>,
) -> List<Pair<ByteArray, a>> {
  when left is {
    [] -> right
    [a, ..rest_left] ->
      when right is {
        [] -> left
        [b, ..rest_right] ->
          if bytearray.compare(a.1st, b.1st) == Less {
            [a, ..merge_asset_pairs(rest_left, right)]
          } else {
            [b, ..merge_asset_pairs(left, rest_right)]
          }
      }
  }
}

fn merge_asset_runs(
  runs: List<List<Pair<ByteArray, a>>>,
) -> List<List<Pair<ByteArray, a>>> {
  when runs is {
    [a, b, ..rest] -> [merge_asset_pairs(a, b), ..merge_asset_runs(rest)]
    _ -> runs
  }
}

fn sorted_asset_runs(
  runs: List<List<Pair<ByteArray, a>>>,
) -> List<Pair<ByteArray, a>> {
  when runs is {
    [] -> []
    [one] -> one
    _ -> sorted_asset_runs(merge_asset_runs(runs))
  }
}

fn asset_pairs_are_sorted(pairs: List<Pair<ByteArray, a>>) -> Bool {
  when pairs is {
    [a, b, ..rest] ->
      bytearray.compare(a.1st, b.1st) == Less && asset_pairs_are_sorted(
        [b, ..rest],
      )
    _ -> True
  }
}

fn sort_asset_pairs(pairs: List<Pair<ByteArray, a>>) -> List<Pair<ByteArray, a>> {
  if asset_pairs_are_sorted(pairs) {
    pairs
  } else {
    sorted_asset_runs(list.map(pairs, fn(pair) { [pair] }))
  }
}

pub fn withdrawal_signature_is_valid_v1(
  info: ledger_state.WithdrawalInfo,
) -> Bool {
  signature_matches_body_bytes(info, cbor.serialise(info.body))
}

fn signature_matches_body_bytes(
  info: ledger_state.WithdrawalInfo,
  body_cbor: ByteArray,
) -> Bool {
  let Pair(verification_key, signature) = info.signature
  let message = withdrawal_signature_domain_v1
    |> bytearray.concat(body_cbor)
    |> blake2b_256
  and {
    bytearray.length(verification_key) == 32,
    bytearray.length(signature) == 64,
    blake2b_224(verification_key) == info.body.l2_owner,
    verify_ed25519_signature(verification_key, message, signature),
  }
}

// The admitted descriptor commits address, lovelace and the complete canonical
// asset frontier. Reconstruct that frontier from the validated withdrawal Value
// instead of reparsing the full output (which may include a maximum script).
pub fn output_value_matches_v1(
  body: ledger_state.WithdrawalBody,
  value: assets.Value,
  output_lovelace: Int,
  output_asset_count: Int,
  output_asset_frontier_commitment: ByteArray,
) -> Bool {
  let valid_domains = list.all(
    body.l2_value,
    fn(policy) {
      if policy.1st == #"" {
        when policy.2nd is {
          [Pair(name, quantity)] -> name == #"" && quantity > 0
          _ -> False
        }
      } else {
        bytearray.length(policy.1st) == 28 && list.all(
          policy.2nd,
          fn(token) { bytearray.length(token.1st) <= 32 && token.2nd > 0 },
        )
      }
    },
  )
  if !valid_domains {
    False
  } else {
    let named = assets.flatten_with(
      value,
      fn(policy, name, quantity, keep, skip) {
        if policy == #"" {
          skip()
        } else {
          keep(
            Pair(
              bytearray.concat(policy, encode_definite_bytes(name)),
              ledger_output_commitment_v1.asset_leaf_hash(
                policy,
                name,
                quantity,
              ),
            ),
          )
        }
      },
    )
      |> sort_asset_pairs
    if list.length(named) != output_asset_count {
      False
    } else {
      let frontier = validation_merkle_v1.build_frontier(
        list.map(named, fn(pair) { pair.2nd }),
      )
      and {
        assets.lovelace_of(value) == output_lovelace,
        validation_merkle_v1.frontier_commitment(
          output_asset_count,
          frontier.peaks,
        ) == output_asset_frontier_commitment,
        output_asset_count <= maximum_withdrawal_asset_count_v1,
      }
    }
  }
}

fn output_core_matches_with_signature_v1(
  info: ledger_state.WithdrawalInfo,
  output: AuthenticatedOutputProjectionV1,
  signature_is_valid: Bool,
) -> Bool {
  payment_owner(output.address_bytes) == Some(info.body.l2_owner) && signature_is_valid && output_value_matches_v1(
    info.body,
    assets.from_asset_list(info.body.l2_value),
    output.lovelace,
    output.asset_count,
    output.asset_frontier_commitment,
  )
}

pub fn classify_ledger_evidence_v1(
  input_state: step_02.State,
  withdrawal_info: ledger_state.WithdrawalInfo,
  evidence: LedgerEvidenceV1,
) -> State {
  expect
    utils.serialise_and_hash_32(withdrawal_info) == input_state.withdrawal_info_hash
  let key = ledger_outref_key(withdrawal_info.body.l2_outref)
  let body_cbor = cbor.serialise(withdrawal_info.body)
  let withdrawal_body_hash = blake2b_256(body_cbor)
  when evidence is {
    AbsentLedgerOutput { non_membership_proof } -> {
      expect
        mpf_proof_v1.does_not_have(
          input_state.pre_utxos_root,
          key,
          non_membership_proof,
        )
      State {
        challenged_header_hash: input_state.challenged_header_hash,
        withdrawal_id: input_state.withdrawal_id,
        withdrawal_body_hash,
        claimed_valid: input_state.claimed_valid,
        output_present: False,
        owner_signature_valid: False,
        cardano_value_size: 0,
        output_lovelace: 0,
        output_asset_count: 0,
        output_asset_frontier_commitment: #"",
      }
    }
    PresentLedgerOutput { descriptor_cbor, membership_proof } -> {
      expect
        mpf_proof_v1.has(
          input_state.pre_utxos_root,
          key,
          descriptor_cbor,
          membership_proof,
        )
      let descriptor = authenticated_output_projection_v1(descriptor_cbor)
      expect
        descriptor.output_index == withdrawal_info.body.l2_outref.output_index
      // The authenticated ledger descriptor already commits the exact admitted
      // asset count. An over-limit output cannot satisfy the withdrawal rule.
      let owner_signature_valid = if descriptor.asset_count > maximum_withdrawal_asset_count_v1 {
        False
      } else {
        payment_owner(descriptor.address_bytes) == Some(
          withdrawal_info.body.l2_owner,
        ) && signature_matches_body_bytes(withdrawal_info, body_cbor)
      }
      State {
        challenged_header_hash: input_state.challenged_header_hash,
        withdrawal_id: input_state.withdrawal_id,
        withdrawal_body_hash,
        claimed_valid: input_state.claimed_valid,
        output_present: True,
        owner_signature_valid,
        cardano_value_size: descriptor.cardano_value_size,
        output_lovelace: descriptor.lovelace,
        output_asset_count: descriptor.asset_count,
        output_asset_frontier_commitment: descriptor.asset_frontier_commitment,
      }
    }
  }
}

fn fixture_outref() -> OutputReference {
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }
}

fn fixture_info(lovelace: Int) -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: fixture_outref(),
      l2_owner: #"11111111111111111111111111111111111111111111111111111111",
      l2_value: [
        Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, lovelace)]),
      ],
      l1_address: address.from_verification_key(
        #"11111111111111111111111111111111111111111111111111111111",
      ),
      l1_datum: NoDatum,
    },
    signature: Pair(#"", #""),
    validity: ledger_state.WithdrawalIsValid,
  }
}

fn fixture_output(lovelace: Int) -> native_tx_types.MidgardTxOutput {
  native_tx_types.MidgardTxOutput {
    address: native_tx_types.MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: native_tx_types.PubKeyCredential(
        #"11111111111111111111111111111111111111111111111111111111",
      ),
      stake_credential: None,
    },
    value: native_tx_types.MidgardValue { lovelace, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

fn fixture_output_core(lovelace: Int) -> AuthenticatedOutputProjectionV1 {
  AuthenticatedOutputProjectionV1 {
    output_index: 0,
    total_length: 0,
    item_commitment: #"",
    cardano_value_size: 0,
    address_bytes: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace,
    asset_count: 0,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
  }
}

fn fixture_state(
  info: ledger_state.WithdrawalInfo,
  root: ledger_state.MidgardLedgerRoot,
) -> step_02.State {
  step_02.State {
    challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
    withdrawal_id: fixture_outref(),
    withdrawal_info_hash: utils.serialise_and_hash_32(info),
    claimed_valid: True,
    pre_utxos_root: root,
  }
}

test accepts_exact_owner_value_and_asset_count() {
  output_core_matches_with_signature_v1(
    fixture_info(1_000_000),
    fixture_output_core(1_000_000),
    True,
  )
}

test rejects_wrong_owner() {
  let output = fixture_output_core(1_000_000)
  !output_core_matches_with_signature_v1(
    fixture_info(1_000_000),
    AuthenticatedOutputProjectionV1 {
      ..output,
      address_bytes: #"6022222222222222222222222222222222222222222222222222222222",
    },
    True,
  )
}

test rejects_wrong_value() {
  !output_core_matches_with_signature_v1(
    fixture_info(1_000_000),
    fixture_output_core(999_999),
    True,
  )
}

test classifies_authenticated_output_membership() {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let key = ledger_outref_key(info.body.l2_outref)
  let value = ledger_output_commitment_v1.encode(descriptor)
  let root = mpf.root(mpf.insert(mpf.empty, key, value, []))
  let state = classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { descriptor_cbor: value, membership_proof: [] },
  )
  and {
    state.output_present,
    !state.owner_signature_valid,
    state.cardano_value_size == descriptor.cardano_value_size,
    state.withdrawal_body_hash == utils.serialise_and_hash_32(info.body),
  }
}

test accepts_authenticated_empty_ledger_nonmembership() {
  let info = fixture_info(1_000_000)
  let state = classify_ledger_evidence_v1(
    fixture_state(info, env.empty_merkle_tree_root),
    info,
    AbsentLedgerOutput { non_membership_proof: [] },
  )
  and {
    !state.output_present,
    !state.owner_signature_valid,
    state.cardano_value_size == 0,
  }
}

test rejects_substituted_withdrawal_opening() fail {
  let authentic = fixture_info(1_000_000)
  classify_ledger_evidence_v1(
    fixture_state(authentic, env.empty_merkle_tree_root),
    fixture_info(999_999),
    AbsentLedgerOutput { non_membership_proof: [] },
  ).claimed_valid
}

test rejects_wrong_outref_key() {
  ledger_outref_key(fixture_outref()) != ledger_outref_key(
    OutputReference { ..fixture_outref(), output_index: 1 },
  )
}

test rejects_substituted_authenticated_descriptor() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let authentic_value = ledger_output_commitment_v1.encode(descriptor)
  let substituted_value = ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor,
      total_length: descriptor.total_length + 1,
    },
  )
  let key = ledger_outref_key(info.body.l2_outref)
  let root = mpf.root(mpf.insert(mpf.empty, key, authentic_value, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput {
      descriptor_cbor: substituted_value,
      membership_proof: [],
    },
  ).output_present
}

test rejects_descriptor_authenticated_under_wrong_key() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let wrong_key = ledger_outref_key(
    OutputReference { ..info.body.l2_outref, output_index: 1 },
  )
  let root = mpf.root(mpf.insert(mpf.empty, wrong_key, descriptor_cbor, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { descriptor_cbor, membership_proof: [] },
  ).output_present
}

test rejects_authenticated_descriptor_with_wrong_output_index() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(1, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let key = ledger_outref_key(info.body.l2_outref)
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { descriptor_cbor, membership_proof: [] },
  ).output_present
}

test rejects_substituted_descriptor_with_wrong_total_length() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor,
      total_length: descriptor.total_length + 1,
    },
  )
  let key = ledger_outref_key(info.body.l2_outref)
  let root = mpf.root(
    mpf.insert(
      mpf.empty,
      key,
      ledger_output_commitment_v1.encode(descriptor),
      [],
    ),
  )
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { descriptor_cbor, membership_proof: [] },
  ).output_present
}

test rejects_substituted_descriptor_with_wrong_item_commitment() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor,
      item_commitment: #"0000000000000000000000000000000000000000000000000000000000000000",
    },
  )
  let key = ledger_outref_key(info.body.l2_outref)
  let root = mpf.root(
    mpf.insert(
      mpf.empty,
      key,
      ledger_output_commitment_v1.encode(descriptor),
      [],
    ),
  )
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { descriptor_cbor, membership_proof: [] },
  ).output_present
}

test rejects_output_substitution_against_authenticated_commitment() fail {
  let info = fixture_info(1_000_000)
  let authentic_output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  let substituted_output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(999_999),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
    0,
    authentic_output_cbor,
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let key = ledger_outref_key(info.body.l2_outref)
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput {
      descriptor_cbor: {
        expect Some(substituted) = ledger_output_descriptor_v1.build_v1(
          0,
          substituted_output_cbor,
        )
        ledger_output_commitment_v1.encode(substituted)
      },
      membership_proof: [],
    },
  ).output_present
}

test rejects_absence_branch_for_present_output() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let key = ledger_outref_key(info.body.l2_outref)
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    AbsentLedgerOutput { non_membership_proof: [] },
  ).output_present
}

test rejects_presence_branch_for_absent_output() fail {
  let info = fixture_info(1_000_000)
  let output_cbor = native_components.encode_midgard_tx_output(
    fixture_output(1_000_000),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  classify_ledger_evidence_v1(
    fixture_state(info, env.empty_merkle_tree_root),
    info,
    PresentLedgerOutput {
      descriptor_cbor: ledger_output_commitment_v1.encode(descriptor),
      membership_proof: [],
    },
  ).output_present
}

test rejects_extra_asset_name_under_ada_policy() {
  let body = ledger_state.WithdrawalBody {
    ..fixture_info(1_000_000).body,
    l2_value: [Pair(#"", [Pair(#"", 1_000_000), Pair(#"01", 1)])],
  }
  !output_value_matches_v1(
    body,
    assets.from_asset_list(body.l2_value),
    1_000_000,
    0,
    validation_merkle_v1.frontier_commitment(0, []),
  )
}

test rejects_negative_non_ada_asset_as_value_mismatch() {
  let body = ledger_state.WithdrawalBody {
    ..fixture_info(1_000_000).body,
    l2_value: [
      Pair(#"", [Pair(#"", 1_000_000)]),
      Pair(
        #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        [Pair(#"01", -1)],
      ),
    ],
  }
  !output_value_matches_v1(
    body,
    assets.from_asset_list(body.l2_value),
    1_000_000,
    1,
    #"00",
  )
}

test descriptor_frontier_matches_mixed_width_value_and_refuses_quantity_change() {
  let policy = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let info = ledger_state.WithdrawalInfo {
    ..fixture_info(1_000_000),
    body: ledger_state.WithdrawalBody {
      ..fixture_info(1_000_000).body,
      l2_value: [
        Pair(#"", [Pair(#"", 1_000_000)]),
        Pair(policy, [Pair(#"0000", 2), Pair(#"ff", 1)]),
      ],
    },
  }
  let output = native_tx_types.MidgardTxOutput {
    ..fixture_output(1_000_000),
    value: native_tx_types.MidgardValue {
      lovelace: 1_000_000,
      assets: [
        Pair(bytearray.concat(policy, #"ff"), 1),
        Pair(bytearray.concat(policy, #"0000"), 2),
      ],
    },
  }
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
    0,
    native_components.encode_midgard_tx_output(output),
  )
  let projection = authenticated_output_projection_v1(
    ledger_output_commitment_v1.encode(descriptor),
  )
  let changed = ledger_state.WithdrawalInfo {
    ..info,
    body: ledger_state.WithdrawalBody {
      ..info.body,
      l2_value: [
        Pair(#"", [Pair(#"", 1_000_000)]),
        Pair(policy, [Pair(#"0000", 3), Pair(#"ff", 1)]),
      ],
    },
  }
  output_core_matches_with_signature_v1(info, projection, True) && !output_core_matches_with_signature_v1(
    changed,
    projection,
    True,
  )
}
