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
use midgard/bounded_item_v1
use midgard/common/types.{H32, Proof}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/types as native_tx_types
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/fraud_proofs/withdrawal_mistag/step_02
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/mpf_proof_v1

const withdrawal_signature_domain_v1 = #"4d6964676172645769746864726177616c5631"

pub const maximum_withdrawal_asset_count_v1 = 100

pub type WithdrawalBodyHash =
  H32<ledger_state.WithdrawalBody>

pub type LedgerEvidenceV1 {
  PresentLedgerOutput {
    output_cbor: ByteArray,
    descriptor_cbor: ByteArray,
    membership_proof: Proof,
  }
  AbsentLedgerOutput { non_membership_proof: Proof }
}

pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  withdrawal_id: ledger_state.WithdrawalId,
  withdrawal_body_hash: WithdrawalBodyHash,
  claimed_valid: Bool,
  output_present: Bool,
  core_valid: Bool,
  cardano_value_size: Int,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  withdrawal_info: ledger_state.WithdrawalInfo,
  evidence: LedgerEvidenceV1,
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
}

type WithdrawalOutputCoreV1 {
  payment_owner: Option<ByteArray>,
  value: native_tx_types.MidgardValue,
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
        _, _, _, _, cardano_value_size_data, _, _, _, _, _, _, _,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == ledger_output_commitment_v1.version
  let projection = AuthenticatedOutputProjectionV1 {
    output_index: builtin.un_i_data(output_index_data),
    total_length: builtin.un_i_data(total_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    cardano_value_size: builtin.un_i_data(cardano_value_size_data),
  }
  expect projection.output_index >= 0
  expect projection.output_index <= ledger_output_commitment_v1.max_output_index
  expect projection.total_length >= 0
  expect bytearray.length(projection.item_commitment) == 32
  expect projection.cardano_value_size >= 0
  expect
    projection.cardano_value_size <= ledger_output_commitment_v1.max_cardano_value_cbor_bytes
  projection
}

fn optional_output_fields_are_shaped_v1(fields: List<Pair<Data, Data>>) -> Bool {
  when fields is {
    [] -> True
    [Pair(key, _)] -> {
      let field = builtin.un_i_data(key)
      field == 2 || field == 3
    }
    [Pair(datum_key, _), Pair(script_key, _)] ->
      builtin.un_i_data(datum_key) == 2 && builtin.un_i_data(script_key) == 3
    _ -> False
  }
}

fn flatten_output_assets_v1(
  policies: List<Pair<Data, Data>>,
) -> List<Pair<ByteArray, Int>> {
  when policies is {
    [] -> []
    [Pair(policy_data, assets_data), ..rest] -> {
      let policy_id = builtin.un_b_data(policy_data)
      let assets = builtin.un_map_data(assets_data)
      flatten_policy_assets_v1(policy_id, assets)
        |> list.concat(flatten_output_assets_v1(rest))
    }
  }
}

fn flatten_policy_assets_v1(
  policy_id: ByteArray,
  entries: List<Pair<Data, Data>>,
) -> List<Pair<ByteArray, Int>> {
  when entries is {
    [] -> []
    [Pair(name_data, quantity_data), ..rest] ->
      [
        Pair(
          policy_id |> bytearray.concat(builtin.un_b_data(name_data)),
          builtin.un_i_data(quantity_data),
        ),
        ..flatten_policy_assets_v1(policy_id, rest)
      ]
  }
}

/// The descriptor's item commitment authenticates these as the exact output
/// bytes admitted by the ledger-output machine. This local decoder therefore
/// projects only the address and value needed by withdrawal classification,
/// and the descriptor admission path has already enforced canonical CBOR.
fn decode_withdrawal_output_core_v1(
  output_cbor: ByteArray,
) -> WithdrawalOutputCoreV1 {
  expect Some(output_data) = cbor.deserialise(output_cbor)
  expect
      [Pair(address_key, address_data), Pair(value_key, value_data), ..optional]
    = builtin.un_map_data(output_data)
  expect builtin.un_i_data(address_key) == 0
  expect builtin.un_i_data(value_key) == 1
  expect optional_output_fields_are_shaped_v1(optional)
  let address_bytes = builtin.un_b_data(address_data)
  let address_length = bytearray.length(address_bytes)
  let address_type = bytearray.at(address_bytes, 0) / 16
  expect
    if address_length == 29 {
      address_type == 6 || address_type == 7
    } else {
      address_length == 57 && address_type >= 0 && address_type <= 3
    }
  let payment_owner = if or {
    address_type == 1,
    address_type == 3,
    address_type == 7,
  } {
    None
  } else {
    Some(bytearray.slice(address_bytes, 1, 28))
  }
  expect [lovelace_data, assets_data] = builtin.un_list_data(value_data)
  let lovelace = builtin.un_i_data(lovelace_data)
  expect lovelace >= 0
  WithdrawalOutputCoreV1 {
    payment_owner,
    value: native_tx_types.MidgardValue {
      lovelace,
      assets: flatten_output_assets_v1(builtin.un_map_data(assets_data)),
    },
  }
}

fn add_native_assets(
  value: assets.Value,
  native_assets: List<Pair<ByteArray, Int>>,
) -> assets.Value {
  when native_assets is {
    [] -> value
    [Pair(unit, quantity), ..rest] ->
      add_native_assets(
        assets.add(
          value,
          bytearray.take(unit, 28),
          bytearray.drop(unit, 28),
          quantity,
        ),
        rest,
      )
  }
}

fn native_value_to_cardano(value: native_tx_types.MidgardValue) -> assets.Value {
  add_native_assets(assets.from_lovelace(value.lovelace), value.assets)
}

pub fn withdrawal_signature_is_valid_v1(
  info: ledger_state.WithdrawalInfo,
) -> Bool {
  let Pair(verification_key, signature) = info.signature
  let message = withdrawal_signature_domain_v1
    |> bytearray.concat(cbor.serialise(info.body))
    |> blake2b_256
  and {
    bytearray.length(verification_key) == 32,
    bytearray.length(signature) == 64,
    blake2b_224(verification_key) == info.body.l2_owner,
    verify_ed25519_signature(verification_key, message, signature),
  }
}

fn output_core_matches_with_signature_v1(
  info: ledger_state.WithdrawalInfo,
  output: WithdrawalOutputCoreV1,
  signature_is_valid: Bool,
) -> Bool {
  let owner_matches = when output.payment_owner is {
    Some(owner) -> owner == info.body.l2_owner
    None -> False
  }
  and {
    owner_matches,
    native_value_to_cardano(output.value) == assets.from_asset_list(
      info.body.l2_value,
    ),
    list.length(output.value.assets) <= maximum_withdrawal_asset_count_v1,
    signature_is_valid,
  }
}

fn output_core_matches_v1(
  info: ledger_state.WithdrawalInfo,
  output: WithdrawalOutputCoreV1,
) -> Bool {
  output_core_matches_with_signature_v1(
    info,
    output,
    withdrawal_signature_is_valid_v1(info),
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
        withdrawal_body_hash: utils.serialise_and_hash_32(withdrawal_info.body),
        claimed_valid: input_state.claimed_valid,
        output_present: False,
        core_valid: False,
        cardano_value_size: 0,
      }
    }
    PresentLedgerOutput { output_cbor, descriptor_cbor, membership_proof } -> {
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
      expect descriptor.total_length == bytearray.length(output_cbor)
      expect
        descriptor.item_commitment == bounded_item_v1.from_bytes(
          ledger_output_commitment_v1.output_field_index,
          descriptor.output_index,
          output_cbor,
        )
      let output = decode_withdrawal_output_core_v1(output_cbor)
      State {
        challenged_header_hash: input_state.challenged_header_hash,
        withdrawal_id: input_state.withdrawal_id,
        withdrawal_body_hash: utils.serialise_and_hash_32(withdrawal_info.body),
        claimed_valid: input_state.claimed_valid,
        output_present: True,
        core_valid: output_core_matches_v1(withdrawal_info, output),
        cardano_value_size: descriptor.cardano_value_size,
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

fn fixture_output_core(lovelace: Int) -> WithdrawalOutputCoreV1 {
  WithdrawalOutputCoreV1 {
    payment_owner: Some(
      #"11111111111111111111111111111111111111111111111111111111",
    ),
    value: native_tx_types.MidgardValue { lovelace, assets: [] },
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
    WithdrawalOutputCoreV1 {
      ..output,
      payment_owner: Some(
        #"22222222222222222222222222222222222222222222222222222222",
      ),
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
    PresentLedgerOutput {
      output_cbor,
      descriptor_cbor: value,
      membership_proof: [],
    },
  )
  and {
    state.output_present,
    !state.core_valid,
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
    !state.core_valid,
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
      output_cbor,
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
    PresentLedgerOutput { output_cbor, descriptor_cbor, membership_proof: [] },
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
    PresentLedgerOutput { output_cbor, descriptor_cbor, membership_proof: [] },
  ).output_present
}

test rejects_authenticated_descriptor_with_wrong_total_length() fail {
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
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { output_cbor, descriptor_cbor, membership_proof: [] },
  ).output_present
}

test rejects_authenticated_descriptor_with_wrong_item_commitment() fail {
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
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  classify_ledger_evidence_v1(
    fixture_state(info, root),
    info,
    PresentLedgerOutput { output_cbor, descriptor_cbor, membership_proof: [] },
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
      output_cbor: substituted_output_cbor,
      descriptor_cbor,
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
      output_cbor,
      descriptor_cbor: ledger_output_commitment_v1.encode(descriptor),
      membership_proof: [],
    },
  ).output_present
}
