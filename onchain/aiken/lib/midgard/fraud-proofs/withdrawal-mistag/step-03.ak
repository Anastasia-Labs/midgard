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
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/types as native_tx_types
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/fraud_proofs/withdrawal_mistag/step_02
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_v1
use midgard/ledger_state
use midgard/mpf_proof_v1

const withdrawal_signature_domain_v1 = #"4d6964676172645769746864726177616c5631"

pub const maximum_withdrawal_asset_count_v1 = 100

pub type WithdrawalBodyHash =
  H32<ledger_state.WithdrawalBody>

pub type LedgerEvidenceV1 {
  PresentLedgerOutput { output_cbor: ByteArray, membership_proof: Proof }
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

pub fn output_core_matches_with_signature_v1(
  info: ledger_state.WithdrawalInfo,
  output: native_tx_types.MidgardTxOutput,
  signature_is_valid: Bool,
) -> Bool {
  let owner_matches = when output.address.payment_credential is {
    native_tx_types.PubKeyCredential(owner) -> owner == info.body.l2_owner
    _ -> False
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

pub fn output_core_matches_v1(
  info: ledger_state.WithdrawalInfo,
  output: native_tx_types.MidgardTxOutput,
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
    PresentLedgerOutput { output_cbor, membership_proof } -> {
      expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
        withdrawal_info.body.l2_outref.output_index,
        output_cbor,
      )
      let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
      expect
        mpf_proof_v1.has(
          input_state.pre_utxos_root,
          key,
          descriptor_cbor,
          membership_proof,
        )
      expect Some(output) = ledger_output_v1.decode_canonical_output(
        output_cbor,
      )
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
    fixture_output(1_000_000),
    True,
  )
}

test rejects_wrong_owner() {
  let output = fixture_output(1_000_000)
  !output_core_matches_with_signature_v1(
    fixture_info(1_000_000),
    native_tx_types.MidgardTxOutput {
      ..output,
      address: native_tx_types.MidgardAddress {
        ..output.address,
        payment_credential: native_tx_types.PubKeyCredential(
          #"22222222222222222222222222222222222222222222222222222222",
        ),
      },
    },
    True,
  )
}

test rejects_wrong_value() {
  !output_core_matches_with_signature_v1(
    fixture_info(1_000_000),
    fixture_output(999_999),
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
    PresentLedgerOutput { output_cbor, membership_proof: [] },
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
