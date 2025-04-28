use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/certificate.{RegisterCredential, UnregisterCredential}
use cardano/transaction.{
  Datum as CardanoDatum, OutputReference, Publish, Redeemer, ScriptPurpose,
}
use midgard/common/types.{PosixTime}
use midgard/ledger_state

pub type Datum {
  event: ledger_state.DepositEvent,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: CardanoDatum,
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  mint_redeemer_index: Int,
  purpose: SpendPurpose,
}

pub type SpendPurpose {
  TransferToReserve { membership_proof: mpf.Proof }
  Refund { refund_approach: RefundApproach }
}

pub type RefundApproach {
  SkippedDeposit { non_membership_proof: mpf.Proof }
  DepositInSettlementGap { settlement_link_ref_input_index: Int }
}

pub type MintRedeemer {
  AuthenticateDeposit {
    nonce_input_index: Int,
    deposit_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
  }
  BurnDepositNFT {
    l2_id: AssetName,
    witness_unregistration_redeemer_index: Int,
  }
}

pub type WitnessRedeemer =
  PolicyId

/// Witness script's prefix from its single CBOR encoded applied bytearray.
/// TODO: This should probably be moved to `env` with a better generation
///       process.
pub const witness_script_prefix: ByteArray =
  #"5901a801010033232323232323223225333004323232323253330093370e900318051baa0011323232323232533300f3370e9000000898018010a9998079801800899b870024800452818079baa3013301400532533300e3002300f37540022900009bad30133010375400264a66601c6004601e6ea80045300103d87a8000132330010013756602860226ea8008894ccc04c004530103d87a80001323332225333014337220240062a66602866e3c04800c4cdd2a4000660306ea00092f5c02980103d87a8000133006006001375c60240026eb4c04c004c05c008c054004c8cc004004dd59809980a180a180a180a18081baa00822533301200114c103d87a80001323332225333013337220100062a66602666e3c02000c4cdd2a40006602e6e980092f5c02980103d87a8000133006006001375c60220026eacc048004c058008c050004dc3a40046eb8010dd6980780098059baa00116300d300e003300c002300b002300b0013006375400229309b2b1bae0015734aae7555cf2ab9f5740ae855d1261225820"

pub fn nonce_out_ref_to_l2_id(nonce_out_ref: OutputReference) -> AssetName {
  cbor.serialise(nonce_out_ref) |> blake2b_256
}

// TODO: It might be possible to move this to the broader `user_events` module.
pub fn validate_witness_redeemer(
  expected_witness_script_hash: ScriptHash,
  witness_redeemer_index: Int,
  for_registration: Bool,
  event_policy_id: PolicyId,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  // Grab redeemer pair of witness script registration.
  expect Some(witness_redeemer_pair) =
    redeemers |> list.at(witness_redeemer_index)

  // Grab the included `Publish` script hash (no validation on `certificate`
  // index, TODO?).
  let present_witness_script_hash =
    if for_registration {
      expect Publish {
        certificate: RegisterCredential {
          credential: Script(witness_script_hash),
          ..
        },
        ..
      } = witness_redeemer_pair.1st
      witness_script_hash
    } else {
      expect Publish {
        certificate: UnregisterCredential {
          credential: Script(witness_unregistration_script_hash),
          ..
        },
        ..
      } = witness_redeemer_pair.1st
      witness_unregistration_script_hash
    }

  // Since `WitnessRedeemer` is an alias for `PolicyId`, acting as a dependency
  // proxy between deposit/withdrawal policy and witness scripts.
  let expected_witness_redeemer: Data = event_policy_id

  and {
    // 1. Registered script must match the expected witness.
    present_witness_script_hash == expected_witness_script_hash,
    // 2. Deposit/withdrawal policy ID, and L2 ID must be correctly set.
    witness_redeemer_pair.2nd == expected_witness_redeemer,
  }
}
