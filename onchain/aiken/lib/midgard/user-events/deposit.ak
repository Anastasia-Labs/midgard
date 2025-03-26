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
use midgard/common/types.{MerkleRoot, PosixTime}
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
  settlement_node_asset_name: AssetName,
  mint_redeemer_index: Int,
  purpose: SpendPurpose,
}

pub type SpendPurpose {
  TransferToReserve { merkle_proof: mpf.Proof }
  Refund { refund_approach: RefundApproach }
}

pub type RefundApproach {
  SkippedDeposit { insertion_root: MerkleRoot, merkle_proof: mpf.Proof }
  DepositInTimeGap { settlement_link_ref_input_index: Int }
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
  #"5901ee01010033232323232323223225333004323232323253330093370e900318051baa0011323322533300c3370e900018069baa0051323232325333013301600213232323253330143370e9000000898018010a99980a1801800899b8700248004528180a1baa0083332223253330163005301737540022900009bad301b3018375400264a66602c600a602e6ea8004530103d87a8000132330010013756603860326ea8008894ccc06c004530103d87a8000132333222533301c337220100062a66603866e3c02000c4cdd2a4000660406ea00092f5c02980103d87a8000133006006001375c60340026eb4c06c004c07c008c074004c8cc004004010894ccc0680045300103d87a8000132333222533301b337220100062a66603666e3c02000c4cdd2a40006603e6e980092f5c02980103d87a8000133006006001375c60320026eacc068004c078008c070004dd5980b980c180c180c180c180a1baa00c375c602e0046eb8c05cc060008dc3a400460246ea802458dd7180a000980a0011bae3012001300e375400a2c6eb4c03c004c03cc040004c02cdd50008b1806980700198060011805801180580098031baa00114984d958dd7000ab9a5573aaae7955cfaba05742ae893001225820"

pub fn nonce_out_ref_to_l2_id(nonce_out_ref: OutputReference) -> AssetName {
  cbor.serialise(nonce_out_ref) |> blake2b_256
}

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

  // Grab the included script hash (no validation on `certificate` index,
  // TODO?).
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

  let expected_witness_redeemer: Data = event_policy_id

  and {
    // 1. Registered script must match the expected witness.
    present_witness_script_hash == expected_witness_script_hash,
    // 2. Deposit policy ID, and L2 ID must be correctly set.
    witness_redeemer_pair.2nd == expected_witness_redeemer,
  }
}
