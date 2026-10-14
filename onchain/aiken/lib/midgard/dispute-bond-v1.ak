//// The validation-dispute bond. VerifySource locks it in its own UTxO at
//// `dispute_bond_v1`, apart from the computation thread, so cancelling the
//// thread cannot withdraw it. Any conviction of the challenged block refunds
//// it to the prover; once `forfeit_after` has passed, anyone may forfeit it to
//// the protocol treasury. Both spends are permissionless.

use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/math
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use env
use midgard/common/utils
use midgard/fraud_proof
use midgard/ledger_state.{HeaderHash, block_maturity_duration_v1}
use midgard/validation_dispute_v1

pub type DisputeBondV1 {
  challenged_header_hash: HeaderHash,
  fraud_prover: VerificationKeyHash,
  forfeit_after: Int,
}

pub type SpendRedeemer {
  Refund { fraud_proof_ref_input_index: Int, output_index: Int }
  Forfeit { output_index: Int }
}

/// The last instant a bond on a block ending at `challenged_block_end_time`
/// cannot be forfeited. The game, its resolution and the fraud record all end
/// before maturity; the refund window after maturity is 2C + R.
pub fn forfeit_after(challenged_block_end_time: Int) -> Int {
  challenged_block_end_time + block_maturity_duration_v1 + 2 * validation_dispute_v1.dispute_side_clock_milliseconds + validation_dispute_v1.dispute_resolution_allowance_milliseconds
}

/// Where a forfeited bond goes: the protocol treasury native script, with no
/// stake credential.
pub const treasury_address: Address =
  Address {
    payment_credential: Script(env.protocol_treasury_script_hash),
    stake_credential: None,
  }

/// Both spend paths. The output at `output_index` must go to the path's
/// destination, carry `InlineDatum(own_out_ref)`, and hold the bond's
/// lovelace: less at most `min(tx.fee, max_dispute_move_fee_lovelace)` when
/// the bond is the transaction's only input, and nothing less otherwise.
pub fn spend(
  fraud_proof_policy_id: PolicyId,
  datum: Option<DisputeBondV1>,
  redeemer: SpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  expect Some(bond) = datum
  let Transaction { inputs, reference_inputs, outputs, fee, validity_range, .. }
    = tx
  expect Some(own_input) = transaction.find_input(inputs, own_out_ref)
  let input_lovelace = lovelace_of(own_input.output.value)
  let floor = when inputs is {
    [_] -> input_lovelace - math.min(fee, env.max_dispute_move_fee_lovelace)
    _ -> input_lovelace
  }
  let pays = fn(output_index: Int, destination: Address) {
    expect Some(Output { address, value, datum: output_datum, .. }) = list.at(
      outputs,
      output_index,
    )
    let tag: Data = own_out_ref
    and {
      address == destination,
      output_datum == InlineDatum(tag),
      lovelace_of(value) >= floor,
    }
  }
  when redeemer is {
    Refund { fraud_proof_ref_input_index, output_index } -> {
      let header_hash, _ <- fraud_proof.get_proven_fraud_record(
        reference_inputs,
        fraud_proof_policy_id,
        fraud_proof_ref_input_index,
      )
      and {
        header_hash == bond.challenged_header_hash,
        pays(
          output_index,
          Address {
            payment_credential: VerificationKey(bond.fraud_prover),
            stake_credential: None,
          },
        ),
      }
    }
    Forfeit { output_index } -> and {
        utils.get_inclusive_lower_bound_of_interval(validity_range) > bond.forfeit_after,
        pays(output_index, treasury_address),
      }
  }
}
