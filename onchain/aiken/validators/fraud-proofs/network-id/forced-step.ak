//// Q35 network-id forced source binding.  Kept physically separate so the
//// accepted/post-UTxO first step and this authenticated forced-leaf door both
//// retain signed reference-publication margin.

use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/network_id/forced_step.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/network_id/step_01.{ForcedNetworkIdMismatch}
use midgard/fraud_proofs/network_id/step_02 as step_02
use midgard/rejection_reason_v1.{ForcedTxInvalid, NetworkIdMismatch}
use midgard/transition_trace

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  expected_network_id: Int,
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
        expect None = input_state
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
        expect ForcedTxInvalid { reason: NetworkIdMismatch } = leaf.verdict
        let Pair(verified, _) = verify_native_tx_proof_source_v1(
          leaf.tx_id,
          leaf.source.compact_cbor,
          leaf.source.witness_set_compact_cbor,
          leaf.source.field_preimage_lengths_cbor,
        )
        expect and {
            verified.tx_id == leaf.tx_id,
            verified.tx_compact.validity_code == 1,
          }
        let expected: Data = step_02.State {
          bad_tx_id: leaf.tx_id,
          committed_tx_network_id: verified.tx_compact.body.network_id,
          expected_network_id,
          fault: ForcedNetworkIdMismatch,
          post_utxo: None,
          forced_source_key: Some(cbor.serialise(membership.key)),
        }
        and {
          output_hash == step_02_validator_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
