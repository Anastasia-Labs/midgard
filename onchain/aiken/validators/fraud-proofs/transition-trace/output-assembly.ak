use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/native_tx/components
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_state
use midgard/transition_trace.{RootMembershipProof}
use midgard/validation_merkle_v1

validator assembly(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 3
    let index = if state.kind == 0 {
      state.output_index
    } else {
      state.deposit_index
    }
    let bytes = delegation.output_bytes(state, args, tx.reference_inputs)
    expect [(s1, s2, s3)] = state.summaries.summaries
    let skeleton = delegation.output_metadata_skeleton(state, bytes)
    let facts = scan.decode_control_v1(state.scan_cbor)
    expect Some(base) = ledger_output_descriptor_v1.assemble(
      index,
      components.decode_midgard_tx_output_cbor(skeleton),
      skeleton,
      s1,
      s2,
      s3,
    )
    let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..base,
      total_length: bytearray.length(bytes),
      item_commitment: ledger_output_commitment_v1.output_item_commitment(
        index,
        bytes,
      ),
      lovelace: facts.lovelace,
      asset_count: facts.asset_count,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
        facts.asset_count,
        facts.asset_peaks,
      ),
      cardano_value_size: facts.cardano_value_size,
    }
    expect ledger_output_commitment_v1.descriptor_is_well_formed(descriptor)
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State {
        ..state,
        phase: 9,
        descriptor_cbor: ledger_output_commitment_v1.encode(descriptor),
      },
    )
  }

  else(_) {
    fail
  }
}
