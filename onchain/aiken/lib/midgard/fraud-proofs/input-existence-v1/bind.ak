use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/input_existence_v1/source.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/input_existence_v1/types.{SourceStateV1}
use midgard/fraud_proofs/native_tx/compact.{decode_forced_tx_compact_v1}
use midgard/fraud_proofs/native_tx/types.{TxCompactView} as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub fn bind_source_v1(
  next_hash: ByteArray,
  policy: PolicyId,
  hub: ByteArray,
  datum: Option<Datum>,
  redeemer: SpendRedeemer,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  when redeemer is {
    ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
      cancel(
        policy,
        datum,
        input_index,
        own_ref,
        computation_thread_mint_redeemer_index,
        tx,
      )
    ct.Continue(Args{source: AcceptedSource { inclusion }}) -> {
      let
          _,
          thread_name,
          _,
          input_state,
          output_hash,
          output_state,
          header,
          _,
          verified
        <- pass_native_tx_to_next_step_carried(
        policy,
        hub,
        datum,
        inclusion,
        own_ref,
        tx,
      )
      expect None = input_state
      let subject = substrate.open_subject_v1(
        thread_name,
        substrate.AcceptedSource { header, tx_inclusion: verified },
      )
      let expected: Data = SourceStateV1 {
        subject,
        header,
        compact: verified.tx_compact,
      }
      output_hash == next_hash && output_state == expected
    }
    ct.Continue(Args{source: ForcedSource {
      input_index,
      output_index,
      header,
      membership,
    }}) -> {
      expect Some(step_datum) = datum
      let _, thread_name, _, input_state, output_hash, output_state <- continue(
        policy,
        step_datum,
        input_index,
        output_index,
        own_ref,
        tx.inputs,
        tx.outputs,
        tx.extra_signatories,
      )
      expect None = input_state
      let subject = substrate.open_subject_v1(
        thread_name,
        substrate.ForcedSource { header, membership },
      )
      let compact = decode_forced_tx_compact_v1(
        membership.value.submitted_source.compact_cbor,
      )
      let expected: Data = SourceStateV1 {
        subject,
        header,
        compact: TxCompactView {
          body: compact.body,
          witness_set_hash: compact.witness_set_hash,
          validity_code: None,
        },
      }
      output_hash == next_hash && output_state == expected
    }
  }
}
