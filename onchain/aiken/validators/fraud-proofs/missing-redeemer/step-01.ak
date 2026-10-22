use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/direct_script_derivation/types.{SourceBoundV1}
use midgard/fraud_proofs/missing_redeemer/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/source_carriage_v1

/// Opens the verdict subject and hands the direct derivation its source bound.
/// The accepted compact bytes are read only from the carriage the inclusion
/// helper has already authenticated against the subject's transaction id; the
/// forced compact bytes are the leaf's own, verified by the forced opener.
validator main(
  next_step_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args{source: AcceptedSource { inclusion }}) -> {
        let _, _, _, _, next_hash, next_state, header, _, verified <- pass_native_tx_to_next_step_carried(
          computation_thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let expected: Data = SourceBoundV1 {
          subject: substrate.open_accepted_subject_v1(verified),
          header,
          compact_cbor: source_carriage_v1.carriage_compact_cbor_v1(inclusion),
        }
        and {
          next_hash == next_step_hash,
          next_state == expected,
        }
      }
      ct.Continue(Args{source: ForcedSource {
        input_index,
        output_index,
        header,
        membership,
      }}) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_name, _, _, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        let Pair(subject, _) = substrate.open_forced_subject_v1(
          thread_name,
          header,
          membership,
        )
        let expected: Data = SourceBoundV1 {
          subject,
          header,
          compact_cbor: membership.value.submitted_source.compact_cbor,
        }
        and {
          next_hash == next_step_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
