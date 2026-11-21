use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/cek_write_bits_loop_v1.{
  WriteBitsHop, advance_write_bits_failure_v1, advance_write_bits_v1,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}

/// writeBits (tag 80) over an index list of any length, hop by hop: a success
/// after the budget hop (semantics index 12) or a failure after the failure
/// roots hop. Neither hop's tag is read here: only writeBits is routed in.
validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.CoreDatum>,
    redeemer: cek_write_bits_loop_v1.WriteBitsRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(WriteBitsHop { input_index, output_index, witness, end, hop }) -> {
        expect Some(step) = datum
        expect Some(bound) = step.data
        expect bound.pre.mode == machine.mode_builtin
        let action = chain.LoopArm { input_index, output_index, witness, end }
        if bound.arm == 31 {
          expect bound.group == 5
          chain.continue_loop(
            settle_script_hash,
            computation_thread_policy_id,
            step,
            action,
            3,
            own_out_ref,
            tx,
            fn(inner, from, end) {
              advance_write_bits_v1(bound, inner, hop, from, end)
            },
          )
        } else {
          expect bound.arm == 36
          expect bound.group == 7
          chain.continue_loop(
            settle_script_hash,
            computation_thread_policy_id,
            step,
            action,
            1,
            own_out_ref,
            tx,
            fn(_inner, from, end) {
              advance_write_bits_failure_v1(bound, witness, hop, from, end)
            },
          )
        }
      }
    }
  }

  else(_) {
    fail
  }
}
