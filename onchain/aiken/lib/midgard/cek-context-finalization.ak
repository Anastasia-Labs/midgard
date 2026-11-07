use midgard/cek_context_chain as chain
use midgard/cek_data_v1
use midgard/validation_machine/machine_types.{
  CekContextControlV1, CekContextPartsControlV1,
}
use midgard/validation_machine/witness.{hash_cek_context_parts_control_v1}

pub type FinalizationPending {
  staged: Data,
  redeemer_items: cek_data_v1.DataSequenceSummaryV1,
  redeemer: cek_data_v1.DataSummaryV1,
}

pub fn finalized(
  pending: FinalizationPending,
  staged: chain.ContextStaged,
  script_info: cek_data_v1.DataSummaryV1,
) -> chain.ContextVerified {
  let parts = CekContextPartsControlV1 {
    redeemer_items: pending.redeemer_items,
    redeemer: pending.redeemer,
    script_info,
  }
  chain.ContextVerified {
    staged: pending.staged,
    successor: chain.ContinueContext {
      next: CekContextControlV1 {
        ..staged.context,
        stage: 11,
        redeemer_context_control_hash: hash_cek_context_parts_control_v1(parts),
      },
    },
  }
}
