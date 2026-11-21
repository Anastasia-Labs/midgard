use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/script_sources_descriptor.{DescriptorStepClaim}
use midgard/validation_machine/script_sources_late_stages
use midgard/validation_semantic_yield_v1

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let (_, transition, extra) = validation_semantic_yield_v1.unique_semantic_dispatch_v1(
      dispatcher_script_hashes,
      tx,
    )
    expect [claim_data, _] = extra
    expect claim: DescriptorStepClaim = claim_data
    script_sources_late_stages.verify_descriptor_step_claim(transition, claim)
  }

  else(_) {
    fail
  }
}
