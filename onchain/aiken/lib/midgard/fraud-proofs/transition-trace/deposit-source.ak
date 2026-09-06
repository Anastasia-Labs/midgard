use aiken/builtin
use aiken/cbor
use cardano/transaction.{Input, OutputReference}
use midgard/fraud_proofs/transition_trace/final_yield.{Args, State}
use midgard/fraud_proofs/transition_trace/proof

pub fn open(
  state: State,
  args: Args,
  references: List<Input>,
) -> (proof.AuthenticatedDepositReference, ByteArray, ByteArray) {
  expect Some(data) = cbor.deserialise(state.deposit_source_cbor)
  expect [out_ref_data, policy_data, asset_data] = builtin.un_list_data(data)
  expect out_ref: OutputReference = out_ref_data
  expect policy: ByteArray = policy_data
  expect asset: ByteArray = asset_data
  let reference = proof.get_authenticated_deposit_reference(
    references,
    policy,
    asset,
    args.deposit_event_ref_index,
  )
  expect reference.out_ref == out_ref
  (reference, policy, asset)
}
