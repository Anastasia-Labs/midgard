use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{InlineDatum, Input, Output, Spend, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/transition_trace/timed_history.{Reference}

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  event_reference: Reference,
  completed_fraud_witness: Data,
  yield_ref_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

pub const yield_role = "V1FpTtF6L1EventYield"

/// Match the one actual dispatcher spend. A second compatible input is rejected
/// so one rewarding invocation cannot discharge multiple terminal spends.
pub fn dispatch(dispatcher: ScriptHash, tx: Transaction) -> (Input, Datum, Args) {
  expect [input] = list.filter(
    tx.inputs,
    fn(input) { input.output.address.payment_credential == Script(dispatcher) },
  )
  expect Output { datum: InlineDatum(raw), .. } = input.output
  expect [Pair(_, redeemer)] = list.filter(
    tx.redeemers,
    fn(pair) { pair.1st == Spend(input.output_reference) },
  )
  expect ct.Continue(args): SpendRedeemer = redeemer
  expect datum: Datum = raw
  (input, datum, args)
}

pub const forced_yield_role = "V1FpTtF6ForcedTimeYield"

/// The spending side selects the semantic target from the carried proof, not
/// from a prover-selected role. Each yield also validates its own variant.
pub fn role_for(raw: Data) -> ByteArray {
  expect builtin.unconstr_index(raw) == 0
  expect [_, _, fault] = builtin.unconstr_fields(raw)
  let tag = builtin.unconstr_index(fault)
  expect tag == 5 || tag == 7
  expect [witness] = builtin.unconstr_fields(fault)
  when builtin.unconstr_index(witness) is {
    0 -> yield_role
    1 -> yield_role
    2 -> forced_yield_role
    _ -> fail
  }
}
