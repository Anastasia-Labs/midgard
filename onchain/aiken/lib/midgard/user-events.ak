use aiken/builtin
use cardano/transaction.{OutputReference}

/// Wrapper datatype for both deposit and withdraw events. This allows a unified
/// serialization function to be defined.
pub type Event<info> {
  id: OutputReference,
  info: info,
}

pub fn unsafe_to_key_value_pair(event_datum_data: Data) -> Pair<Data, Data> {
  expect [id_data, info_data, ..] = builtin.unconstr_fields(event_datum_data)
  Pair(id_data, info_data)
}
