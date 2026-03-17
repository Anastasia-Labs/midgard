use aiken/builtin
use midgard/active_operators.{Datum}

test decode_none_datum() {
  let d = builtin.constr_data(0, [builtin.constr_data(1, [])])
  expect _x: Datum = d
  True
}

test decode_some_datum() {
  let d = builtin.constr_data(0, [builtin.constr_data(0, [builtin.i_data(123)])])
  expect _x: Datum = d
  True
}
