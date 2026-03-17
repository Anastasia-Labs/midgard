use aiken/builtin
use aiken/collection/list

test option_none_ctor0() {
  let d = builtin.constr_data(0, [])
  expect _x: Option<Int> = d
  True
}

test option_none_ctor1() {
  let d = builtin.constr_data(1, [])
  expect _x: Option<Int> = d
  True
}

test option_some_ctor0() {
  let d = builtin.constr_data(0, [builtin.i_data(1)])
  expect _x: Option<Int> = d
  True
}

test option_some_ctor1() {
  let d = builtin.constr_data(1, [builtin.i_data(1)])
  expect _x: Option<Int> = d
  True
}
