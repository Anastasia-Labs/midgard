//// Duplicated from `aiken-design-patterns` to simply change the type
//// signatures.

use aiken/builtin
use aiken/crypto.{Blake2b_256, Hash, ScriptHash, blake2b_224}
use aiken/primitive/bytearray
use cardano/transaction.{Transaction}

const postfix: ByteArray = #"0001"

/// Applies a pre-hashed validator parameter to the wrapped handler.
/// Applies a prehashed parameter to a single-argument validator.
pub fn apply_prehashed_param(
  version: Int,
  prefix: ByteArray,
  param: ByteArray,
) -> ScriptHash {
  builtin.integer_to_bytearray(True, 1, version)
    |> bytearray.concat(prefix)
    |> bytearray.concat(param)
    |> bytearray.concat(postfix)
    |> blake2b_224
}
/// Builds a no-datum wrapper for a pre-hashed validator parameter.

/// Wraps a prehashed-parameter validator that does not consume a datum.
pub fn prehashed_param_no_datum_wrapper(
  validator_function: fn(
    Hash<Blake2b_256, a>,
    redeemer,
    endpoint_specific,
    Transaction,
  ) ->
    Bool,
  param: Hash<Blake2b_256, a>,
  redeemer: redeemer,
  variable_arg: endpoint_specific,
  tx: Transaction,
) -> Bool {
  validator_function(param, redeemer, variable_arg, tx)
}
