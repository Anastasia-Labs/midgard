//// Engine selectors for the mint-authorization family: the §5.6 mint-item
//// reader's canonicality wall and the machine-twin evaluator's refusal
//// classes that the step-03 selectors do not reach through the validator.

use aiken/primitive/bytearray
use midgard/fraud_proofs/mint_authorization/engine
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

const policy_a: ByteArray =
  #"42424242424242424242424242424242424242424242424242424242"

const absent_key: ByteArray =
  #"99999999999999999999999999999999999999999999999999999999"

fn item_of(policy_id: ByteArray, assets_map: ByteArray) -> ByteArray {
  #"82"
    |> bytearray.concat(#"581c")
    |> bytearray.concat(policy_id)
    |> bytearray.concat(assets_map)
}

/// The reader returns the 28-byte policy id of a canonical item.
test mint_authorization_engine_reads_a_canonical_item() {
  // {"A": 1, "AB": 2} — canonical: length-first key order.
  engine.policy_id_of_mint_item_v1(item_of(policy_a, #"a241410142414202")) == policy_a
}

/// A burn entry is a mint purpose like any other: negative quantities pass
/// the reader (the machine derives one purpose per canonical field-5 item,
/// sign-agnostic).
test mint_authorization_engine_reads_a_burn_item() {
  // {"A": -1}
  engine.policy_id_of_mint_item_v1(item_of(policy_a, #"a1414120")) == policy_a
}

/// Keys out of canonical order are not a committed item the machine could
/// have derived a purpose from.
test mint_authorization_engine_rejects_out_of_order_asset_names() fail {
  // {"AB": 2, "A": 1} — longer key first.
  engine.policy_id_of_mint_item_v1(item_of(policy_a, #"a242414202414101")) == policy_a
}

/// An asset name longer than 32 bytes is outside §5.6's grammar.
test mint_authorization_engine_rejects_an_oversized_asset_name() fail {
  let long_name = fixture.repeated_bytes_v1(#"41", 33)
  let assets_map = #"a1"
    |> bytearray.concat(#"5821")
    |> bytearray.concat(long_name)
    |> bytearray.concat(#"01")
  engine.policy_id_of_mint_item_v1(item_of(policy_a, assets_map)) == policy_a
}

/// Bytes past the asset map make the item non-canonical: the reader demands
/// exact consumption.
test mint_authorization_engine_rejects_trailing_item_bytes() fail {
  engine.policy_id_of_mint_item_v1(item_of(policy_a, #"a141410100")) == policy_a
}

/// An empty asset map carries no mintable entry — §5.6 demands at least one.
test mint_authorization_engine_rejects_an_empty_asset_map() fail {
  engine.policy_id_of_mint_item_v1(item_of(policy_a, #"a0")) == policy_a
}

// ## Evaluator refusal classes

fn sig_script(key_hash: ByteArray) -> ByteArray {
  #"8200581c" |> bytearray.concat(key_hash)
}

/// The machine's node budget is 16384: the 16385th node is a counted
/// refusal, not a verdict — exactly `reject_native_script_node_count`'s
/// class, which step-03 maps to refusal (the decoding family's claim).
test mint_authorization_engine_refuses_past_the_node_budget() {
  let payload = fixture.repeated_bytes_v1(#"820181", 16384)
    |> bytearray.concat(sig_script(absent_key))
  engine.evaluate_native_script_v1(payload, [], -1, -1) == engine.ScriptNodeLimitV1
}

/// Bytes past a complete script are a malformed payload, not an evaluated
/// one.
test mint_authorization_engine_refuses_trailing_script_bytes() {
  let payload = sig_script(absent_key) |> bytearray.concat(#"00")
  engine.evaluate_native_script_v1(payload, [], -1, -1) == engine.ScriptMalformedV1
}

/// The empty payload is malformed — there is no script to evaluate.
test mint_authorization_engine_refuses_an_empty_payload() {
  engine.evaluate_native_script_v1(#"", [], -1, -1) == engine.ScriptMalformedV1
}

/// An unknown node tag is malformed.
test mint_authorization_engine_refuses_an_unknown_tag() {
  engine.evaluate_native_script_v1(#"820700", [], -1, -1) == engine.ScriptMalformedV1
}

/// A timelock against the absent interval sentinel (-1) is unsatisfied in
/// both directions — the machine's `>= 0` guards.
test mint_authorization_engine_finds_timelocks_unsatisfied_without_interval() {
  and {
    engine.evaluate_native_script_v1(#"820400", [], -1, -1) == engine.ScriptEvaluatedV1 {
      satisfied: False,
    },
    engine.evaluate_native_script_v1(#"820500", [], -1, -1) == engine.ScriptEvaluatedV1 {
      satisfied: False,
    },
  }
}

/// `at_least` with `required == 0` over an empty child list is satisfied by
/// the machine's own empty-container rule.
test mint_authorization_engine_finds_zero_of_zero_satisfied() {
  engine.evaluate_native_script_v1(#"83030080", [], -1, -1) == engine.ScriptEvaluatedV1 {
    satisfied: True,
  }
}

/// Nested containers evaluate through the frame stack: an `any` holding a
/// failed `all` and a met `sig` is satisfied.
test mint_authorization_engine_evaluates_nested_containers() {
  let met = sig_script(policy_a)
  let failed_all = #"820181" |> bytearray.concat(sig_script(absent_key))
  let payload = #"820282"
    |> bytearray.concat(failed_all)
    |> bytearray.concat(met)
  engine.evaluate_native_script_v1(payload, [policy_a], -1, -1) == engine.ScriptEvaluatedV1 {
    satisfied: True,
  }
}
