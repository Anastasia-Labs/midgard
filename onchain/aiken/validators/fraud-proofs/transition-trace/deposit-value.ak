use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/address.{Credential}
use cardano/assets
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/transition_trace/deposit_source
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_value_v1 as value
use midgard/user_events

fn batch(
  control: value.LedgerOutputValueControlV1,
  facts: scan.LedgerOutputScanControlV1,
  witnesses: List<value.LedgerOutputValueWitnessV1>,
) -> value.LedgerOutputValueControlV1 {
  when witnesses is {
    [] -> control
    [witness, ..rest] -> {
      expect Some(next) = value.step_v1(
        control,
        facts.asset_count,
        facts.asset_peaks,
        facts.lovelace,
        witness,
      )
      batch(next, facts, rest)
    }
  }
}

fn policy_of(item: (value.LedgerOutputValueWitnessV1, Int)) -> ByteArray {
  expect value.LedgerOutputValueAsset { policy_id, .. } = item.1st
  policy_id
}

fn source_matches(
  requested: List<(value.LedgerOutputValueWitnessV1, Int)>,
  source: Data,
  current_policy: ByteArray,
  next_index: Int,
  remaining: List<Pair<Data, Data>>,
  event_policy: ByteArray,
  event_asset: ByteArray,
) -> Bool {
  when requested is {
    [] -> True
    [item, ..rest] -> {
      expect
          value.LedgerOutputValueAsset { policy_id, asset_name, quantity, .. }
        = item.1st
      let (start, tokens) = if policy_id == current_policy {
        (next_index, remaining)
      } else {
        (0, source_tokens(source, policy_id))
      }
      expect item.2nd >= start
      expect [actual, ..tail] = list.drop(tokens, item.2nd - start)
      and {
        policy_id != event_policy || asset_name != event_asset,
        actual == Pair(builtin.b_data(asset_name), builtin.i_data(quantity)),
        source_matches(
          rest,
          source,
          policy_id,
          item.2nd + 1,
          tail,
          event_policy,
          event_asset,
        ),
      }
    }
  }
}

// Source has passed the complete captured-Value hash check in deposit_source.open.
// Decode only the requested policy and entries; revalidating the full map on
// every bounded step needlessly scales each transaction with all source assets.
fn source_tokens(source: Data, policy: ByteArray) -> List<Pair<Data, Data>> {
  when
    list.find(
      builtin.un_map_data(source),
      fn(pair) { pair.1st == builtin.b_data(policy) },
    )
  is {
    None -> []
    Some(Pair(_, tokens)) -> builtin.un_map_data(tokens)
  }
}

fn source_lovelace(source: Data) -> Int {
  when source_tokens(source, #"") is {
    [] -> 0
    [Pair(name, quantity)] -> {
      expect name == builtin.b_data(#"")
      builtin.un_i_data(quantity)
    }
    _ -> fail
  }
}

validator value_output(dispatcher: ScriptHash) {
  withdraw(
    indexed: List<(value.LedgerOutputValueWitnessV1, Int)>,
    _credential: Credential,
    tx: Transaction,
  ) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 7 && state.kind == 1
    let witnesses = list.map(indexed, fn(pair) { pair.1st })
    expect !list.is_empty(witnesses) && list.length(witnesses) <= 4
    let facts = scan.decode_control_v1(state.scan_cbor)
    let reference = deposit_source.open(state, args)
    let policy = reference.commitment.policy
    let asset = user_events.out_ref_to_nonce(reference.commitment.event_id)
    expect facts.asset_count == state.deposit_asset_count
    expect facts.lovelace == source_lovelace(reference.value)
    expect
      list.all(
        indexed,
        fn(pair) {
          when pair.1st is {
            value.LedgerOutputValueNoWitness -> pair.2nd == -1
            _ -> pair.2nd >= 0
          }
        },
      )
    let requested = indexed
      |> list.filter(fn(pair) { pair.1st != value.LedgerOutputValueNoWitness })
      |> list.sort(
          fn(left, right) {
            if policy_of(left) == policy_of(right) {
              int.compare(left.2nd, right.2nd)
            } else {
              bytearray.compare(policy_of(left), policy_of(right))
            }
          },
        )
    expect source_matches(requested, reference.value, #"", 0, [], policy, asset)
    let control = if state.value_cbor == #"" {
      value.initial_control_v1(facts.asset_count)
    } else {
      value.decode_control_v1(state.value_cbor)
    }
    let next = batch(control, facts, witnesses)
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State {
        ..state,
        phase: if next.stage == value.stage_terminal {
          10
        } else {
          7
        },
        value_cbor: value.encode_control_v1(next),
        value_summary: value.finalize_v1(next),
      },
    )
  }

  else(_) {
    fail
  }
}

fn requested_asset(
  name: ByteArray,
  quantity: Int,
  index: Int,
) -> (value.LedgerOutputValueWitnessV1, Int) {
  (
    value.LedgerOutputValueAsset {
      asset_index: index,
      policy_id: #"11111111111111111111111111111111111111111111111111111111",
      asset_name: name,
      quantity,
      siblings: [],
      previous: None,
    },
    index,
  )
}

test shared_source_walk_checks_exact_names_and_quantities() {
  let policy = #"11111111111111111111111111111111111111111111111111111111"
  let source = assets.from_asset(policy, #"01", 7)
    |> assets.add(policy, #"02", 8)
    |> assets.add(policy, #"03", 9)
  and {
    source_matches(
      [requested_asset(#"01", 7, 0), requested_asset(#"03", 9, 2)],
      source,
      #"",
      0,
      [],
      #"",
      #"",
    ),
    !source_matches(
      [requested_asset(#"01", 8, 0)],
      source,
      #"",
      0,
      [],
      #"",
      #"",
    ),
    !source_matches(
      [requested_asset(#"02", 7, 0)],
      source,
      #"",
      0,
      [],
      #"",
      #"",
    ),
    !source_matches(
      [requested_asset(#"01", 7, 0)],
      source,
      #"",
      0,
      [],
      policy,
      #"01",
    ),
  }
}

test shared_source_walk_rejects_repeated_source_index() fail {
  let policy = #"11111111111111111111111111111111111111111111111111111111"
  source_matches(
    [requested_asset(#"01", 7, 0), requested_asset(#"01", 7, 0)],
    assets.from_asset(policy, #"01", 7),
    #"",
    0,
    [],
    #"",
    #"",
  )
}
