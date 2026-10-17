use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_value_v1 as value

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

validator value_output(dispatcher: ScriptHash) {
  withdraw(
    witnesses: List<value.LedgerOutputValueWitnessV1>,
    _credential: Credential,
    tx: Transaction,
  ) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 7 && state.kind != 2
    expect !list.is_empty(witnesses) && list.length(witnesses) <= 8
    let facts = scan.decode_control_v1(state.scan_cbor)
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
          8
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
