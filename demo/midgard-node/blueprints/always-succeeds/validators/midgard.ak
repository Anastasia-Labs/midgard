use aiken/builtin
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/address.{Credential, Script}
use cardano/assets.{AssetName, PolicyId, quantity_of}
use cardano/script_context.{ScriptContext, ScriptInfo, Spending}
use cardano/transaction.{
  Input, Mint, OutputReference, ScriptPurpose, Spend, Transaction, Withdraw,
}

pub type MidgardOutputReference {
  MidgardOutputReference { transaction_id: ByteArray, output_index: Int }
}

pub type MidgardCredential {
  MidgardVerificationKey(ByteArray)
  MidgardScript(ByteArray)
}

pub type MidgardAddress {
  MidgardAddress {
    payment_credential: MidgardCredential,
    stake_credential: Data,
  }
}

pub type MidgardOutput {
  MidgardOutput {
    address: MidgardAddress,
    value: Data,
    datum: Data,
    reference_script: Data,
  }
}

pub type MidgardInput {
  MidgardInput {
    output_reference: MidgardOutputReference,
    output: MidgardOutput,
  }
}

pub type MidgardTxInfo {
  MidgardTxInfo {
    inputs: List<MidgardInput>,
    reference_inputs: List<MidgardInput>,
    outputs: List<MidgardOutput>,
    fee: Int,
    validity_range: Data,
    observers: List<ByteArray>,
    signatories: List<ByteArray>,
    mint: Data,
    redeemers: Data,
    id: ByteArray,
  }
}

pub type MidgardContextProbeTxInfo {
  MidgardContextProbeTxInfo {
    inputs: List<MidgardInput>,
    reference_inputs: List<MidgardInput>,
    outputs: List<MidgardOutput>,
    fee: Int,
    validity_range: Data,
    observers: List<ByteArray>,
    signatories: List<ByteArray>,
    mint: Pairs<ByteArray, Pairs<ByteArray, Int>>,
    redeemers: Pairs<MidgardScriptPurpose, Data>,
    id: ByteArray,
  }
}

pub type MidgardScriptPurpose {
  MidgardMint(ByteArray)
  MidgardSpend(ByteArray, MidgardOutputReference)
  MidgardObserve(ByteArray)
  MidgardReceive(ByteArray)
}

pub type MidgardScriptContext {
  MidgardScriptContext {
    transaction: MidgardTxInfo,
    redeemer: Int,
    purpose: MidgardScriptPurpose,
  }
}

pub type MidgardOutputReferenceRedeemerScriptContext {
  MidgardOutputReferenceRedeemerScriptContext {
    transaction: MidgardTxInfo,
    redeemer: MidgardOutputReference,
    purpose: MidgardScriptPurpose,
  }
}

pub type MidgardV1ContextProbeRedeemer {
  MidgardV1ContextProbeRedeemer {
    expected_spend_script_hash: ByteArray,
    expected_own_ref: MidgardOutputReference,
    expected_first_input: MidgardOutputReference,
    expected_second_input: MidgardOutputReference,
    expected_first_reference: MidgardOutputReference,
    expected_second_reference: MidgardOutputReference,
    expected_first_output_script_hash: ByteArray,
    expected_second_output_script_hash: ByteArray,
    expected_signer: ByteArray,
    expected_observer: ByteArray,
    expected_policy: ByteArray,
    expected_asset_name: ByteArray,
    expected_mint_quantity: Int,
    expected_mint_redeemer: Data,
    expected_observe_redeemer: Data,
    expected_receive_script_hash: ByteArray,
    expected_receive_redeemer: Data,
  }
}

pub type MidgardV1ContextProbeScriptContext {
  MidgardV1ContextProbeScriptContext {
    transaction: MidgardContextProbeTxInfo,
    redeemer: MidgardV1ContextProbeRedeemer,
    purpose: MidgardScriptPurpose,
  }
}

pub type PlutusContextProbeRedeemer {
  PlutusContextProbeRedeemer {
    expected_output_reference: OutputReference,
    expected_datum: Int,
    expected_signer: VerificationKeyHash,
    expected_first_reference: OutputReference,
    expected_second_reference: OutputReference,
    expected_policy: PolicyId,
    expected_asset_name: AssetName,
    expected_mint_quantity: Int,
    expected_observer: ScriptHash,
  }
}

fn midgard_output_reference_eq(
  left: MidgardOutputReference,
  right: MidgardOutputReference,
) -> Bool {
  and {
    left.transaction_id == right.transaction_id,
    left.output_index == right.output_index,
  }
}

fn midgard_inputs_contain_ref(
  inputs: List<MidgardInput>,
  expected: MidgardOutputReference,
) -> Bool {
  when inputs is {
    [] -> False
    [input, ..rest] ->
      midgard_output_reference_eq(input.output_reference, expected) || midgard_inputs_contain_ref(
        rest,
        expected,
      )
  }
}

fn midgard_inputs_begin_with(
  inputs: List<MidgardInput>,
  expected_first: MidgardOutputReference,
  expected_second: MidgardOutputReference,
) -> Bool {
  when inputs is {
    [first, second, ..] -> and {
        midgard_output_reference_eq(first.output_reference, expected_first),
        midgard_output_reference_eq(second.output_reference, expected_second),
      }
    _ -> False
  }
}

fn midgard_script_output_matches(
  output: MidgardOutput,
  expected_script_hash: ByteArray,
) -> Bool {
  when output.address.payment_credential is {
    MidgardScript(actual_script_hash) -> actual_script_hash == expected_script_hash
    _ -> False
  }
}

fn midgard_outputs_begin_with_script_hashes(
  outputs: List<MidgardOutput>,
  expected_first: ByteArray,
  expected_second: ByteArray,
) -> Bool {
  when outputs is {
    [first, second, ..] -> and {
        midgard_script_output_matches(first, expected_first),
        midgard_script_output_matches(second, expected_second),
      }
    _ -> False
  }
}

fn midgard_outputs_contain_script_hash(
  outputs: List<MidgardOutput>,
  expected_script_hash: ByteArray,
) -> Bool {
  when outputs is {
    [] -> False
    [output, ..rest] ->
      when output.address.payment_credential is {
        MidgardScript(actual_script_hash) ->
          actual_script_hash == expected_script_hash || midgard_outputs_contain_script_hash(
            rest,
            expected_script_hash,
          )
        _ -> midgard_outputs_contain_script_hash(rest, expected_script_hash)
      }
  }
}

fn midgard_bytearray_present(
  values: List<ByteArray>,
  expected: ByteArray,
) -> Bool {
  when values is {
    [] -> False
    [value, ..rest] -> value == expected || midgard_bytearray_present(
        rest,
        expected,
      )
  }
}

fn midgard_asset_quantity_of(
  assets: Pairs<ByteArray, Int>,
  expected_asset_name: ByteArray,
) -> Int {
  when assets is {
    [] -> 0
    [Pair(asset_name, quantity), ..rest] ->
      if asset_name == expected_asset_name {
        quantity
      } else {
        midgard_asset_quantity_of(rest, expected_asset_name)
      }
  }
}

fn midgard_quantity_of(
  mint: Pairs<ByteArray, Pairs<ByteArray, Int>>,
  expected_policy: ByteArray,
  expected_asset_name: ByteArray,
) -> Int {
  when mint is {
    [] -> 0
    [Pair(policy, assets), ..rest] ->
      if policy == expected_policy {
        midgard_asset_quantity_of(assets, expected_asset_name)
      } else {
        midgard_quantity_of(rest, expected_policy, expected_asset_name)
      }
  }
}

fn midgard_redeemer_map_has_spend(
  redeemers: Pairs<MidgardScriptPurpose, Data>,
  expected_script_hash: ByteArray,
  expected_output_reference: MidgardOutputReference,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, _), ..rest] ->
      when purpose is {
        MidgardSpend(script_hash, output_reference) -> or {
            and {
              script_hash == expected_script_hash,
              midgard_output_reference_eq(
                output_reference,
                expected_output_reference,
              ),
            },
            midgard_redeemer_map_has_spend(
              rest,
              expected_script_hash,
              expected_output_reference,
            ),
          }
        _ ->
          midgard_redeemer_map_has_spend(
            rest,
            expected_script_hash,
            expected_output_reference,
          )
      }
  }
}

fn midgard_redeemer_map_has_mint(
  redeemers: Pairs<MidgardScriptPurpose, Data>,
  expected_policy: ByteArray,
  expected_redeemer: Data,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, redeemer), ..rest] ->
      when purpose is {
        MidgardMint(policy) -> or {
            and { policy == expected_policy, redeemer == expected_redeemer },
            midgard_redeemer_map_has_mint(
              rest,
              expected_policy,
              expected_redeemer,
            ),
          }
        _ ->
          midgard_redeemer_map_has_mint(
            rest,
            expected_policy,
            expected_redeemer,
          )
      }
  }
}

fn midgard_redeemer_map_has_observe(
  redeemers: Pairs<MidgardScriptPurpose, Data>,
  expected_observer: ByteArray,
  expected_redeemer: Data,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, redeemer), ..rest] ->
      when purpose is {
        MidgardObserve(observer) -> or {
            and {
              observer == expected_observer,
              redeemer == expected_redeemer,
            },
            midgard_redeemer_map_has_observe(
              rest,
              expected_observer,
              expected_redeemer,
            ),
          }
        _ ->
          midgard_redeemer_map_has_observe(
            rest,
            expected_observer,
            expected_redeemer,
          )
      }
  }
}

fn midgard_redeemer_map_has_receive(
  redeemers: Pairs<MidgardScriptPurpose, Data>,
  expected_script_hash: ByteArray,
  expected_redeemer: Data,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, redeemer), ..rest] ->
      when purpose is {
        MidgardReceive(script_hash) -> or {
            and {
              script_hash == expected_script_hash,
              redeemer == expected_redeemer,
            },
            midgard_redeemer_map_has_receive(
              rest,
              expected_script_hash,
              expected_redeemer,
            ),
          }
        _ ->
          midgard_redeemer_map_has_receive(
            rest,
            expected_script_hash,
            expected_redeemer,
          )
      }
  }
}

fn output_reference_eq(left: OutputReference, right: OutputReference) -> Bool {
  and {
    left.transaction_id == right.transaction_id,
    left.output_index == right.output_index,
  }
}

fn reference_inputs_begin_with(
  references: List<Input>,
  expected_first: OutputReference,
  expected_second: OutputReference,
) -> Bool {
  when references is {
    [first, second, ..] -> and {
        output_reference_eq(first.output_reference, expected_first),
        output_reference_eq(second.output_reference, expected_second),
      }
    _ -> False
  }
}

fn signer_present(
  signers: List<VerificationKeyHash>,
  expected: VerificationKeyHash,
) -> Bool {
  when signers is {
    [] -> False
    [signer, ..rest] -> signer == expected || signer_present(rest, expected)
  }
}

fn withdrawal_present(
  withdrawals: Pairs<Credential, Int>,
  expected_observer: ScriptHash,
) -> Bool {
  when withdrawals is {
    [] -> False
    [Pair(credential, _), ..rest] ->
      when credential is {
        Script(script_hash) ->
          script_hash == expected_observer || withdrawal_present(
            rest,
            expected_observer,
          )
        _ -> withdrawal_present(rest, expected_observer)
      }
  }
}

fn redeemer_map_has_spend(
  redeemers: Pairs<ScriptPurpose, Data>,
  expected_output_reference: OutputReference,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, _), ..rest] ->
      when purpose is {
        Spend(output_reference) ->
          output_reference_eq(output_reference, expected_output_reference) || redeemer_map_has_spend(
            rest,
            expected_output_reference,
          )
        _ -> redeemer_map_has_spend(rest, expected_output_reference)
      }
  }
}

fn redeemer_map_has_mint(
  redeemers: Pairs<ScriptPurpose, Data>,
  expected_policy: PolicyId,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, _), ..rest] ->
      when purpose is {
        Mint(policy) ->
          policy == expected_policy || redeemer_map_has_mint(
            rest,
            expected_policy,
          )
        _ -> redeemer_map_has_mint(rest, expected_policy)
      }
  }
}

fn redeemer_map_has_withdrawal(
  redeemers: Pairs<ScriptPurpose, Data>,
  expected_observer: ScriptHash,
) -> Bool {
  when redeemers is {
    [] -> False
    [Pair(purpose, _), ..rest] ->
      when purpose is {
        Withdraw(Script(script_hash)) ->
          script_hash == expected_observer || redeemer_map_has_withdrawal(
            rest,
            expected_observer,
          )
        _ -> redeemer_map_has_withdrawal(rest, expected_observer)
      }
  }
}

fn script_info_matches_spend(
  info: ScriptInfo,
  expected_output_reference: OutputReference,
  expected_datum: Int,
) -> Bool {
  when info is {
    Spending { output, datum } -> {
      expect Some(actual_datum_data) = datum
      expect actual_datum: Int = actual_datum_data
      and {
        output_reference_eq(output, expected_output_reference),
        actual_datum == expected_datum,
      }
    }
    _ -> False
  }
}

validator hub_oracle_mint {
  else(_) {
    trace @"Midgard Demo – Hub Oracle Mint"
    True
  }
}

validator scheduler_mint {
  else(_) {
    trace @"Midgard Demo – Scheduler Mint"
    True
  }
}

validator scheduler_spend {
  else(_) {
    trace @"Midgard Demo – Scheduler Spend"
    True
  }
}

validator state_queue_spend {
  else(_) {
    trace @"Midgard Demo – State Queue Spend"
    True
  }
}

validator state_queue_mint {
  else(_) {
    trace @"Midgard Demo – State Queue Mint"
    True
  }
}

validator registered_operators_mint {
  else(_) {
    trace @"Midgard Demo – Registered Operators Mint"
    True
  }
}

validator registered_operators_spend {
  else(_) {
    trace @"Midgard Demo – Registered Operators Spend"
    True
  }
}

validator active_operators_mint {
  else(_) {
    trace @"Midgard Demo – Active Operators Mint"
    True
  }
}

validator active_operators_spend {
  else(_) {
    trace @"Midgard Demo – Active Operators Spend"
    True
  }
}

validator retired_operators_mint {
  else(_) {
    trace @"Midgard Demo – Retired Operators Mint"
    True
  }
}

validator retired_operators_spend {
  else(_) {
    trace @"Midgard Demo – Retired Operators Spend"
    True
  }
}

validator escape_hatch_mint {
  else(_) {
    trace @"Midgard Demo – Escape Hatch Mint"
    True
  }
}

validator escape_hatch_spend {
  else(_) {
    trace @"Midgard Demo – Escape Hatch Spend"
    True
  }
}

validator fraud_proof_catalogue_mint {
  else(_) {
    trace @"Midgard Demo – Fraud Proof Catalogue Mint"
    True
  }
}

validator fraud_proof_catalogue_spend {
  else(_) {
    trace @"Midgard Demo – Fraud Proof Catalogue Spend"
    True
  }
}

validator fraud_proof_mint {
  else(_) {
    trace @"Midgard Demo – Fraud Proof Mint"
    True
  }
}

validator fraud_proof_spend {
  else(_) {
    trace @"Midgard Demo – Fraud Proof Spend"
    True
  }
}

validator deposit_spend {
  else(_) {
    trace @"Midgard Demo – Deposit Spend"
    True
  }
}

validator deposit_mint {
  else(_) {
    trace @"Midgard Demo – Deposit Mint"
    True
  }
}

validator reserve_spend {
  else(_) {
    trace @"Midgard Demo – Reserve Spend"
    True
  }
}

validator reserve_withdraw {
  else(_) {
    trace @"Midgard Demo – Reserve Withdraw"
    True
  }
}

validator always_fail {
  else(_) {
    trace @"Midgard Demo – Always Fail"
    fail
  }
}

validator datum_equals_redeemer_spend {
  spend(
    datum: Option<Int>,
    redeemer: Int,
    _own_out_ref: OutputReference,
    _tx: Transaction,
  ) {
    expect Some(actual_datum) = datum
    actual_datum == redeemer
  }
}

validator midgard_v1_spend_guard {
  else(context: MidgardScriptContext) {
    when context.purpose is {
      MidgardSpend(script_hash, own_out_ref) -> and {
          context.redeemer == 42,
          builtin.length_of_bytearray(script_hash) == 28,
          midgard_inputs_contain_ref(context.transaction.inputs, own_out_ref),
        }
      _ -> False
    }
  }
}

validator midgard_v1_spend_out_ref_guard {
  else(context: MidgardOutputReferenceRedeemerScriptContext) {
    when context.purpose is {
      MidgardSpend(script_hash, own_out_ref) -> and {
          midgard_output_reference_eq(context.redeemer, own_out_ref),
          builtin.length_of_bytearray(script_hash) == 28,
          midgard_inputs_contain_ref(context.transaction.inputs, own_out_ref),
        }
      _ -> False
    }
  }
}

validator midgard_v1_receive_guard {
  else(context: MidgardScriptContext) {
    when context.purpose is {
      MidgardReceive(script_hash) -> and {
          context.redeemer == 99,
          builtin.length_of_bytearray(script_hash) == 28,
          midgard_outputs_contain_script_hash(
            context.transaction.outputs,
            script_hash,
          ),
        }
      _ -> False
    }
  }
}

validator midgard_v1_observe_guard {
  else(context: MidgardScriptContext) {
    when context.purpose is {
      MidgardObserve(script_hash) -> and {
          context.redeemer == 77,
          builtin.length_of_bytearray(script_hash) == 28,
          midgard_bytearray_present(context.transaction.observers, script_hash),
        }
      _ -> False
    }
  }
}

validator midgard_v1_always_fail {
  else(_) {
    fail
  }
}

validator midgard_v1_context_probe {
  else(context: MidgardV1ContextProbeScriptContext) {
    let redeemer = context.redeemer
    when context.purpose is {
      MidgardSpend(script_hash, own_out_ref) -> and {
          script_hash == redeemer.expected_spend_script_hash,
          midgard_output_reference_eq(
            own_out_ref,
            redeemer.expected_own_ref,
          ),
          midgard_inputs_begin_with(
            context.transaction.inputs,
            redeemer.expected_first_input,
            redeemer.expected_second_input,
          ),
          midgard_inputs_begin_with(
            context.transaction.reference_inputs,
            redeemer.expected_first_reference,
            redeemer.expected_second_reference,
          ),
          midgard_outputs_begin_with_script_hashes(
            context.transaction.outputs,
            redeemer.expected_first_output_script_hash,
            redeemer.expected_second_output_script_hash,
          ),
          midgard_bytearray_present(
            context.transaction.signatories,
            redeemer.expected_signer,
          ),
          midgard_bytearray_present(
            context.transaction.observers,
            redeemer.expected_observer,
          ),
          midgard_quantity_of(
            context.transaction.mint,
            redeemer.expected_policy,
            redeemer.expected_asset_name,
          ) == redeemer.expected_mint_quantity,
          midgard_redeemer_map_has_spend(
            context.transaction.redeemers,
            redeemer.expected_spend_script_hash,
            redeemer.expected_own_ref,
          ),
          midgard_redeemer_map_has_mint(
            context.transaction.redeemers,
            redeemer.expected_policy,
            redeemer.expected_mint_redeemer,
          ),
          midgard_redeemer_map_has_observe(
            context.transaction.redeemers,
            redeemer.expected_observer,
            redeemer.expected_observe_redeemer,
          ),
          midgard_redeemer_map_has_receive(
            context.transaction.redeemers,
            redeemer.expected_receive_script_hash,
            redeemer.expected_receive_redeemer,
          ),
          builtin.length_of_bytearray(context.transaction.id) == 32,
        }
      _ -> False
    }
  }
}

validator plutus_v3_context_probe {
  else(context: ScriptContext) {
    expect redeemer: PlutusContextProbeRedeemer = context.redeemer
    and {
      script_info_matches_spend(
        context.info,
        redeemer.expected_output_reference,
        redeemer.expected_datum,
      ),
      reference_inputs_begin_with(
        context.transaction.reference_inputs,
        redeemer.expected_first_reference,
        redeemer.expected_second_reference,
      ),
      signer_present(
        context.transaction.extra_signatories,
        redeemer.expected_signer,
      ),
      quantity_of(
        context.transaction.mint,
        redeemer.expected_policy,
        redeemer.expected_asset_name,
      ) == redeemer.expected_mint_quantity,
      withdrawal_present(
        context.transaction.withdrawals,
        redeemer.expected_observer,
      ),
      redeemer_map_has_spend(
        context.transaction.redeemers,
        redeemer.expected_output_reference,
      ),
      redeemer_map_has_mint(
        context.transaction.redeemers,
        redeemer.expected_policy,
      ),
      redeemer_map_has_withdrawal(
        context.transaction.redeemers,
        redeemer.expected_observer,
      ),
      builtin.length_of_bytearray(context.transaction.id) == 32,
    }
  }
}

validator payout_spend {
  else(_) {
    trace @"Midgard Demo – Payout Spend"
    True
  }
}

validator payout_mint {
  else(_) {
    trace @"Midgard Demo – Payout Mint"
    True
  }
}

validator withdrawal_spend {
  else(_) {
    trace @"Midgard Demo – Withdrawal Spend"
    True
  }
}

validator withdrawal_mint {
  else(_) {
    trace @"Midgard Demo – Withdrawal Mint"
    True
  }
}

validator tx_order_spend {
  else(_) {
    trace @"Midgard Demo – Tx Order Spend"
    True
  }
}

validator tx_order_mint {
  else(_) {
    trace @"Midgard Demo – Tx Order Mint"
    True
  }
}

validator settlement_spend {
  else(_) {
    trace @"Midgard Demo – Settlement Spend"
    True
  }
}

validator settlement_mint {
  else(_) {
    trace @"Midgard Demo – Settlement Mint"
    True
  }
}
