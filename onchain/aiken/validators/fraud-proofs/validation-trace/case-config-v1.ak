use aiken/collection/dict
use aiken/collection/list
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}
use midgard/validation_case_config_v1.{CaseConfigV1, config_asset_name}

/// The validation-case configuration NFT (`midgard/validation_case_config_v1`).
/// It is minted once, by the transaction that spends `init_utxo`, into an
/// output at this validator's own address carrying a `CaseConfigV1` inline
/// datum. The spend always fails, so the configuration never moves or
/// changes, and the NFT is never burned.
validator main(init_utxo: OutputReference) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    let Transaction { inputs, outputs, mint, .. } = self
    expect [Pair(name, 1)] = dict.to_pairs(assets.tokens(mint, policy_id))
    expect Some(config_output) = list.find(
      outputs,
      fn(output) { assets.quantity_of(output.value, policy_id, name) == 1 },
    )
    expect InlineDatum(config_data) = config_output.datum
    expect _config: CaseConfigV1 = config_data
    and {
      name == config_asset_name,
      list.any(inputs, fn(input) { input.output_reference == init_utxo }),
      config_output.address.payment_credential == Script(policy_id),
    }
  }

  else(_) {
    fail
  }
}
