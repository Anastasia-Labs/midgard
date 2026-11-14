use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/validation_asset_fold.{unique_asset_fold_dispatch}
use midgard/validation_machine/value_and_mint.{verify_asset_fold_claim}

validator main(
  replay_asset_dispatcher_script_hash: ScriptHash,
  output_asset_dispatcher_script_hash: ScriptHash,
  mint_asset_dispatcher_script_hash: ScriptHash,
) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let (dispatcher, claim) = unique_asset_fold_dispatch(
      replay_asset_dispatcher_script_hash,
      output_asset_dispatcher_script_hash,
      mint_asset_dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    and {
      when claim.descriptor is {
        None -> dispatcher == mint_asset_dispatcher_script_hash
        Some(_) -> dispatcher != mint_asset_dispatcher_script_hash
      },
      verify_asset_fold_claim(
        claim,
        dispatcher == output_asset_dispatcher_script_hash,
      ),
    }
  }

  else(_) {
    fail
  }
}
