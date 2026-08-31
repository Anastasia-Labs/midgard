use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Mint, Transaction, placeholder}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof.{MintRedeemer}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

/// The spending validator of a fraud proof token never allows it to be spent.
/// Midgard fraud proof tokens last forever.
validator spend {
  else(_) {
    trace @"Midgard - Fraud Proof Spending Validator"
    fail
  }
}

/// Midgard fraud proof tokens last forever.
validator mint(computation_thread_script_hash: ByteArray) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let Transaction { mint, redeemers, .. } = self

    let
        MintRedeemer {
          computation_thread_token_asset_name,
          computation_thread_mint_redeemer_index,
        }
      = redeemer

    // 1. `Success` redeemer of the computation thread's token policy must be
    //    invoked.
    let mint_redeemer_data = utils.get_redeemer_at(
      redeemers: redeemers,
      expected_purpose: Mint(computation_thread_script_hash),
      redeemer_index: computation_thread_mint_redeemer_index,
    )
    expect
        ct.Success {
          burning_token_asset_name: success_redeemer_burning_asset_name,
        }
      = mint_redeemer_data

    // 2. Asset name from the computation thread's redeemer must match the asset
    //    name being minted here.
    expect
      success_redeemer_burning_asset_name == computation_thread_token_asset_name

    // 3. Only the computation thread token must be burned, and fraud proof
    //    token with the same asset name must be minted.
    let expected_mint = assets.merge(
      assets.from_asset(
        computation_thread_script_hash,
        computation_thread_token_asset_name,
        -1,
      ),
      assets.from_asset(policy_id, computation_thread_token_asset_name, 1),
    )
    expect mint == expected_mint

    // Done.
    True
  }

  else(_) {
    fail
  }
}

// ## Q01 terminal token-coupling selectors

fn run_fraud_proof_mint(
  thread_asset_name: ByteArray,
  minted: assets.Value,
  computation_thread_redeemer: Data,
) -> Bool {
  mint.mint(
    fixture.computation_thread_policy_id,
    MintRedeemer {
      computation_thread_token_asset_name: thread_asset_name,
      computation_thread_mint_redeemer_index: 0,
    },
    fixture.fraud_proof_policy_id,
    Transaction {
      ..placeholder,
      mint: minted,
      redeemers: [
        Pair(
          Mint(fixture.computation_thread_policy_id),
          computation_thread_redeemer,
        ),
      ],
    },
  )
}

fn exact_terminal_mint(asset_name: ByteArray) -> assets.Value {
  assets.from_asset(fixture.computation_thread_policy_id, asset_name, -1)
    |> assets.add(fixture.fraud_proof_policy_id, asset_name, 1)
}

fn success_redeemer(asset_name: ByteArray) -> Data {
  let redeemer: Data = ct.Success { burning_token_asset_name: asset_name }
  redeemer
}

/// Terminal positive: the permanent fraud record replaces exactly the thread
/// token with the same asset name under the fraud-proof policy.
test fraud_proof_mint_accepts_exact_success_coupling() {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  run_fraud_proof_mint(
    asset_name,
    exact_terminal_mint(asset_name),
    success_redeemer(asset_name),
  )
}

/// The permanent record cannot be minted without consuming its computation
/// thread token.
test fraud_proof_mint_rejects_missing_thread_burn() fail {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  run_fraud_proof_mint(
    asset_name,
    assets.from_asset(fixture.fraud_proof_policy_id, asset_name, 1),
    success_redeemer(asset_name),
  )
}

/// The Success redeemer, the burned thread, and the permanent token must all
/// identify one exact category/header thread.
test fraud_proof_mint_rejects_mismatched_success_asset_name() fail {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let other_asset_name = #"00000001"
    |> bytearray.concat(
        #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
      )
  run_fraud_proof_mint(
    asset_name,
    exact_terminal_mint(asset_name),
    success_redeemer(other_asset_name),
  )
}

/// Cancellation is exceptional termination, never authority to mint a
/// permanent fraud-proof record.
test fraud_proof_mint_rejects_cancellation_route() fail {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let cancellation: Data = ct.BurnForCancellation {
    burning_token_asset_name: asset_name,
  }
  run_fraud_proof_mint(
    asset_name,
    exact_terminal_mint(asset_name),
    cancellation,
  )
}

/// A terminal transition cannot piggyback any unrelated mint or burn.
test fraud_proof_mint_rejects_extra_mint() fail {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  run_fraud_proof_mint(
    asset_name,
    exact_terminal_mint(asset_name)
      |> assets.add(fixture.computation_thread_policy_id, "extra", 1),
    success_redeemer(asset_name),
  )
}
