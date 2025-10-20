use aiken/collection/dict
use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId, Value, flatten, quantity_of, tokens}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}
use midgard/hub_oracle.{Datum, Init, MintRedeemer, asset_name}

validator mint(nonce_utxo: OutputReference) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let Init = redeemer
    // (1) nonce_utxo must be spent
    expect
      list.any(self.inputs, fn(input) { input.output_reference == nonce_utxo })

    // (2) The hub oracle NFT must be minted
    expect quantity_of(self.mint, policy_id, asset_name) == 1

    // (3) Let hub_oracle_output be the transaction output with the hub oracle NFT
    expect Some(hub_oracle_output) =
      list.find(
        self.outputs,
        fn(output) { quantity_of(output.value, policy_id, asset_name) == 1 },
      )

    // (4) hub_oracle_output must not contain any other non-ADA tokens from this policy
    expect dict.size(tokens(hub_oracle_output.value, policy_id)) == 1

    // (5) hub_oracle_output must be sent to the burn everything spending validator address
    // TODO: Verify address once burn_everything_addr is defined
    // expect hub_oracle_output.address == burn_everything_addr
    // (6) Parse the hub oracle datum
    expect InlineDatum(datum_data) = hub_oracle_output.datum
    expect hub_datum: Datum = datum_data

    // (7) Verify all linked lists are initialized
    expect and {
        verify_list_init(
          self,
          hub_datum.registered_operators,
          hub_datum.registered_operators_addr,
        ),
        verify_list_init(
          self,
          hub_datum.active_operators,
          hub_datum.active_operators_addr,
        ),
        verify_list_init(
          self,
          hub_datum.retired_operators,
          hub_datum.retired_operators_addr,
        ),
        verify_list_init(
          self,
          hub_datum.state_queue,
          hub_datum.state_queue_addr,
        ),
        verify_list_init(
          self,
          hub_datum.settlement_queue,
          hub_datum.settlement_queue_addr,
        ),
      }

    // (8) Verify all singletons are initialized
    expect and {
        verify_singleton_init(
          self,
          hub_datum.scheduler,
          hub_datum.scheduler_addr,
          "Scheduler",
        ),
        verify_singleton_init(
          self,
          hub_datum.fraud_proof_catalogue,
          hub_datum.fraud_proof_catalogue_addr,
          "FraudProofCatalogue",
        ),
        verify_singleton_init(
          self,
          hub_datum.escape_hatch,
          hub_datum.escape_hatch_addr,
          "EscapeHatch",
        ),
      }

    // (9) Verify exactly 9 NFTs are minted
    let total_minted = count_total_minted(self.mint)
    expect total_minted == 9

    True
  }

  else(_) {
    fail
  }
}

// Helper: Verify a linked list root node NFT is minted and sent to correct address
fn verify_list_init(
  tx: Transaction,
  list_policy_id: PolicyId,
  list_address: Address,
) -> Bool {
  expect quantity_of(tx.mint, list_policy_id, "Node") == 1
  expect Some(_list_output) =
    list.find(
      tx.outputs,
      fn(output) {
        and {
          output.address == list_address,
          quantity_of(output.value, list_policy_id, "Node") == 1,
        }
      },
    )
  True
}

// Helper: Verify a singleton NFT is minted and sent to correct address
fn verify_singleton_init(
  tx: Transaction,
  singleton_policy_id: PolicyId,
  singleton_address: Address,
  singleton_asset_name: ByteArray,
) -> Bool {
  expect quantity_of(tx.mint, singleton_policy_id, singleton_asset_name) == 1
  expect Some(_singleton_output) =
    list.find(
      tx.outputs,
      fn(output) {
        and {
          output.address == singleton_address,
          quantity_of(output.value, singleton_policy_id, singleton_asset_name) == 1,
        }
      },
    )
  True
}

// Helper: Count total number of NFTs minted
fn count_total_minted(mint_value: Value) -> Int {
  flatten(mint_value)
    |> list.foldl(
        0,
        fn(item, acc) {
          let (_, _, quantity) = item
          if quantity > 0 {
            acc + 1
          } else {
            acc
          }
        },
      )
}
