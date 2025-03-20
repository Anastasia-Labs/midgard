use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered.{
  Key, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{InlineDatum, Input, OutputReference, Transaction}
use midgard/fraud_proof/catalogue.{
  Deinit, Init, MintRedeemer, NewFraudCategory, RemoveFraudCategory,
  SpendRedeemer,
}

validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    redeemer: SpendRedeemer,
    _input: OutputReference,
    self: Transaction,
  ) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)
    and {
      quantity_of(
        self.mint,
        fraud_proof_catalog_mint_script_hash,
        redeemer.fraud_proof_catalogue_asset_name,
      ) == -1,
      dict.size(tokens) == 1,
    }
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
  governance_key: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          ordered.init(node_in_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          ordered.deinit(node_outputs, node_mint, policy_id),
        }
      NewFraudCategory {
        new_node_key,
        anchor_node_input_index,
        appended_node_output_index,
        anchor_node_output_index,
      } -> {
        expect Some(old_node_input) =
          list.at(self.inputs, anchor_node_input_index)
        expect Key(old_node_key) =
          ordered.get_key(ordered.get_raw_key_and_link(old_node_input.output))
        let new_node_key_as_int = bytearray.to_int_big_endian(new_node_key)
        let old_node_key_as_int = bytearray.to_int_big_endian(old_node_key)

        expect Some(appended_node_output) =
          list.at(self.outputs, appended_node_output_index)
        expect InlineDatum(appended_node_output_datum) =
          appended_node_output.datum
        expect catalogue.Datum(_) = appended_node_output_datum

        and {
          ordered.append(
            Key(new_node_key),
            appended_node_output_index,
            anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          list.has(self.extra_signatories, governance_key),
          bytearray.length(new_node_key) == 4,
          if ordered.prove_is_root_node(policy_id, old_node_input.output) {
            new_node_key_as_int == 0
          } else {
            and {
              old_node_key_as_int < 4095,
              new_node_key_as_int == old_node_key_as_int + 1,
            }
          },
        }
      }
      RemoveFraudCategory {
        node_to_remove_key,
        removed_node_input_index,
        anchor_node_input_index,
      } -> and {
          ordered.remove(
            Key(node_to_remove_key),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          list.has(self.extra_signatories, governance_key),
        }
    }
  }

  else(_) {
    fail
  }
}
