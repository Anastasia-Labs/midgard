pub type Datum {
  init_step_script_hash: ByteArray,
}

pub type MintRedeemer {
  Init
  Deinit
  NewFraudCategory {
    new_node_key: ByteArray,
    anchor_node_input_index: Int,
    appended_node_output_index: Int,
    anchor_node_output_index: Int,
  }
  RemoveFraudCategory {
    node_to_remove_key: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
}

pub type SpendRedeemer {
  fraud_proof_catalogue_asset_name: ByteArray,
}
