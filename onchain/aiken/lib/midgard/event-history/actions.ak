pub type Operation {
  InsertFiller {
    predecessor_input_index: Int,
    predecessor_output_index: Int,
    filler_output_index: Int,
  }
  InsertOrder {
    predecessor_input_index: Int,
    predecessor_output_index: Int,
    order_output_index: Int,
    nonce_input_index: Int,
    external_reference_index: Option<Int>,
  }
  PromoteFiller {
    filler_input_index: Int,
    order_output_index: Int,
    refund_output_index: Int,
    nonce_input_index: Int,
    external_reference_index: Option<Int>,
  }
  ReclaimFiller {
    predecessor_input_index: Int,
    filler_input_index: Int,
    predecessor_output_index: Int,
    refund_output_index: Int,
  }
  RetireOrder {
    predecessor_output_index: Int,
    funds_output_index: Int,
    structural_refund_output_index: Option<Int>,
  }
}

pub type Observe {
  Initialize { nonce_input_index: Int, root_output_index: Int }
  Apply { hub_reference_index: Int, operation: Operation }
}
