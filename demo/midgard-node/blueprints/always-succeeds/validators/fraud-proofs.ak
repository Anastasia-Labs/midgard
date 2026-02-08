validator double_spend {
  else(_) {
    trace @"Midgard Demo – Double Spend Fraud Proof"
    True
  }
}

validator non_existent_input {
  else(_) {
    trace @"Midgard Demo – Non-Existent Input Fraud Proof"
    True
  }
}

validator non_existent_input_no_index {
  else(_) {
    trace @"Midgard Demo – Non-Existent Input No Index Fraud Proof"
    True
  }
}

validator invalid_range {
  else(_) {
    trace @"Midgard Demo – Invalid Range Fraud Proof"
    True
  }
}
