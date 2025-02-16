pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue { index_output: Int, computation_thread_redeemer_index: Int }
}
