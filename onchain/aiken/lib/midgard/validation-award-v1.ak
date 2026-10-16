use midgard/computation_thread as ct
use midgard/validation_resolution_v1

/// A thread at the award validator: the resolution certified the challenger's
/// step.
pub type Datum =
  ct.StepDatum<validation_resolution_v1.WinningValidationResolutionStateV1>

pub type SpendRedeemer {
  /// Closes the awarded thread into its case as the challenger's win.
  CloseAwardedThread { input_index: Int, case_input_index: Int }
}

/// The award validator's mint purpose: the lost-defense receipt policy.
pub type ReceiptRedeemerV1 {
  /// Mints one receipt beside the case input at `case_input_index`, whose
  /// `ThreadClose` fixes the transaction's mint exactly.
  MintReceipt { case_input_index: Int }
}
