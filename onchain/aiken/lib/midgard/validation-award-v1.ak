use midgard/computation_thread as ct
use midgard/operator_directory.{SlashingDirectory}
use midgard/validation_resolution_v1

/// A thread at the award validator: the resolution certified the challenger's
/// step.
pub type Datum =
  ct.StepDatum<validation_resolution_v1.WinningValidationResolutionStateV1>

/// The award validator holds two kinds of output: awarded threads (datum
/// `Datum`) and lost-defense receipts (datum
/// `validation_case_v1.LostDefenseReceiptV1`).
pub type SpendRedeemer {
  /// Closes the awarded thread into its case as the challenger's win.
  CloseAwardedThread { input_index: Int, case_input_index: Int }
  /// Spends a receipt whose token is burned in the same transaction.
  SpendReceipt
}

/// The award validator's mint purpose: the lost-defense receipt policy.
pub type ReceiptRedeemerV1 {
  /// Mints one receipt beside the case input at `case_input_index`, whose
  /// `ThreadClose` fixes the transaction's mint exactly.
  MintReceipt { case_input_index: Int }
  /// Burns the one receipt held by the input at `receipt_input_index`. The
  /// operator directories are read from the hub at
  /// `hub_oracle_ref_input_index`.
  BurnReceipt {
    receipt_input_index: Int,
    hub_oracle_ref_input_index: Int,
    permission: ReceiptBurnPermissionV1,
  }
}

/// Why a receipt may be burned.
pub type ReceiptBurnPermissionV1 {
  /// `directory` slashes the receipt's defender in the same transaction with
  /// `SlashOperatorForLostDefense` naming this receipt.
  SlashedForThisLoss { directory: SlashingDirectory }
  /// The receipt's defender is in neither the active nor the retired operator
  /// set (as the state queue's `OperatorAlreadySlashed`): a later loss of an
  /// operator already slashed.
  DefenderAlreadySlashed {
    active_operators_element_ref_input_index: Int,
    retired_operators_element_ref_input_index: Int,
  }
}
