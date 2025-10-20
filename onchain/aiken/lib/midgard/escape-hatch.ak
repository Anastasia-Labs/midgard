use midgard/common/types.{PosixTime}

pub const escape_hatch_asset_name = "EscapeHatch"

pub type Datum {
  emergency_end_time: PosixTime,
}

pub type MintRedeemer {
  Init
  Deinit
}

pub type SpendRedeemer {
  TriggerEscapeHatch {
    escape_hatch_output_index: Int,
    hub_oracle_ref_input_index: Int,
  }
}
