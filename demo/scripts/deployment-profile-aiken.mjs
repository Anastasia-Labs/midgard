/** The profile fields compiled into Aiken and their exact rendering. */
export const timingConstants = {
  block_maturity_ms: "block_maturity_duration_v1",
  dispute_response_window_ms: "response_window_milliseconds",
  operator_shift_ms: "shift_duration",
  registration_ms: "registration_duration",
  event_wait_ms: "event_wait_duration",
  user_events_negligence_timeout_ms: "user_events_negligence_timeout",
  new_shift_inactivity_grace_period_ms: "new_shift_inactivity_grace_period",
  max_validity_range_ms: "max_validity_range_length",
  max_event_validity_range_ms: "max_event_validity_range_length",
  da_attestation_timeout_ms: "da_attestation_timeout_v1",
  da_small_response_window_ms: "small_response_window_ms_v1",
  da_full_response_window_ms: "full_response_window_ms_v1",
  da_challenge_window_ms: "da_challenge_window_ms_v1",
  da_slash_grace_ms: "da_slash_grace_ms_v1",
  da_bond_withdraw_delay_ms: "da_bond_withdraw_delay_ms_v1",
};
// The pooled DA committee bond (#685). The availability ParametersV1 carries
// these amounts on-chain; off-chain consumers must match the selected profile.
export const daBondConstants = {
  da_bond_lovelace: "da_bond_lovelace_v1",
  da_slash_penalty_lovelace: "da_slash_penalty_lovelace_v1",
  da_bond_min_top_up_lovelace: "da_bond_min_top_up_lovelace_v1",
  da_bond_pool_floor_lovelace: "da_bond_pool_floor_lovelace_v1",
  challenge_record_lovelace: "challenge_record_lovelace_v1",
};
export const economicsConstants = {
  requiredBondLovelace: "required_bond",
  slashingPenaltyLovelace: "slashing_penalty",
  inactivitySlashingPenaltyLovelace: "inactivity_slashing_penalty",
  fraudProverRewardLovelace: "fraud_prover_reward",
  proverCollateralFloorLovelace: "prover_collateral_floor",
};
export const limitConstants = {
  max_bisection_rounds: "max_bisection_rounds",
  max_inactivity_strikes: "max_inactivity_strikes",
  coins_per_utxo_byte: "coins_per_utxo_byte",
};
// Stage-0 allow-lists (P1). Each is null (every key admitted) or the
// verification key hashes the deployment admits, fixed for its life. They
// render as `Option<List<ByteArray>>` env constants checked by
// `midgard/common/allow_list.admits`.
export const allowListConstants = {
  operators: "operator_allow_list",
  fraud_provers: "fraud_prover_allow_list",
  da_challengers: "da_challenger_allow_list",
};

export const renderAiken = (profile, template) => {
  const constants = [
    [profile.timing, timingConstants],
    [profile.da_bond, daBondConstants],
    [profile.limits, limitConstants],
    [profile.economics, economicsConstants],
  ].flatMap(([values, names]) =>
    Object.entries(names).map(
      ([key, name]) =>
        `pub const ${name}: Int = ${values[key].toLocaleString("en-US").replaceAll(",", "_")}`,
    ),
  );
  const allowLists = Object.entries(allowListConstants).map(([key, name]) => {
    const keys = profile.allow_lists[key];
    // The layout `aiken fmt` gives each form, so the generated env stays
    // format-clean.
    const head = `pub const ${name}: Option<List<ByteArray>> =`;
    if (keys === null) return `${head} None`;
    if (keys.length === 1) return `${head}\n  Some([#"${keys[0]}"])`;
    return `${head}\n  Some(\n    [\n${keys.map((entry) => `      #"${entry}",`).join("\n")}\n    ],\n  )`;
  });
  return `${template.trimEnd()}\n\n${[...constants, ...allowLists].join("\n\n")}\n`;
};
