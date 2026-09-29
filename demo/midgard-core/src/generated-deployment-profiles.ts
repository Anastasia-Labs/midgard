export const DEPLOYMENT_PROFILES = {
  mainnet: {
    name: "mainnet",
    network: "Mainnet",
    l1_finality: {
      confirmation_depth: 30,
    },
    timing: {
      block_maturity_ms: 604800000,
      dispute_response_window_ms: 300000,
      operator_shift_ms: 3600000,
      registration_ms: 30,
      event_wait_ms: 129600000,
      user_events_negligence_timeout_ms: 1200000,
      max_inactivity_between_block_commitments_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 300000,
      max_validity_range_ms: 480000,
      da_attestation_timeout_ms: 3600000,
      da_small_response_window_ms: 3600000,
      da_full_response_window_ms: 172800000,
      da_challenge_window_ms: 259200000,
      da_slash_grace_ms: 172800000,
      da_bond_withdraw_delay_ms: 778080000,
    },
    da_bond: {
      da_bond_lovelace: 100000000000,
      da_slash_penalty_lovelace: 25000000000,
      da_bond_min_top_up_lovelace: 1000000000,
      da_bond_pool_floor_lovelace: 5000000,
      challenge_record_lovelace: 27000000,
    },
    limits: {
      max_bisection_rounds: 32,
      max_inactivity_strikes: 5,
      coins_per_utxo_byte: 4310,
    },
    economics: {
      profile: "public-preprod-launch-v1",
      requiredBondLovelace: 100000000000,
      slashingPenaltyLovelace: 25000000000,
      inactivitySlashingPenaltyLovelace: 10000000000,
      fraudProverRewardLovelace: 75000000000,
      proverCollateralFloorLovelace: 5000000,
    },
  },
  "preprod-public": {
    name: "preprod-public",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 30,
    },
    timing: {
      block_maturity_ms: 604800000,
      dispute_response_window_ms: 300000,
      operator_shift_ms: 3600000,
      registration_ms: 30,
      event_wait_ms: 1800000,
      user_events_negligence_timeout_ms: 1200000,
      max_inactivity_between_block_commitments_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 300000,
      max_validity_range_ms: 480000,
      da_attestation_timeout_ms: 3600000,
      da_small_response_window_ms: 3600000,
      da_full_response_window_ms: 172800000,
      da_challenge_window_ms: 259200000,
      da_slash_grace_ms: 172800000,
      da_bond_withdraw_delay_ms: 778080000,
    },
    da_bond: {
      da_bond_lovelace: 100000000000,
      da_slash_penalty_lovelace: 25000000000,
      da_bond_min_top_up_lovelace: 1000000000,
      da_bond_pool_floor_lovelace: 5000000,
      challenge_record_lovelace: 27000000,
    },
    limits: {
      max_bisection_rounds: 32,
      max_inactivity_strikes: 5,
      coins_per_utxo_byte: 4310,
    },
    economics: {
      profile: "public-preprod-launch-v1",
      requiredBondLovelace: 100000000000,
      slashingPenaltyLovelace: 25000000000,
      inactivitySlashingPenaltyLovelace: 10000000000,
      fraudProverRewardLovelace: 75000000000,
      proverCollateralFloorLovelace: 5000000,
    },
  },
  "preprod-testing": {
    name: "preprod-testing",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 3,
    },
    timing: {
      block_maturity_ms: 900000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 300000,
      user_events_negligence_timeout_ms: 1200000,
      max_inactivity_between_block_commitments_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 300000,
      max_validity_range_ms: 480000,
      da_attestation_timeout_ms: 600000,
      da_small_response_window_ms: 720000,
      da_full_response_window_ms: 840000,
      da_challenge_window_ms: 720000,
      da_slash_grace_ms: 300000,
      da_bond_withdraw_delay_ms: 2340000,
    },
    da_bond: {
      da_bond_lovelace: 500000000,
      da_slash_penalty_lovelace: 100000000,
      da_bond_min_top_up_lovelace: 5000000,
      da_bond_pool_floor_lovelace: 5000000,
      challenge_record_lovelace: 27000000,
    },
    limits: {
      max_bisection_rounds: 32,
      max_inactivity_strikes: 5,
      coins_per_utxo_byte: 4310,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
  },
  "local-devnet-testing": {
    name: "local-devnet-testing",
    network: "Custom",
    l1_finality: {
      confirmation_depth: 3,
    },
    timing: {
      block_maturity_ms: 900000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 300000,
      user_events_negligence_timeout_ms: 1200000,
      max_inactivity_between_block_commitments_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 300000,
      max_validity_range_ms: 480000,
      da_attestation_timeout_ms: 600000,
      da_small_response_window_ms: 720000,
      da_full_response_window_ms: 840000,
      da_challenge_window_ms: 720000,
      da_slash_grace_ms: 300000,
      da_bond_withdraw_delay_ms: 2340000,
    },
    da_bond: {
      da_bond_lovelace: 500000000,
      da_slash_penalty_lovelace: 100000000,
      da_bond_min_top_up_lovelace: 5000000,
      da_bond_pool_floor_lovelace: 5000000,
      challenge_record_lovelace: 27000000,
    },
    limits: {
      max_bisection_rounds: 32,
      max_inactivity_strikes: 5,
      coins_per_utxo_byte: 4310,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
  },
  "preprod-emulator-testing": {
    name: "preprod-emulator-testing",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 3,
    },
    timing: {
      block_maturity_ms: 14400000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 600000,
      user_events_negligence_timeout_ms: 1200000,
      max_inactivity_between_block_commitments_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 300000,
      max_validity_range_ms: 480000,
      da_attestation_timeout_ms: 600000,
      da_small_response_window_ms: 720000,
      da_full_response_window_ms: 840000,
      da_challenge_window_ms: 1800000,
      da_slash_grace_ms: 300000,
      da_bond_withdraw_delay_ms: 15180000,
    },
    da_bond: {
      da_bond_lovelace: 500000000,
      da_slash_penalty_lovelace: 100000000,
      da_bond_min_top_up_lovelace: 5000000,
      da_bond_pool_floor_lovelace: 5000000,
      challenge_record_lovelace: 27000000,
    },
    limits: {
      max_bisection_rounds: 32,
      max_inactivity_strikes: 5,
      coins_per_utxo_byte: 4310,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
  },
} as const;

export const DEPLOYMENT_PROFILE_DIGESTS = {
  mainnet: "49eff45a76fa11fdc52b04eb32ba39176137b33bf2ca5666ae0d3f7a7b60f11d",
  "preprod-public":
    "b2660905760a27e23fdcccc7adb25a7991a1cf16caa806283fa29621622b3ea2",
  "preprod-testing":
    "6ac7c84ebbcf1dee6659eb5028617b6a565acc70b917e138563f6e95c2dd4cda",
  "local-devnet-testing":
    "d830215686d4a23d94dceb93a64e469076efb1f9abc419fa4231eef7575a57a0",
  "preprod-emulator-testing":
    "482c50df9e885112e61c445def8f410db4770ea7683fcea86353b067182a50f3",
} as const;

export const DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE = {
  "public-preprod-launch-v1": {
    profile: "public-preprod-launch-v1",
    requiredBondLovelace: 100000000000,
    slashingPenaltyLovelace: 25000000000,
    inactivitySlashingPenaltyLovelace: 10000000000,
    fraudProverRewardLovelace: 75000000000,
    proverCollateralFloorLovelace: 5000000,
  },
  "bounded-acceptance-v1": {
    profile: "bounded-acceptance-v1",
    requiredBondLovelace: 900000000,
    slashingPenaltyLovelace: 500000000,
    inactivitySlashingPenaltyLovelace: 100000000,
    fraudProverRewardLovelace: 400000000,
    proverCollateralFloorLovelace: 5000000,
  },
} as const;

export const SELECTED_DEPLOYMENT_PROFILE =
  DEPLOYMENT_PROFILES["preprod-testing"];
export const SELECTED_DEPLOYMENT_PROFILE_DIGEST =
  DEPLOYMENT_PROFILE_DIGESTS["preprod-testing"];
for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
  Object.freeze(profile.l1_finality);
  Object.freeze(profile.timing);
  Object.freeze(profile.da_bond);
  Object.freeze(profile.limits);
  Object.freeze(profile.economics);
  Object.freeze(profile);
}
Object.freeze(DEPLOYMENT_PROFILES);
Object.freeze(DEPLOYMENT_PROFILE_DIGESTS);
for (const economics of Object.values(DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE))
  Object.freeze(economics);
Object.freeze(DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE);
