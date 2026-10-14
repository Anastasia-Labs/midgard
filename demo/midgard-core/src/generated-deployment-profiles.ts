export const DEPLOYMENT_PROFILES = {
  mainnet: {
    name: "mainnet",
    network: "Mainnet",
    l1_finality: {
      confirmation_depth: 30,
      commit_event_depth: 2160,
      security_parameter: 2160,
      active_slot_coeff: "0.05",
      slot_length_ms: 1000,
    },
    timing: {
      block_maturity_ms: 604800000,
      dispute_response_window_ms: 300000,
      operator_shift_ms: 3600000,
      registration_ms: 30,
      event_wait_ms: 130080000,
      user_events_negligence_timeout_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 480000,
      max_validity_range_ms: 480000,
      max_event_validity_range_ms: 600000,
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
      max_dispute_move_fee_lovelace: 1200000,
      fraud_proof_output_lovelace: 2000000,
    },
    economics: {
      profile: "public-preprod-launch-v1",
      requiredBondLovelace: 100000000000,
      slashingPenaltyLovelace: 25000000000,
      inactivitySlashingPenaltyLovelace: 10000000000,
      fraudProverRewardLovelace: 75000000000,
      proverCollateralFloorLovelace: 5000000,
    },
    allow_lists: {
      operators: ["706c616365686f6c6465723a6f706572000000000000000000000001"],
      fraud_provers: [
        "706c616365686f6c6465723a70726f76000000000000000000000001",
      ],
      da_challengers: [
        "706c616365686f6c6465723a64616368000000000000000000000001",
      ],
    },
    excluded_fraud_proof_categories: [
      "validationTraceDispute",
      "executionSourceScriptDecoding",
      "receivePurposeLanguage",
      "unusedScriptWitness",
      "missingScriptSource",
      "missingRedeemer",
      "unusedRedeemer",
      "executionNativeScriptInvalid",
      "scriptIntegrityHashMismatch",
      "distinctAssetAccumulationLimit",
    ],
  },
  "preprod-public": {
    name: "preprod-public",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 30,
      commit_event_depth: 22,
      security_parameter: 2160,
      active_slot_coeff: "0.05",
      slot_length_ms: 1000,
    },
    timing: {
      block_maturity_ms: 604800000,
      dispute_response_window_ms: 300000,
      operator_shift_ms: 3600000,
      registration_ms: 30000,
      event_wait_ms: 1800000,
      user_events_negligence_timeout_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 480000,
      max_validity_range_ms: 480000,
      max_event_validity_range_ms: 600000,
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
      max_inactivity_strikes: 1000000,
      coins_per_utxo_byte: 4310,
      max_dispute_move_fee_lovelace: 1200000,
      fraud_proof_output_lovelace: 2000000,
    },
    economics: {
      profile: "public-preprod-launch-v1",
      requiredBondLovelace: 100000000000,
      slashingPenaltyLovelace: 25000000000,
      inactivitySlashingPenaltyLovelace: 10000000000,
      fraudProverRewardLovelace: 75000000000,
      proverCollateralFloorLovelace: 5000000,
    },
    allow_lists: {
      operators: [
        "041388eef29da389e7b9dac19509ef8ac95a398a9909991a4326d424",
        "4189f341bddd0c74ecb4f5a1ddae8653ee204be022fe530e2f65a6a6",
      ],
      fraud_provers: [
        "3fce32946bbc65915c9abecaac07b0b00b0ee85fce73d06a209e3e5f",
      ],
      da_challengers: [
        "c3276ebd1e6886c2bdc4ef2ca4d084ed8da3901b6054d3c881eb1e81",
      ],
    },
    excluded_fraud_proof_categories: [
      "validationTraceDispute",
      "executionSourceScriptDecoding",
      "receivePurposeLanguage",
      "unusedScriptWitness",
      "missingScriptSource",
      "missingRedeemer",
      "unusedRedeemer",
      "executionNativeScriptInvalid",
      "scriptIntegrityHashMismatch",
      "distinctAssetAccumulationLimit",
    ],
  },
  "preprod-testing": {
    name: "preprod-testing",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 10,
      commit_event_depth: 3,
      security_parameter: 2160,
      active_slot_coeff: "0.05",
      slot_length_ms: 1000,
    },
    timing: {
      block_maturity_ms: 900000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 300000,
      user_events_negligence_timeout_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 480000,
      max_validity_range_ms: 480000,
      max_event_validity_range_ms: 600000,
      da_attestation_timeout_ms: 600000,
      da_small_response_window_ms: 880000,
      da_full_response_window_ms: 880000,
      da_challenge_window_ms: 720000,
      da_slash_grace_ms: 300000,
      da_bond_withdraw_delay_ms: 2380000,
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
      max_dispute_move_fee_lovelace: 1200000,
      fraud_proof_output_lovelace: 2000000,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
    allow_lists: {
      operators: null,
      fraud_provers: null,
      da_challengers: null,
    },
    excluded_fraud_proof_categories: [],
  },
  "local-devnet-testing": {
    name: "local-devnet-testing",
    network: "Custom",
    l1_finality: {
      confirmation_depth: 10,
      commit_event_depth: 3,
      security_parameter: 2160,
      active_slot_coeff: "0.05",
      slot_length_ms: 1000,
    },
    timing: {
      block_maturity_ms: 900000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 300000,
      user_events_negligence_timeout_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 480000,
      max_validity_range_ms: 480000,
      max_event_validity_range_ms: 600000,
      da_attestation_timeout_ms: 600000,
      da_small_response_window_ms: 880000,
      da_full_response_window_ms: 880000,
      da_challenge_window_ms: 720000,
      da_slash_grace_ms: 300000,
      da_bond_withdraw_delay_ms: 2380000,
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
      max_dispute_move_fee_lovelace: 1200000,
      fraud_proof_output_lovelace: 2000000,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
    allow_lists: {
      operators: null,
      fraud_provers: null,
      da_challengers: null,
    },
    excluded_fraud_proof_categories: [],
  },
  "preprod-emulator-testing": {
    name: "preprod-emulator-testing",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 3,
      commit_event_depth: 8,
      security_parameter: 2160,
      active_slot_coeff: "0.05",
      slot_length_ms: 1000,
    },
    timing: {
      block_maturity_ms: 14400000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 600000,
      user_events_negligence_timeout_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 480000,
      max_validity_range_ms: 480000,
      max_event_validity_range_ms: 600000,
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
      max_dispute_move_fee_lovelace: 1200000,
      fraud_proof_output_lovelace: 2000000,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
    allow_lists: {
      operators: null,
      fraud_provers: null,
      da_challengers: null,
    },
    excluded_fraud_proof_categories: [],
  },
  "preprod-emulator-curated": {
    name: "preprod-emulator-curated",
    network: "Preprod",
    l1_finality: {
      confirmation_depth: 3,
      commit_event_depth: 8,
      security_parameter: 2160,
      active_slot_coeff: "0.05",
      slot_length_ms: 1000,
    },
    timing: {
      block_maturity_ms: 14400000,
      dispute_response_window_ms: 60000,
      operator_shift_ms: 1800000,
      registration_ms: 30000,
      event_wait_ms: 600000,
      user_events_negligence_timeout_ms: 1200000,
      new_shift_inactivity_grace_period_ms: 480000,
      max_validity_range_ms: 480000,
      max_event_validity_range_ms: 600000,
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
      max_dispute_move_fee_lovelace: 1200000,
      fraud_proof_output_lovelace: 2000000,
    },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900000000,
      slashingPenaltyLovelace: 500000000,
      inactivitySlashingPenaltyLovelace: 100000000,
      fraudProverRewardLovelace: 400000000,
      proverCollateralFloorLovelace: 5000000,
    },
    allow_lists: {
      operators: ["8372d26e23e36568f931b8e3a418ca29dc545054f017548733c78198"],
      fraud_provers: [
        "d429435c9f332ef50d6fc8f5694bd256a36740da3d21e83aade0594f",
      ],
      da_challengers: [
        "3bd3eb08b8fb0166ddaa1cbe99248f88d6a749387740a7e6e1548d9b",
      ],
    },
    excluded_fraud_proof_categories: [
      "validationTraceDispute",
      "executionSourceScriptDecoding",
      "receivePurposeLanguage",
      "unusedScriptWitness",
      "missingScriptSource",
      "missingRedeemer",
      "unusedRedeemer",
      "executionNativeScriptInvalid",
      "scriptIntegrityHashMismatch",
      "distinctAssetAccumulationLimit",
    ],
  },
} as const;

export const DEPLOYMENT_PROFILE_DIGESTS = {
  mainnet: "2da189a14fc8a9cef249d3823b1e3fd097611d25a567d35270381e3888f25496",
  "preprod-public":
    "9f6ebbc5abe49911c777eadff39a90dace1574b682b8d50a5bfc084c0f4a6ce5",
  "preprod-testing":
    "0265a09ca7bc54e70c52c11c3afc3d16e5b8485a6232b5f161db064842752f09",
  "local-devnet-testing":
    "694ad45334152fce9bd5a80030e34b4f9151d14fc5a7971884226df336f59315",
  "preprod-emulator-testing":
    "ffc1f052d02729e26e7c361bd43b44fded404fa94e6a63260648eb9c507252cc",
  "preprod-emulator-curated":
    "0e58cceae07da737c0ae6a8ed19deee158270e486c955ecedb4b454764eff74d",
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

/** The least span from planning a commit to its TTL (`deployment-profiles.mjs`). */
export const COMMIT_TTL_FUTURE_BUFFER_MS = 30000;

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
  for (const keys of Object.values(profile.allow_lists))
    if (keys !== null) Object.freeze(keys);
  Object.freeze(profile.allow_lists);
  Object.freeze(profile.excluded_fraud_proof_categories);
  Object.freeze(profile);
}
Object.freeze(DEPLOYMENT_PROFILES);
Object.freeze(DEPLOYMENT_PROFILE_DIGESTS);
for (const economics of Object.values(DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE))
  Object.freeze(economics);
Object.freeze(DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE);
