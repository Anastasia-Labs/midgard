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
  mainnet: "8228a2d0552be07669710ff433b980772ae1506cf2bb3c7160782a6a59366912",
  "preprod-public":
    "8bd688e6ca826e9ccbdf290a6df6fa0da2badd462d6f3b9ed601ee7938bda0f5",
  "preprod-testing":
    "30d117db4ca0852fc22f80a4c687b3c118e5eeb6eb3cf360f02c2e2d13ad2b70",
  "local-devnet-testing":
    "15441a106087154f16c6052eb984260fe4bbb2c9b168fa7f15e2ab17082eda29",
  "preprod-emulator-testing":
    "4a3edd7153f17f26fc115b61a63721848ed6979e92a16e96789fabf629192789",
  "preprod-emulator-curated":
    "e4b118c838a4b968f6b77a1a71fd30c63b4489852d83ca5779ff0bd96ade6eb9",
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

/** `user_events_witness_script_prefix`, the same in every profile's Aiken env. */
export const USER_EVENT_WITNESS_SCRIPT_PREFIX =
  "5902e20101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc3a400200514a0809901319199119801001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c966002003168992cc004cdd78008044566002601460286ea8006264b3001300c3015375400315980099baf3018301637540046030602c6ea800629462c80a22c80a0c05c00a2c809a2c8098c058009015180b000998019bac3001301237540146eb4c050c048dd50064566002601060226ea8016264b30010018b44c966002601260266ea80062b30013375e602c60286ea8004c058c050dd5003c528c59012459012180a800a028330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0240020028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c1225820";

/** The delegated verifier script hashes pinned in every profile's Aiken env. */
export const VERIFIER_SCRIPT_HASHES = Object.freeze({
  plutarch_phas_validator_hash:
    "295ef8e61e32fca2d4e99da4c663ee2d5188ec94e1b907713d61e4ed",
  plutarch_pexcludes_validator_hash:
    "2dcb7892575cda83abd101588d7da0d7b15bbde00e601e610a6c0e5a",
  mpf_chunked_verify_validator_hash:
    "6d5607e377782585db39b7c62f3a5dfbab2ad2b2c060bd5627bf3a5c",
} as const);

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
