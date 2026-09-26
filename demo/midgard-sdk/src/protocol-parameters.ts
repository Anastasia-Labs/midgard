import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { Network } from "@lucid-evolution/lucid";

import { PosixTimeDuration } from "./common.js";

export const SHIFT_DURATION_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.operator_shift_ms,
);
export const REGISTRATION_DURATION_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.registration_ms,
);
export const MATURITY_DURATION_MS = BigInt(
  MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
);
export const USER_EVENTS_NEGLIGENCE_TIMEOUT_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.user_events_negligence_timeout_ms,
);
export const MAX_INACTIVITY_BETWEEN_BLOCK_COMMITMENTS_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing
    .max_inactivity_between_block_commitments_ms,
);
export const NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.new_shift_inactivity_grace_period_ms,
);
export const MAX_VALIDITY_RANGE_LENGTH_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.max_validity_range_ms,
);
export const MAX_INACTIVITY_STRIKES = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.max_inactivity_strikes,
);
export const EVENT_WAIT_DURATION_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.event_wait_ms;

//TODO: change event_wait_duration to POSIXTime or maturity_duration to number for better consistency
export type ProtocolParameters = {
  event_wait_duration: number;
  maturity_duration: PosixTimeDuration;
  slashing_penalty: bigint;
  fraud_prover_reward: bigint;
  required_bond: bigint;
  inactivity_slashing_penalty: bigint;
};

export const getProtocolParameters = (network: Network): ProtocolParameters => {
  if (network !== SELECTED_DEPLOYMENT_PROFILE.network) {
    throw new Error("Network must match the compiled deployment profile");
  }
  const economics = SELECTED_DEPLOYMENT_PROFILE.economics;
  return {
    event_wait_duration: EVENT_WAIT_DURATION_MS,
    maturity_duration: MATURITY_DURATION_MS,
    slashing_penalty: BigInt(economics.slashingPenaltyLovelace),
    fraud_prover_reward: BigInt(economics.fraudProverRewardLovelace),
    required_bond: BigInt(economics.requiredBondLovelace),
    inactivity_slashing_penalty: BigInt(
      economics.inactivitySlashingPenaltyLovelace,
    ),
  };
};
