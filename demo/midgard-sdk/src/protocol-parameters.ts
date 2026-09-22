import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Network } from "@lucid-evolution/lucid";

import { PosixTimeDuration } from "./common.js";

export const SHIFT_DURATION_MS = 60n * 60n * 1000n;
export const REGISTRATION_DURATION_MS = 30n;
export const MATURITY_DURATION_MS = BigInt(
  MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
);
export const USER_EVENTS_NEGLIGENCE_TIMEOUT_MS = 5n * 60n * 1000n;
export const MAX_INACTIVITY_BETWEEN_BLOCK_COMMITMENTS_MS = 10n * 6n * 1000n;
export const NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS = 5n * 60n * 1000n;
export const MAX_VALIDITY_RANGE_LENGTH_MS = 8n * 60n * 1000n;
export const MAX_INACTIVITY_STRIKES = 5n;
export const EVENT_WAIT_DURATION_MS = 60_000;

//TODO: change event_wait_duration to POSIXTime or maturity_duration to number for better consistency
export type ProtocolParameters = {
  event_wait_duration: number;
  maturity_duration: PosixTimeDuration;
  slashing_penalty: bigint;
  fraud_prover_reward: bigint;
  required_bond: bigint;
  inactivity_slashing_penalty: bigint;
};

/**
 * Development/emulator construction defaults only. A Cardano network label
 * cannot identify release economics: public Preprod and bounded acceptance
 * both run on `Preprod`. Production operator, settlement, and fraud-removal
 * builders must consume the parsed deployment-manifest economics block.
 */
export const getProtocolParameters = (network: Network): ProtocolParameters => {
  if (network === "Mainnet") {
    return {
      event_wait_duration: 60_000,
      maturity_duration: MATURITY_DURATION_MS,
      slashing_penalty: 25_000_000_000n,
      fraud_prover_reward: 75_000_000_000n,
      required_bond: 100_000_000_000n,
      inactivity_slashing_penalty: 10_000_000_000n,
    };
  } else {
    return {
      event_wait_duration: EVENT_WAIT_DURATION_MS,
      maturity_duration: MATURITY_DURATION_MS,
      slashing_penalty: 500_000_000n,
      fraud_prover_reward: 400_000_000n,
      required_bond: 900_000_000n,
      inactivity_slashing_penalty: 100_000_000n,
    };
  }
};
