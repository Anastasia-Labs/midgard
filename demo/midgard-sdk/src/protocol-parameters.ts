import { Network } from "@lucid-evolution/lucid";
import { PosixTimeDuration } from "@/common.js";

export const SHIFT_DURATION_MS = 60n * 60n * 1000n;
export const REGISTRATION_DURATION_MS = 30n;
export const MATURITY_DURATION_MS = 30n;
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
};

/**
 * Given the network, this functions returns appropriate protocol parameters.
 */
export const getProtocolParameters = (network: Network): ProtocolParameters => {
  if (network === "Mainnet") {
    return {
      event_wait_duration: 60_000,
      maturity_duration: 30n,
      slashing_penalty: 2000000n,
    };
  } else {
    return {
      event_wait_duration: EVENT_WAIT_DURATION_MS,
      maturity_duration: 1n,
      slashing_penalty: 1000000n,
    };
  }
};
