import { Network } from "@lucid-evolution/lucid";

export type ProtocolParameters = {
  event_wait_duration: number;
};

// This must stay aligned with the compiled on-chain environment in
// `onchain/aiken/env/default.ak`. We do not currently compile per-network
// variants of that module, so the off-chain protocol view must not diverge by
// network.
const ONCHAIN_EVENT_WAIT_DURATION_MS = 60_000;
// This must stay aligned with `onchain/aiken/lib/midgard/protocol-parameters.ak`.
export const ONCHAIN_SHIFT_DURATION_MS = 30;
// This must stay aligned with `onchain/aiken/validators/scheduler.ak`.
export const SCHEDULER_TRANSITION_MAX_VALIDITY_WINDOW_MS = 10 * 60 * 1000;

/**
 * Given the network, this functions returns appropriate protocol parameters.
 */
export const getProtocolParameters = (_network: Network): ProtocolParameters => ({
  event_wait_duration: ONCHAIN_EVENT_WAIT_DURATION_MS,
});

export const resolveEventInclusionTime = (
  validTo: number,
  network: Network,
): number => validTo + getProtocolParameters(network).event_wait_duration - 1;
