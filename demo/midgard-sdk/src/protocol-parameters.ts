import { Network } from "@lucid-evolution/lucid";

/**
 * Off-chain projection of the protocol parameters currently needed by the SDK.
 */
export type ProtocolParameters = {
  event_wait_duration: number;
};

// This must stay aligned with the compiled on-chain environment in
// `onchain/aiken/env/default.ak`. We do not currently compile per-network
// variants of that module, so the off-chain protocol view must not diverge by
// network.
const ONCHAIN_EVENT_WAIT_DURATION_MS = 60_000;
// This must stay aligned with `onchain/aiken/lib/midgard/protocol-parameters.ak`.
export const ONCHAIN_SHIFT_DURATION_MS = 300_000;
// This must stay aligned with `onchain/aiken/validators/scheduler.ak`.
export const SCHEDULER_TRANSITION_MAX_VALIDITY_WINDOW_MS = 10 * 60 * 1000;

/**
 * Returns the protocol parameters expected by the SDK for the selected
 * network.
 *
 * The current implementation is network-invariant by design because the
 * corresponding on-chain environment is compiled once and shared across
 * networks.
 */
export const getProtocolParameters = (
  _network: Network,
): ProtocolParameters => ({
  event_wait_duration: ONCHAIN_EVENT_WAIT_DURATION_MS,
});

/**
 * Computes the last millisecond at which an event may be included given the
 * transaction validity upper bound.
 */
export const resolveEventInclusionTime = (
  validTo: number,
  network: Network,
): number => validTo + getProtocolParameters(network).event_wait_duration - 1;
